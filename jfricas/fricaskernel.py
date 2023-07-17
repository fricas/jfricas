# -*- coding: utf-8 -*-
# https://jfricas.readthedocs.io
# https://github.com/fricas/jfricas.

###################################################################
# This file is one part of the FriCAS to Jupyter notebook bridge. It
# implements an IPython kernel such that pressing SHIFT-ENTER in a
# Jupyter notebook cell takes the cell content and sends it via
# webSPAD to a webserver (hunchentoot). Hunchentoot, in turn, is
# supposed to be connected to a FriCAS process (see webspad.lisp).
###################################################################

from ipykernel.kernelbase import Kernel
from subprocess import Popen, run, PIPE, STDOUT
import requests
import json
import os

__version__ = '2.0.0'

###################################################################
# BEGIN user configuration options
###################################################################
shell_timeout = 15 # Timeout for shell commands in secs.
shell_result = None # store last sh result in python
shell_result_fricas = '__shell_result:=["{0}"]' # store sh result in FriCAS

# Templates (TeX)
pretex1 = r"\def\sp{^}\def\sb{_}\def\leqno(#1){}"
pretex2 = r"\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}"
pretex3 = r"\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}"
pretex = pretex1+pretex2+pretex3

###################################################################
# END user config
###################################################################

try:
    import notebook.notebookapp
    static_file_path = notebook.notebookapp.DEFAULT_STATIC_FILES_PATH
except:
    static_file_path = None


# Function to find a free port
def get_free_port():
        import socket
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(("",0))
        s.listen(1)
        port = s.getsockname()[1]
        s.close()
        return port

# Get new free port
htport = str(get_free_port())

# PID of FriCAS+HT
pid = None


class httpSPAD():

    def __init__(self, url = 'http://localhost:{0}/json'.format(htport)):
        # Store parameters
        self.url = url
        self.output = None


    def put(self, code):
        # POST request
        payload = {'code': code}
        r = requests.post(self.url, data=payload)
        data = r.text
        data = data.replace('\r','\\r')
        data = data.replace('\n','\\n')
        self.output = json.loads(data.rstrip('\\n'))
        return(r)


class SPAD(Kernel):
    implementation = 'spad'
    implementation_version = __version__
    language = 'spad'
    language_version = '0.1'
    language_info = {'name': 'spad', 'mimetype': 'text/plain',
                     'file_extension': '.input',}
    banner = "FriCAS Jupyter Kernel"


    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.server = httpSPAD()
        self.fricas_operations = fricas_operations
        self.fricas_constructors = fricas_constructors
        self.fricas_ids = sorted(fricas_operations + fricas_constructors)


    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):

        ok_status = {
            'status': 'ok',
            # The base class increments the execution count
            'execution_count': self.execution_count,
            'payload': [],
            'user_expressions': {},
        }

        # Pre-process input for special command that are not given
        # to FriCAS, but handled otherwise.
        # Available are:
        # ")quit" (tell user to quit via jupyter notebook means)
        # ")pquit" (tell user to quit via jupyter notebook means)
        # ")python" (evaluate a python expression),
        # ")!" (Send the cell content to a shell
        # Note that there are other system commands in FriCAS like
        # )help, )compile, )read.

        #----------------------------------------------------------
        cmd = ')python'
        if code.startswith(cmd):
            # Python code in cell
            self.output = str(eval(code[len(cmd):].lstrip()))
            self.send_response(self.iopub_socket, 'stream',
                               {'name': 'stdout', 'text': self.output})
            return ok_status

        #----------------------------------------------------------
        cmd = ')!'
        if code.startswith(cmd):
            # Shell code in cell
            global shell_result
            cmd = str(code[len(cmd):].lstrip())
            cp = run(cmd, stdout=PIPE, stderr=STDOUT,
                          timeout=shell_timeout, shell=True)
            self.output = cp.stdout.decode()
            self.send_response(self.iopub_socket, 'stream',
                               {'name': 'stdout', 'text': self.output})

            # Store last shell result in global python variable
            shell_result = self.output

            # Store last shell result inside FriCAS (list of lines)
            s = self.output.rstrip('\n').replace('"', '_"')
            s = shell_result_fricas.format(s.replace('\n', '",_\n"'))
            self.server.put(s)

            return ok_status

        #----------------------------------------------------------
        # send code to hunchentoot and get response from FriCAS

        # It would be enough to send just the whole code string,
        # because webSPAD takes care if the code consists of several
        # lines. However, if, for example, the input cell contains the
        # following code lines.
        #   2+3
        #   [1,]
        #   4+5
        # and we send this to FriCAS as one block, then the evaluation
        # stops at the second line and returns an error and output
        # that contains a good evaluation of the first line and a
        # error output for the second line.
        # Unfortunately, all the output comes in one chunk and it is
        # hard to detect what the actuall error message is.
        # Therefore, we split the input into separate input blocks by the
        # following rules
        # (a) All whitespace is remove from the end of a line.
        # (b) A line ending in _ (underscore) makes the next line
        #     belonging to the same block.
        # (c) A line starting with a non-whitespace character begins
        #     a new input block.
        # (d) All lines that begin with a space character are associated
        #     with the current block.
        # Then we handle one block after the other.
        lines = code.split('\n')
        blocks = []
        block = ""
        for line in lines:
            line = line.rstrip()
            if not block:
                block = line
                continue
            l = len(block) # assert(l>0)
            if block[l-1] == '_' or (line and line[0] == ' '):
                block += '\n' + line
            elif block: # The new line begins with non-whitespace
                blocks.append(block)
                block = line
        if block: blocks.append(block)

        # Now we have all the blocks. We take care of a special case,
        # namely if the user wants to quit FriCAS. In that case,
        # return a message that the user should close the respective
        # notebook by means of the Jupyter menu "File -> Close and Halt".
        # We treat this like an error and abort further execution of code
        # following the )quit line.

        quit_msg = 'Use the menu "File -> Close and Halt" to quit the notebook!'

        for block in blocks:
            if block.startswith(')'):
                cmd = block[1:]
                if cmd.startswith('p'): cmd = cmd[1:]
                if cmd in ['q', 'qu', 'qui', 'quit']:
                    self.send_response(self.iopub_socket, 'stream',
                                       {'name': 'stderr', 'text': quit_msg})
                    # We abort evaluation at a ")quit" situation.
                    return ok_status

            r = self.server.put(block)
            # Uncomment the follwing for debugging purposes.
##            self.send_response(self.iopub_socket, 'stream',
##                              {'name': 'stdout', 'text': str(r.text)})

            if r.ok and not silent:
                if not self.handle_fricas_result(): # True=ok, False=error
                    return {'status': 'error',
                            'execution_count': self.execution_count,
                            'payload': [],
                            'user_expressions': {},
                           }

        return ok_status


    def do_shutdown(self, restart):
        "Changes in 5.0: <data> replaced by <text>"
        output = "-- Bye. Kernel shutdown "
        self.send_response(self.iopub_socket, 'stream',
                           {'name': 'stdout', 'text': output})

        try:
            r = self.server.put(")quit")
            pid.terminate() # terminate fricas+HT
        except:
            print("Go down anyway ...")
        return {'restart': False}


    # get_identifier_part is an auxiliary function
    def get_identifier_part(self, code):
        # A FriCAS identifier consists of alpha-numeric characters
        # and one of the letters ! ? _. For code completion and code
        # inspection we ignore other fancy operators, i.e,, to find
        # the start of the identifier, we go simply backwards from the
        # current position until we find a non-admissible character.
        admissible = "abcdefghijklmnopqrstuvwxyz_!?0123456789"
        n = len(code)-1
        while n>=0 and code[n].lower() in admissible: n -= 1
        return code[n+1:]


    def do_complete(self, code, cursor_pos):
        code = code[:cursor_pos]
        token = self.get_identifier_part(code)
        start = cursor_pos - len(token)
        matches = [id for id in self.fricas_ids if id.startswith(token)]
        return {'matches': matches,
                'cursor_start': start,
                'cursor_end': cursor_pos,
                'metadata': dict(),
                'status': 'ok'}


    def do_inspect(self, code, cursor_pos, detail_level=0):
        data = dict()
        code = code[:cursor_pos]
        token = self.get_identifier_part(code)

        if not token:
            return {'status' : 'ok', 'found' : False,
                    'data' :  {}, 'metadata' : {}}

        start = cursor_pos - len(token)
        if token in self.fricas_operations:
            r = self.server.put(')display operation {0}'.format(token))
        elif token in self.fricas_constructors:
            r = self.server.put(')show {0}'.format(token))
        else:
            return {'status' : 'ok', 'found' : False,
                    'data' :  {},'metadata' : {}}

        msg = ['No data from FriCAS']
        if r.ok:
            lines = self.server.output["stdout"].split('\n')
            msg = [l for l in lines if not l.startswith('--FORMAT:')]

        return {'status' : 'ok', 'found' : True,
                'data' : {'text/plain' : '\n'.join(msg)},'metadata' : {}}


    #--------------------------------------------------------------
    # Auxiliary functions
    #--------------------------------------------------------------

    def addLinks(self, s):
        """Every word of the form Aaaa is replaced by
        "<a href=\"" + api + Aaaa + ".html\">"+Aaaa+"</a>".
        """
        import re
        api = r'https://fricas.github.io/api/'
        con = r'([A-Z][A-Za-z0-9]*)'
        sty = 'style="color:blue;text-decoration:none;"'
        repl = r'<a href="'+api+r'\1.html" target="_blank" '+sty+r'>\1</a>'
        return re.sub(con, repl, s)

    def formatTypeTime(self, lines):
        brs = '<br />'
        fTT = '<div style="text-align:right;"><sub>{0}</sub></div>'
        return fTT.format(brs.join(lines))

    # Since we set $printStorageIfTrue, we will get a line that looks like
    # --FORMAT:END:Storage:n
    # where n is the step number that will be used for Out[n] in Jupyter.
    # We return a list of output records of the form
    #   {'step': step, 'outputs': outputs}
    # where outputs is a list with entries of the form
    #   {content_type: content}.
    def split_output(self, s):
        stepped_outputs = [] # intermediate collection of data
        lines = s.split('\n')
        n = len(lines)
        i = 0
        outputs = []
        step = 0
        while i < n:
            content_type = 'text/plain' # default
            line = lines[i]
            content = line # initialize
            i += 1
            if line.strip() == '': continue

            # In the following we misuse the content_type field
            # with the value 'ERROR' to signal that the output should be
            # send as text to stderr.

            if line.startswith('--FORMAT:BEG:'):
                y = line[13:]
                p = y.find(':')
                if p < 0:
                    f = y
                else:
                    f = y[:p]
                    step = int(y[p+1:])
                content = '' # delete '--FORMAT:BEG:'-line
                end_marker = '--FORMAT:END:' + y
                if f == 'FormatMathJax':
                    content_type = 'text/latex'
                elif f == 'TypeTime' or f == 'Type' or f == 'Time':
                    line = lines[i]
                    i += 1
                    if f == 'TypeTime':
                        p = line.find('Type: ')
                        ty = self.addLinks(line[p+6:])
                        p = lines[i].find('Time: ')
                        content = [ty, lines[i][p:]] # type and time
                        i += 1 # we have two lines to consider
                    elif f == 'Type':
                        p = line.find('Type: ')
                        content = [self.addLinks(line[p+6:])] # type
                    elif f == 'Time':
                        p = line.find('Time: ')
                        content = [line[p:]] # time
                    i += 1 # step over end marker
                    outputs.append({'text/html': self.formatTypeTime(content)})
                    continue
                elif f == 'ERROR' or f == 'KeyedMsg':
                    content_type = 'ERROR'

            elif line == '$$': # Corresponds to TexFormat
                f = 'TexFormat'
                content_type = 'text/latex'
                content += pretex + '\n'
                end_marker = '$$'
            elif line.startswith('<math xmlns='): # Corresponds to MathMLFormat
                f = 'MathMLFormat'
                content_type = 'text/html'
                end_marker = '</math>'
            elif line.startswith('scheme: '):  # Corresponds to TexmacsFormat
                f = 'TexmacsFormat'
                end_marker = ')'
            else:
                outputs.append({'ERROR': line + '\n'})
                continue

            # Collect the lines into content until the end_marker is found
            while i < n:
                line = lines[i]
                i += 1
                if line == end_marker:
                    if not end_marker.startswith('--FORMAT:END:'):
                        content += end_marker + '\n'
                    break
                else:
                    content += line + '\n'

            if f == 'Storage':
                # end of output series
                stepped_outputs.append({'step': step, 'outputs': outputs})
                outputs = []
            else:
                outputs.append({content_type: content})
        if outputs:
            stepped_outputs.append({'step': step, 'outputs': outputs})

        return stepped_outputs



    def handle_fricas_result(self):
        # See webspad.list for the structure of out.
        # If an error occured, simply put every output to stderr.
        out = self.server.output
        stepped_outputs = self.split_output(out['stdout'])
        # Uncommend the following for debugging purposes.
##        self.send_response(self.iopub_socket, 'stream',
##                           {'name': 'stdout', 'text':
##                            'SteppedOutput: {0}'.format(stepped_outputs)})

        if out['error?'] == 'T':
            for so in stepped_outputs:
                for data in so['outputs']:
                    for k in data.keys():
                        self.send_response(self.iopub_socket, 'stream',
                            {'name': 'stderr', 'text': data[k]})
            return False # signal error

        for so in stepped_outputs:
            if not so['outputs']: continue
            is_first = True
            # Now print the remaining output
            for data in so['outputs']:
                for k in data.keys():
                    if k == 'ERROR':
                        self.send_response(self.iopub_socket, 'stream',
                            {'name': 'stderr', 'text': data[k]})
                    elif is_first:
                        self.send_response(self.iopub_socket, 'execute_result',
                            {'execution_count': so['step'],
                             'data': data,
                             'metadata': {}})
                        is_first = False
                    else:
                        self.send_response(self.iopub_socket,'display_data',
                            {'data': data, 'metadata': {}})

        return True


if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    import itertools

    # Get all FriCAS identifiers for tab-completion into the variables
    # fricas_operations and fricas_constructors.
    # We need this for do_complete and do_inspect.
    # We start a separate process tho get this information.
    pid0 = Popen(['fricas', '-nosman',
                 '-eval', ')what operations',
                 '-eval', ')what categories',
                 '-eval', ')what domains',
                 '-eval', ')what packages',
                 '-eval', ')quit'],stdout=PIPE)

    out = pid0.stdout.readlines()
    lines = [s.decode("utf-8").rstrip() for s in out]

    # Look for line that start the ")what operations" output.
    while not lines.pop(0).startswith('Operations whose names satisfy '): pass

    fricas_operations = []
    line = lines.pop(0) # skip empty line from beginning
    line = lines.pop(0)
    while line:
        fricas_operations.append(line)
        line = lines.pop(0)
    # operations already come sorted from FriCAS

    # Look for line that starts the ")what categories" output,
    # Regular expression: /^-+ Categories -*$/
    while not lines.pop(0).startswith('--------------'): pass
    wordlist = [x.split() for x in lines if not x.startswith('--------------')]
    # wordlist is a list of lists. Flatten it.
    fricas_constructors = list(itertools.chain.from_iterable(wordlist))
    fricas_constructors.sort()

    # Prepare to load webSPAD and start the Hunchentoot webserver
    # while starting FriCAS.
    path = os.path.dirname(os.path.abspath(__file__))
    prereq = ')lisp (load "{0}/webspad")'.format(path)
    start  = ')lisp (defvar webspad::fricas-acceptor '
    start += '(webspad::start {0} "localhost"))'.format(htport)

    # A couple of ways to start FriCAS
    # --------------------------------
    # Uncomment or edit as you like.
    # Default:
    pid = Popen(['fricas','-eval',prereq,'-eval',start])

    # Start the FriCAS process in a separate terminal.
    # That might be good for controlling FriCAS directly in case
    # the Jupyter interface hangs or a process runs too long.
    # Of course, the start-fricas-in-terminal way also works
    # with additional fricas options.
    ## pid = Popen(['gnome-terminal', '--title=jfricas', '--'] +
    ##             ['fricas','-eval',prereq,'-eval',start])

    # Start the FriCAS process without opening the HyperDoc window.
    ## pid = Popen(['fricas','-eval',prereq,'-eval',start] +
    ##             ['-noht])

    # Start the FriCAS process by calling only FRICASsys.
    # The following does currently not work.
    ## pid = Popen(['fricas','-nosman','-eval',prereq,'-eval',start])
    # but
    ## pid = Popen(['gnome-terminal', '--title=jfricas', '--'] +
    ##             ['fricas','-nosman','-eval',prereq,'-eval',start])
    # does.

    # Start the kernel.
    IPKernelApp.launch_instance(kernel_class=SPAD)
