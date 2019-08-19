====================
Kernel Configuration
====================
For advanced users we here provide a description of the parameters that
may be changed without running the risk to spoil the kernel.
Nevertheless, you have to do this at your own risk (best effort support only ;)

Finding the Kernel
------------------
First of all, we have to find the location where the kernel has been
installed:
::

   In the running kernel (notebook or console):
   
   )python os.path.abspath(__file__)
   
Example:
   
.. image:: pics/kernel_loc.png    


Edit the Kernel File
--------------------
Open the file path (from above) into your favourite editor and look for
the ``BEGIN  user configuration options``.
::

    # ********************************
    # BEGIN user configuration options
    # ********************************
    pycmd = ')python'
    shcmd = '!'
    shutd = ')shutdown'
    gplot = ')gnuplot'

Start options
^^^^^^^^^^^^^
The start options cotrol how the fricas webserver will be started:
::

    fricas_start_options = '-noht'   ### -nox blocks if draw is used (others?)
    fricas_terminal = []             ###  E.g. ['xterm','-e'] for 'xterm'


Shell commands
^^^^^^^^^^^^^^
These variables control the external shell commands:
::
 
    shell_timeout = 15 # Timeout for shell commands in secs.
    shell_result = None # store last sh result in python
    shell_result_fricas = '__system_result:="{0}"' # store sh result in Fricas

HTML prefix
^^^^^^^^^^^
The HTML prefix defines which strings (Type: String) returned from FricAS
will be interpreted as HTML code.

::
	
	html_prefix = '$HTML$'

LaTeX/MathJax related
^^^^^^^^^^^^^^^^^^^^^
The MathJax output (``set output tex on``) is controlled by the following
options:
::
	
	# LaTeX color/size parameters
	type_color = r"blue"
	type_size = r"\scriptsize"
	tex_color = r"black"
	tex_size = r"\normalsize"
	
	# Templates (TeX)
	pretex1 = r"\(\def\sp{^}\def\sb{_}\def\leqno(#1){}\)"
	pretex2 = r"\(\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\)"
	pretex3 = r"\(\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}\)"
	pretex4 = r"\(\require{color}\)"
	pretex = pretex1+pretex2+pretex3+pretex4
	ljax = r"$$"  # variants: r"\("
	rjax = r"$$"  #           r"\)"
	
	# texout_types.format(tex_color,tex_size,tex,type_color,type_size,type)
	texout_types = r"""
	{{\color{{{0}}} {1} {2}}} \\[0.9ex] {{\color{{{3}}} {4} \text{{{5}}}}} \\
	"""
	
	# texout.format(tex_color,tex_size,tex)
	texout = r"""
	{{\color{{{0}}} {1} {2}}}
	"""

Gnuplot
^^^^^^^
For the built-in Gnuplot feature:
::
	
	# gnuplot javascript files location
	# (todo: ought to be locally served, e.g. /static)
	# gpjsf = 'https://nilqed.github.io/jfricas.pip/js'
	gpjsf = '/static/gpjs'
	
	# gnuplot canvas template (html5)
	gptpl =r"""
	<script src="{0}/canvastext.js"></script>
	<script src="{0}/gnuplot_common.js"></script>
	<canvas id="{1}" width=600 height=400></canvas>
	<script>{2}</script>
	<script>{3}();</script>
	"""

End of user config
^^^^^^^^^^^^^^^^^^
If you make changes beyond this point, then you will be on your own:
::
	
	# ***************
	# END user config
	# ***************

