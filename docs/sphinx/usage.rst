===========
Basic Usage
===========

:Reference: Jupyter Notebook_ (readthedocs)

Start the notebook server from the command line:
::
    
	jupyter notebook

You should see the notebook open in your browser.


.. _Notebook: https://jupyter-notebook.readthedocs.io/en/stable/



New -> FriCAS
-------------
Chose the new ``FriCAS`` kernel by
::

  New -> FriCAS

.. image:: pics/new_fricas.png

If all goes as it should, you will see a similar output when entering the
commands as in the picture below. 

.. image:: pics/versions.png

The *console* window shows kernel messages, especially it will tell you
when the notebook was saved (periodically)

.. image:: pics/kernel_console.png


Input
-----

:Send key: ``Shift-Return``

You may enter single line code followed by a ``Shift-Return``, i.e. hold
down the ``Shift`` key, then press the ``Return`` key (**note** that 
``--`` designates a commment in ``Fricas``). 

.. image:: pics/single_line.png

or appropriately indented multi-line code:

.. image:: pics/multi_line.png

**Recall** that ``_`` denotes the line continuation character in ``FriCAS``.

There are a lot of features in Jupyter, e.g. running all cells
 
.. image:: pics/run_all.png

or a print preview (rendered HTML) which allows you to save the complete
page (including images, javascript and so on) by your browser. For instance,
``Save page as`` in ``Firefox``.

.. image:: pics/print_preview.png


Close and Halt
--------------
The notebook may be terminated by
::

  )quit
  
or from the menu (preferred)
::

  Close and Halt

.. image:: pics/close_and_halt.png


Code completion
---------------

:Key: ``TAB``

Code completion works as usual by pressing the ``TAB`` key 

.. image:: pics/code_completion.png


Code inspection
---------------

:Key: ``Shift-TAB``

Press ``Shift-TAB`` to show a pop-up pager that will show the usage of
an operation, provided that it was found (similar as ``)display operation``).

.. image:: pics/code_inspect.png

The pager text may be expanded if necessary (mouse click on ``+``):

.. image:: pics/code_inspect_expand.png


Accessing the shell
-------------------

:prefix: ``!``

To run a shell command (e.g. ``ls``), put a ``!`` in front of the command.
**Note** that ``!`` must be the first character in the cell, otherwise it
will not be interpreted as shell prefix character.  

.. image:: pics/shell_prefix.png

You may enter multiple lines, however, ``!`` must still be the first character
in the cell, as already remarked.

.. image:: pics/shell_multiline.png

The output of the last shell command (at least parts of it) is also 
**stored** in the ``Fricas`` variable 
::

  __system_result
  
 
.. image:: pics/system_result.png


Python commands
---------------

:usage:  ``)python``

With the fake system command ``)python``, one may evaluate a limited set
of python commands (internally an ``eval``, works only in Jupyter).

For example, getting the IP port of the current Fricas+HT instance:

.. image:: pics/htport.png

or, if wou want to know, where the current kernel has been installed:

.. image:: pics/fricaskernel_loc.png

Remark: If you want to have more access to Python, you could edit the
kernel file and add ``globals`` to ``eval``.

Editing files
-------------
There are may ways to create and edit files during a Jupyter notebook session.
(certainly from the Jupyter main page). You can use the Fricas system 
command ``)edit``, provided the shell variable ``EDITOR`` has been set, or
you may use any editor by prefixing ``!``, e.g. ``gedit``.
  
:example: ``!gedit``

Create a new file ``test.input`` and enter the function definition
::

  f(x) == x*x

Then ``save`` and (optionally) ``close``:

.. image:: pics/gedit.png

Read the function in by ``)read`` and try ``f(8)``:

.. image:: pics/gedit2.png


LaTeX output
------------

:command: ``)set output tex on/off`` 

Setting ``TeX`` output on, you will see both, algebraic and ``MathJax``
output (**right-click** on the ``MathJax`` output to get a context menu).

.. image:: pics/set_out_tex_on.png


You have to turn the plain text output off if you do want the rendered 
output only: ``)set output algebra off``

.. image:: pics/tex_algebra_off.png


HTML from FriCAS
----------------

:prefix: ``$HTML$`` 

There is an experimental feature that allows to return string output from
Fricas which will be rendered as ``HTML`` if prefixed with ``$HTML$``:

.. image:: pics/html_prefix.png

A string (``Type: String``) like
::

   "$HTML$<h1>Header"
   
will appear as HTML header, while

::

   "<h1>Header"

will be displayed as plain text.

Draw
----
The ``draw`` commands of Fricas will work as as usually. You may save
the plot as a ``Postscript`` file, then convert it to ``PNG`` and
eventually insert it into the notebook. 

.. image:: pics/draw.png

:todo: ps2png 

A simple (yet powerful) application to convert postscript images to ``PNG``
is ``ps2png`` from the TtH_ and TtM_ packages
::

   !apt search ttm
   
   tth-common/bionic,now 4.12+ds-2 amd64 [installed]
      auxiliary software material for TtH and TtM

   ttm/bionic 4.12+ds-2 amd64
      TeX/LaTeX to MathML converter
      

Install ``tth-common`` with ``apt`` then ``ps2png`` will be available.

After conversion you can insert the ``PNG`` (or ``GIF`` as well) into the
notebook by
::

   ESC-M  (i.e. change to markdown mode)
   
   ![text](pic.png)  (RETURN)
   
and the picture should appear.   

:SYNOPSIS: ps2png

::

       tth [options] [<file.tex] [>file.html] [2>err]
       tth [options] file.tex [2>err]
       
       ttm [options] [<file.tex] [>file.xml] [2>err]
       ttm [options] file.tex [2>err]

       latex2gif file (no extension)

       ps2gif file.ps file.gif [icon.gif]

       ps2png file.ps file.png [icon.gif]



.. _TtM: http://manpages.ubuntu.com/manpages/precise/man1/ttm.1.html
.. _TtH: http://manpages.ubuntu.com/manpages/precise/man1/tth.1.html



Gnuplot (experimental)
----------------------
Gnuplot_ (cited from the homepage) is a portable command-line driven graphing utility for Linux, OS/2, MS Windows, OSX, VMS, and many other platforms. It was originally created to allow scientists and students to visualize mathematical functions and data interactively, but has grown to support many non-interactive uses such as web scripting. It is also used as a plotting engine by third-party applications like Octave. Gnuplot has been supported and under active development since 1986.
 
To use Gnuplot_ in ``jfricas``, it is required to have installed a version which supports
the ``canvas`` terminal, e.g. version 5.2 or later   
::

  sudo apt install gnuplot-nox

  gnuplot-nox/bionic,now 5.2.2+dfsg1-2ubuntu1 amd64 [installed]
  Command-line driven interactive plotting program. No-X package

In order to find and serve the javascript files for the *HTML canvas* it is also
required to create a symbolic link (as in the example below)
::

  # Open a python3 session to find the static file path
  >>> import notebook.notebookapp
  >>> notebook.notebookapp.DEFAULT_STATIC_FILES_PATH
  '/home/kfp/.local/lib/python3.6/site-packages/notebook/static'

  
  # In a shell console create the symbolic link correspondingly
  ln -s /usr/share/gnuplot/gnuplot/5.2/js/ 
    /home/kfp/.local/lib/python3.6/site-packages/notebook/static/gpjs

  # Start a notebook and check if it is ok by
  http://localhost:8888/static/gpjs/canvastext.js


:ref:  Gnuplot_ 


.. image:: pics/gnuplot_test.png

.. image:: pics/sys_gnuplot.png

.. image:: pics/gpdata1.png

.. image:: pics/gpdata2.png

.. _Gnuplot: http://www.gnuplot.info/

Help
----

.. image:: pics/sys_help.png

.. image:: pics/sys_help_edit.png

.. image:: pics/sys_d_op.png


Syntax Highlighting
-------------------
For those who like syntax highlighting there is a *codemirror mode* file, ``spad.js`` in 
the folder ``docs/static``.
This file should be served from
::

    localhost:8888/static/components/codemirror/mode/spad/spad.js

In order to install it, we first have to find the ``static`` directory of the Jupyter notebook.
::

    STATIC=$(python3 -c "import notebook.notebookapp as nap; print(nap.DEFAULT_STATIC_FILES_PATH)")

    echo $STATIC
    e.g. /.../.local/lib/python3.6/site-packages/notebook/static


    ls $STATIC/components/codemirror/mode/
    # if no spad dir there, then

    mkdir -p $STATIC/components/codemirror/mode/spad
    cp docs/static/spad.js $STATIC/components/codemirror/mode/spad

Next time you start a notebook the mode will be loaded, and you should see coloured
keywords and built-in's like ``if, then, else, for ,repeat ...``.



