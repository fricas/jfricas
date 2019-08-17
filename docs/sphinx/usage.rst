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


Gnuplot
-------
:experimental:  todo 

:ref:  gnuplot_ 

.. image:: pics/gnuplot_test.png

.. image:: pics/sys_gnuplot.png

.. _gnuplot: http://www.gnuplot.info/

Help
----

.. image:: pics/sys_help.png

.. image:: pics/sys_help_edit.png

.. image:: pics/sys_d_op.png