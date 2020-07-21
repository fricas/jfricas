=====
Usage
=====

Starting a notebook
-------------------

To start a notebook, type in a terminal:
::

  $ jupyter notebook

Your default webbrowser should open with a list of the files in the
current directory.

Choose ``New->FriCAS`` to open a new notebook of type ``FriCAS``.

You can also start a nootbook directly from the commandline.
::

  jupyter notebook foo.ipynb

Note that you might realize a longer delay when you start the very
first FriCAS_ notebook for the very first time, because of some cache
initialization of the Hunchentoot_ webserver. That delay will only
happen once.


Closing a notebook
------------------

To close the notebook, choose
::

  File -> Close and Halt

from the menu.

Note that simply closing the browser tab does not close the notebook
or the running FriCAS_ process(es) behind it. You can reopen the tab by
clicking on the name of your notebook (next to the then green icon).

It is also possible to kill the FriCAS_ process(es) running behind the
notebook by clicking on the ``Shutdown`` button in the running
notebook section of your Jupyter_ session. See
::

   http://localhost:8888/tree#running

Note that you then have to close the browser tab belonging to the
respective notebook yourself.

If you run jupyter locally, you should not click on the ``Logout``
button in the upper right corner. However, in case you did, you can
login again with the token that is printed in the terminal where you
have entered ``jupyter notebook``.

In case you kill jupyter by pressing ``Ctrl-C`` in the terminal window
from where you have started ``jupyter notebook``, there will be
running FriCAS_ processes left in your process tree. You must kill
that by means of your operating system.


Running a notebook
------------------

You can enter any FriCAS_ expression into a cell of the notebook. For
example,
::

  D(sin x, x)

followed by ``Shift-RETURN`` will return
::

  cos x

The special string ``)version`` returns the version number of FriCAS_
that you are using.

Command completion works as usual by pressing ``TAB``.
It will complete any know function or constructor (category, domain,
or package) name.

Pressing ``Shift-TAB`` shows help for the respective identifier under
the cursor (if available).

Pressing ``Shift-TAB`` twice yields extended help.

For general information about how to work with a Jupyter_ notebook
consult the respective `Jupyter notebook documentation
<https://jupyter-notebook.readthedocs.io/en/stable/notebook.html>`_
directly.

Showing nicer output
--------------------

With the help of MathJax_, **jFriCAS** can show output in a nicely
rendered form. This feature can be switched on via
::

 )set output tex on
 )set output algebra off

For example::

  exp(sin x)/(exp(x)+x)

appears as

  :math:`\frac{{e}^{\sin\left(x\right)}}{{e}^{x}+x}`

instead of the standard 2D output
::

    sin(x)
  %e
  --------
     x
   %e  + x

If the rendered output is too small, try to right-click on it and use
`Math Settings` -> `Scale All Math` to adjust it to your preferred size.

Note that MathJax_ cannot always deal correctly with the TeX output of
FriCAS_. There is a new output format in preparation


  https://github.com/hemmecke/fricas/tree/formatted

which includes code for a better MathJax_ support.
