=============
Miscellaneous
=============

.. toctree::
   :maxdepth: 1
   :caption: Contents:


FriCAS start options
--------------------

By default FriCAS_ is started by calling the ``fricas`` script that is
accessible from your ``$PATH``. You can change with what options the
script is called by changing the corresponding lines at the end of
your installed `fricaskernel.py`_. You find there a few other ways to
start FriCAS_, in particular, you can start a terminal that exposes
the FriCAS_ session that runs behind your Jupyter_ notebook or you can
start FriCAS_ without opening the HyperDoc window.


Output to files
---------------

Note that **jFriCAS** never directly writes to (most of) the streams
in FriCAS_. Rather it replaces them by its own streams to collect the
output for the Jupyter_ notebook. That means that the command
::

   )set output algebra foo.txt
   sin x + exp x

will not work as expected. The output still appears inside the
Jupyter_ notebook. No output is written to ``foo.txt``. See
`webspad.lisp`_ to figure out which streams are actually used in
**jFriCAS**.
