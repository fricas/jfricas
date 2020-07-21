=======
jFriCAS
=======

https://github.com/fricas/jfricas


**jFriCAS** is a Jupyter_ kernel for FriCAS_, i.e.,
it makes a Jupyter_ notebook frontend for FriCAS_ possible.

When a Jupyter_ notebook of type FriCAS_ is opened, **jFriCAS** starts
a new FriCAS_ process and a webserver (Hunchentoot_). In fact,
**jFriCAS** makes FriCAS_ behave like a webserver, i.e., responding to
GET and POST requests.

On the Python side these GET and POST requests are handled by the
requests_ library inside the wrapper kernel `fricaskernel.py`_. To put
it in a nutshell, **jFriCAS** essentially comprises three files which
have to be installed:

  :Kernel spec:    `kernel.json`_
  :Wrapper Kernel: `fricaskernel.py`_
  :Web service:    `webspad.lisp`_

Cell input input in the Jupyter_ notebook is taken by
`fricaskernel.py`_ and sent via `webspad.lisp`_ to FriCAS_ (running
behind a webserver). Output from FriCAS_ is collected by
`webspad.lisp`_ and given back to `fricaskernel.py`_ where it is
analysed and transformed into an output format that the Jupyter_
notebook understands, and thus eventually displayed.

FriCAS_ allows different output formats.
**jFriCAS** supoorts 2D and TeX output formats.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   install
   usage
   misc
