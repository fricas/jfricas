============
Installation
============

**jFriCAS** comprises three files which have to be installed:

  :Kernel spec:    `kernel.json`_
  :Wrapper Kernel: `fricaskernel.py`_
  :Web service:    `webspad.lisp`_

The whole trick is to put these files into the right places.


.. toctree::
   :maxdepth: 1
   :caption: Contents:


Prerequisites
-------------
The only supported OS at the moment is GNU/Linux - Debian/Ubuntu.

For some other OS a docker image will be provided (soon).

We require a version of FriCAS_ which was compiled with a `Common
Lisp`_ that supports multithreading and `Hunchentoot`_.
We recommend `SBCL`_ 1.4.5 or later.

Since Python 2.7 is not maintained anymore, we require Python3_.
We recommend Python 3.6 or later.
For instance:
::

   $ sudo apt update
   $ sudo apt install python3

Check via
::

   $ python --version

whether your system's ``python`` refer to version 3 or higher.

`Pip3`_ is also required (recommended version 9.0.1 or later).
::

   $ sudo apt install python3-pip

In order to load Common Lisp programs we require `ASDF`_ (v3.3 or
later). Install `cl-asdf`_ as follows:
::

   $ sudo apt install cl-asdf


The `Hunchentoot`_ webserver (v1.2.35 or later). `cl-hunchentoot`_
can be installed by:
::

   $ sudo apt install cl-hunchentoot


Install Jupyter_ system-wide via
::

   $ sudo apt install jupyter

or under ``$HOME/.local`` via
::

   $ pip3 install jupyter


It is hardly worth to mention, a web-browser is required as well. We
strongly recommend the latest `Firefox`_.


Install from PyPI
-----------------

If the prerequisites are satisfied, installing **jFriCAS** from
``PyPI`` is as simple as calling
::

   $ pip3 install jfricas

The ``kernel.json`` file will be installed automatically by the above
command.


Install from GitHub
-------------------
::

   $ git clone https://github.com/fricas/jfricas.git
   $ cd jfricas
   $ pip3 install .


Install System-wide
-------------------

Do the the installation command with ``sudo`` prefix.
::

   $ sudo pip3 install jfricas

Note that ``sudo pip3 install --user jfricas`` is not supported.


Virtual environments
--------------------

In case you do not want to clutter your ``$HOME/.local`` directory or
you want to develop **jFriCAS** itself, it makes sense to install
**jFriCAS** into a virtual environment.

Make sure you have the ``venv`` python module installed.
::

  $ sudo apt install python3-venv

Now create a virtual environment and activate it.
::

  $ V=$HOME/venv-jfricas
  $ python -m venv $V
  $ source $V/bin/activate

  $ pip3 install wheel
  $ pip3 install jupyter
  $ pip3 install requests
  $ pip3 install jfricas

Note that ``jupyter --paths`` still shows that the runtime path is
under ``$HOME/.local``.


Check installation
------------------

You should see ``jfricas`` among the output of
::

   $ jupyter kernelspec list


Uninstall
---------

The **jFriCAS** code can be removed by
::

   $ pip3 uninstall jfricas

Additionally, you have to remove the ``jfricas`` kernelspec for
Jupyter_. Locate it via
::

  $ jupyter kernelspec list

and remove it via
::

  $ jupyter kernelspec remove jfricas

or by removing the respective directory.


Check requirements
------------------

The following sequence of commands show how to check the presence and
(if present) the versions of the required programs:
::

  echo ")version" | fricas -nosman
        Value = "FriCAS 1.3.5 compiled at Sun Feb  3 18:21:59 UTC 2019"

  echo ")lisp (lisp-implementation-type)" | fricas -nosman
        Value = "SBCL", required: SBCL

  echo ")lisp (lisp-implementation-version)" | fricas -nosman
        Value = "1.4.5.debian", required: >= 1.2.6

  python3 --version
        Python 3.6.9, required: >= 3.3,

  pip3 --version
        pip 9.0.1 from /usr/lib/python3/dist-packages (python 3.6))

  pip3 show requests
        Name: requests
        Version: 2.22.0
        Summary: Python HTTP for Humans.
        Home-page: http://python-requests.org
  Install: pip3 install requests

  pip3 freeze (should show something like ...
        ipykernel==4.8.2
        ipython==6.4.0
        ipython-genutils==0.2.0
        ipywidgets==7.2.1
        jsonschema==2.6.0
        jupyter==1.0.0
        jupyter-client==5.2.3
        jupyter-console==5.2.0
        jupyter-core==4.4.0
        ...
  otherwise install jupyter with:
  pip3 install jupyter

  jupyter --version
        4.6.3

  apt list cl-asdf
        cl-asdf/bionic,bionic,now 2:3.3.1-1 all [installed,automatic]

  apt list cl-hunchentoot
        cl-hunchentoot/bionic,bionic,now 1.2.35-1 all [installed]


  cat /etc/os-release

  NAME="Ubuntu"
  VERSION="18.04.2 LTS (Bionic Beaver)"
  ID=ubuntu
  ID_LIKE=debian
  PRETTY_NAME="Ubuntu 18.04.2 LTS"
  VERSION_ID="18.04"
  HOME_URL="https://www.ubuntu.com/"
  SUPPORT_URL="https://help.ubuntu.com/"
  BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
  PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
  VERSION_CODENAME=bionic
  UBUNTU_CODENAME=bionic


References
----------
.. _Python3: https://www.python.org/
.. _Pip3: https://pypi.org/project/pip/
.. _Jupyter: https://jupyter.org/
.. _cl-asdf: https://tracker.debian.org/pkg/cl-asdf
.. _cl-hunchentoot: https://tracker.debian.org/pkg/hunchentoot
.. _Hunchentoot: https://edicl.github.io/hunchentoot/
.. _Common Lisp: https://en.wikipedia.org/wiki/Common_Lisp
.. _SBCL: http://sbcl.org/
.. _ASDF: https://common-lisp.net/project/asdf/
.. _Firefox: https://www.mozilla.org/en-US/

+------------------------+------------+----------+------------------+
| App / Versions, OS     | Min. ver.  | Tested   | OS/inst          |
+========================+============+==========+==================+
| `FriCAS`_              | 1.3.2      | 1.3.5    | Deb/Ub (make)    |
+------------------------+------------+----------+------------------+
| `Python3`_             | 3.5        | 3.6.9    | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `Pip3`_                | 9.0        | 9.0.1    | apt              |
+------------------------+------------+----------+------------------+
| `Jupyter`_             | 4.4        | 4.6.3    | pip3             |
+------------------------+------------+----------+------------------+
| `Requests`_            | 2.22.0     | 2.22.0   | pip3             |
+------------------------+------------+----------+------------------+
| `cl-asdf`_             | 3.3        | 3.3.1    | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `cl-hunchentoot`_      | 1.2.35     | 1.2.35   | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+


iFriCAS
-------
An earlier attempt to provide a jupyter kernel for FriCAS_ that was
LISP based was called **iFriCAS**.
Because it has been registered with the same kernel name: ``FriCAS``,
you cannot use it togeter with **jFriCAS**.

If ::

    $ jupyter kernelspec list

shows ``ifricas`` as available kernel, then you have to remove it by
::

    $ jupyter kernelspec remove ifricas

or edit the name in ``kernel.json`` in the respective directory.
