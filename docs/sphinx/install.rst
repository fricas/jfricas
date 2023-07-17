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

We require a version of FriCAS_ which was compiled with a
`Common Lisp`_ that supports multithreading and Hunchentoot_.
We recommend `SBCL`_ 1.4.5 or later.

See the
`FriCAS installation guide <http://fricas.github.io/install.html>`_
in order to install a FriCAS_ version that you can use with **jFriCAS**.

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

Note that **jFriCAS** 1.0.0 only works under a particular setup that
we do not support anymore. Use version 2.0.0.

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

   fricas --version
        FriCAS a3c1d1dc5829126604345db3d672c2386d9296ae
        based on sbcl 2.1.11.debian

   echo ")lisp (lisp-implementation-type)" | fricas -nosman
        Value = "SBCL", required: SBCL

   echo ")lisp (lisp-implementation-version)" | fricas -nosman
        Value = "2.1.11.debian", required: >= 1.2.6

   python3 --version
        Python 3.10.6, required: >= 3.3,

   pip3 --version
        pip 22.0.2 from /usr/lib/python3/dist-packages/pip (python 3.10)

   pip3 show requests
        Name: requests
        Version: 2.31.0
        Summary: Python HTTP for Humans.
        Home-page: Home-page: https://requests.readthedocs.io
   Install: pip3 install requests

   pip3 freeze             should show something like ...
        ipykernel==6.24.0
        ipython==8.14.0
        ipython-genutils==0.2.0
        ipywidgets==8.0.7
        jsonschema==4.18.0
        jsonschema-specifications==2023.6.1
        jupyter==1.0.0
        jupyter-console==6.6.3
        jupyter-contrib-core==0.4.2
        jupyter-contrib-nbextensions==0.7.0
        jupyter-events==0.6.3
        jupyter-highlight-selected-word==0.2.0
        jupyter-nbextensions-configurator==0.6.3
        jupyter_client==8.3.0
        jupyter_core==5.3.1
        jupyter_server==2.7.0
        jupyter_server_terminals==0.4.4
        jupyterlab-pygments==0.2.2
        jupyterlab-widgets==3.0.8
        jupytext==1.14.7
        ...

   otherwise install jupyter with:
   pip3 install jupyter

   jupyter --version
        4.6.3
        Selected Jupyter core packages...
        IPython          : 8.14.0
        ipykernel        : 6.24.0
        ipywidgets       : 8.0.7
        jupyter_client   : 8.3.0
        jupyter_core     : 5.3.1
        jupyter_server   : 2.7.0
        jupyterlab       : not installed
        nbclient         : 0.8.0
        nbconvert        : 7.6.0
        nbformat         : 5.9.0
        notebook         : 6.5.4
        qtconsole        : 5.4.3
        traitlets        : 5.9.0

   cat /etc/os-release

   PRETTY_NAME="Ubuntu 22.04.2 LTS"
   NAME="Ubuntu"
   VERSION_ID="22.04"
   VERSION="22.04.2 LTS (Jammy Jellyfish)"
   VERSION_CODENAME=jammy
   ID=ubuntu
   ID_LIKE=debian
   HOME_URL="https://www.ubuntu.com/"
   SUPPORT_URL="https://help.ubuntu.com/"
   BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
   PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
   UBUNTU_CODENAME=jammy


References
----------
.. _Python3: https://www.python.org/
.. _Pip3: https://pypi.org/project/pip/
.. _Jupyter: https://jupyter.org/
.. _Hunchentoot: https://edicl.github.io/hunchentoot/
.. _Common Lisp: https://en.wikipedia.org/wiki/Common_Lisp
.. _SBCL: http://sbcl.org/
.. _Firefox: https://www.mozilla.org/en-US/
