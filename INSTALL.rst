===============================
FriCAS Jupyter Kernel (jfricas)
===============================

This Jupyter kernel for `FriCAS`_ is (hopefully) the final one of a series of
attempts to provide a stable and easy maintainable application. The concept is
rather simple, compared to the former kernels based on CL/ZeroMQ or pexpect:
::

    |------------------|                     |---------------|
    | webSPAD (WS)     |<---|kernel.json|--->|requests (Py)  |
    | Hunchentoot (HT) |                     |fricaskernel.py|
    | FriCAS (BOOT)    |                     |Jupyter/IPython|<--->|Browser  |
    | Common Lisp (CL) |                     |Python3        |     |Notebook |
    |------------------|                     |---------------|     |QtConsole|


`Hunchentoot`_ is a (well-known) webserver for `Common Lisp`_ while `webSPAD`_
is a program (also in CL) providing the interface between FriCAS and
Hunchentoot. In other words, FriCAS acts like a webserver responding to GET and
POST requests. On the Python side these GET and POST requests are handled by the
``requests`` library inside the wrapper kernel ``fricaskernel.py``. To put it 
in a nutshell, our new kernel essentially comprises three files which we have 
to install:


 :Kernel spec: kernel.json
 :Wrapper Kernel: fricaskernel.py
 :Web service: webspad.lisp   

The whole trick is to put these files into the right places.


**INSTALLATION INSTRUCTIONS**

1. `Prerequisites`_
2. `Installation using pip3`_
3. `Manual installation`_
4. `Virtual environments`_
5. `Running the notebook`_
6. `Attaching a console`_
7. `Hyperdoc, Graphics, X11`_
8. `Uninstall`_

Appendix

*  `Checking requirements`_
*  `References`_ 
*  `Notes`_


Prerequisites
-------------
The only supported OS at the moment is GNU/Linux - Debian/Ubuntu.

For some other OS a docker image will be provided (soon).

We require a version of `FriCAS`_ which was compiled with a `Common Lisp`_ that 
supports multithreading and `Hunchentoot`_. We recommend `SBCL`_ 1.4.5 or later.

Since Python 2.7 will not be maintained past 2020, we will concentrate on
`Python3`_ (https://pythonclock.org/) only. We recommend Python 3.6 or later.
For instance:
::

  	$ sudo apt update
	$ sudo apt install python3.6


`Pip3`_ is also required (recommended version 9.0.1 or later)::

        $ sudo apt install python3-pip


In order to load Common Lisp programs we require `ASDF`_ (v3.3 or later):
::

        $ sudo apt install cl-asdf


The `Hunchentoot`_ webserver (v1.2.35 or later) will be installed by:
::

        $ sudo apt install cl-hunchentoot


It is hardly worth to mention, a web-browser is required as well. We
strongly recommend the latest `Firefox`_.



Installation using pip3  
-----------------------
Regardless of whether you will use a virtual environment **venv**, this method 
should work as follows (assuming the prerequisites are satisfied):

Install ``Jupyter`` and ``jfricas`` from ``PyPI`` by
::

    $ pip3 install jupyter
    $ pip3 install jfricas
    $ python3 -m jfricas.install_kernel

The ``requests`` library and the ``kernel.json`` file will be installed 
automatically by *jfricas*.

Installation from GitHub
^^^^^^^^^^^^^^^^^^^^^^^^
::

    $ pip3 install jupyter
    $ git clone https://github.com/fricas/jfricas.git
    $ cd jfricas
    $ pip3 install .
    $ python3 -m jfricas.install_kernel


or, install directly from the ``git`` repository branch: 

::

    $ pip3 install jupyter
    $ pip3 install -e git+https://github.com/fricas/jfricas.git@master#egg=jfricas
    $ python3 -m jfricas.install_kernel
    

Manual installation
-------------------
We recall that on Debian/Ubuntu ``pip3`` will install packages locally, that is
the default is ``--user``. Thus, the method which we are going to describe here 
is to install into ``$HOME/.local/...`` folders.

First, we have to install ``jupyter`` and ``requests``:
::

    $ pip3 install jupyter
    $ pip3 install requests

Second, you will have to get the Git repository (assuming 'git' is installed):
::
 
	$ git clone https://github.com/fricas/jfricas.git 

Then ``cd`` to it:
::

	$ cd ./jfricas

and perform the following bash commands:
::

	$ jupyter kernelspec install ./jfricas/kspec --name='jfricas' --user
	$ jupyter kernelspec list 
	### you should see 'jfricas' now.

	# Search for the Python3 user site 
	$ PYUSRSITE=$(python3 -m site --user-site)

	# Copy kernel and webspad
	$ mkdir -p -v $PYUSRSITE/jfricas
	$ cp -v ./jfricas/fricaskernel.py $PYUSRSITE/jfricas
	$ cp -v ./jfricas/webspad.lisp    $PYUSRSITE/jfricas
  
That is all.

The above commands (except the jupyter installation) are more or less
the content of the ``install.sh`` file in the root directory of the Git
repository (``./install.sh`` from within the Git repo).



Virtual environments
--------------------
  tbd


Running the notebook
--------------------
To start the notebook, type (anywhere):
::

	$ jupyter notebook

A browser window should open where you can choose ``NEW->FriCAS``, then
after a while (first time some Lisp programs will be compiled, do not
worry) you may enter for instance: ``)version``, followed by ``Shift-RETURN``. 
  
To leave the notebook you can enter (**save it** before!)
::
  
  	)quit
  	
as usual, or choose (preferred) 
::
  
  	Close and Halt
  	
from the menu. After that (one or the other) the ``TAB`` can be closed.
To quit completely (leaving jupyter), press the ``Quit`` button on the
main Jupyter window. 
  

Attaching a console
-------------------
During a notebook session, a console can be attached to the running kernel
by the command
::
  
    $ jupyter qtconsole --existing
    
Of course, instead of a notebook at all, one may use consoles with the 
kernel:
::
  
    $ jupyter qtconsole --kernel=jfricas

   (or 'console' for the ordinary one, instead of 'qtconsole').
  
The ``QT`` console also requires:
::

    PyQt4 >= 4.7, PyQt5, PySide >= 1.0.3 or PySide2.
    


Hyperdoc, Graphics, X11
-----------------------
  tbd
  


Uninstall
---------
Depending on which method you have installed the kernel it can be completely
removed easily either by
::
  
	$ pip3 uninstall jfricas 
	
or by using the bash script ``uninstall.sh``:
::
  
    $ ./uninstall.sh
    
The commands may also be performed by hand:
::
  
    # Remove kernel
    jupyter kernelspec remove jfricas
    jupyter kernelspec list

    # Search for Python user site 
    PYUSRSITE=$(python3 -m site --user-site)

    # Remove kernel and webspad
    rm -v $PYUSRSITE/jfricas/fricaskernel.py
    rm -v $PYUSRSITE/jfricas/webspad.lisp
    rmdir -v --ignore-fail-on-non-empty $PYUSRSITE/jfricas 
  

APPENDIX
--------

Checking requirements
---------------------
The following sequence of commands show how to check the presence and (if)
the versions of the required programs:
::

  echo ")version" | fricas -nosman
        Value = "FriCAS 1.3.5 compiled at Sun Feb  3 18:21:59 UTC 2019"

  echo ")lisp (lisp-implementation-type)" | fricas -nosman
        Value = "SBCL", required: SBCL

  echo ")lisp (lisp-implementation-version)" | fricas -nosman
        Value = "1.4.5.debian", required: >= 1.2.6

  python3 --version
        Python 3.6.8, required: >= 3.3,

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
        4.4.0

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
.. _FriCAS: https://github.com/fricas/fricas
.. _Python3: https://www.python.org/
.. _Pip3: https://pypi.org/project/pip/
.. _Jupyter: https://jupyter.org/
.. _Requests: http://python-requests.org/
.. _cl-asdf: https://tracker.debian.org/pkg/cl-asdf
.. _cl-hunchentoot: https://tracker.debian.org/pkg/hunchentoot
.. _Hunchentoot: https://edicl.github.io/hunchentoot/
.. _Common Lisp: https://en.wikipedia.org/wiki/Common_Lisp
.. _SBCL: http://sbcl.org/
.. _ASDF: https://common-lisp.net/project/asdf/
.. _Firefox: https://www.mozilla.org/en-US/
.. _INSTALL: https://github.com/fricas/jfricas/blob/master/INSTALL
.. _webSPAD: https://github.com/nilqed/webSPAD


+------------------------+------------+----------+------------------+
| App / Versions, OS     | Min. ver.  | Tested   | OS/inst          |
+========================+============+==========+==================+
| `FriCAS`_              | 1.3.2      | 1.3.5    | Deb/Ub (make)    |
+------------------------+------------+----------+------------------+
| `Python3`_             | 3.5        | 3.6      | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `Pip3`_                | 9.0        | 9.0.1    | apt              |
+------------------------+------------+----------+------------------+
| `Jupyter`_             | 4.4        | 4.4      | pip3             |
+------------------------+------------+----------+------------------+
| `Requests`_            | 2.22.0     | 2.22.0   | pip3             |
+------------------------+------------+----------+------------------+
| `cl-asdf`_             | 3.3        | 3.3.1    | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `cl-hunchentoot`_      | 1.2.35     | 1.2.35   | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+


Notes
-----
If jupyter is already in use and 
::

	$ jupyter kernelspec list

shows `ifricas` as available kernel, then you have to remove it by 
::

	$ jupyter kernelspec remove ifricas

because it has been registered with the same kernel name: `FriCAS`.
Alternatively you may edit `kernel.json` of jfricas to use another name instead.

`*`) ifricas was the name of the old LISP based kernel.

Do not use ``)quit`` in consoles if you do not want to terminate the kernel. 
Use ``<menu>/Quit`` instead (this will close the terminal window only).

Command completion works as usual by pressing ``TAB``. 


:Authors:
    Ralf Hemmecke,  
    Kurt Pagani
    
    (and sundry other good-natured folks)

:Version: 0.2.9 of 2019/07/30
