=====================
FriCAS Jupyter Kernel 
=====================

Prerequisites
-------------

+------------------------+------------+----------+------------------+
| App / Versions, OS     | Min. ver.  | Tested   | OS/inst          |
+========================+============+==========+==================+
| `FriCAS`_              | 1.3.2      | 1.3.5    | Deb/Ub (make)    |
+------------------------+------------+----------+------------------+
| `Python3`_             | 3.3        | 3.6      | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `Pip3`_                | 9.0        | 9.0.1    | apt              |
+------------------------+------------+----------+------------------+
| `Jupyter`_             | 4.2        | 4.4      | pip3             |
+------------------------+------------+----------+------------------+
| `Requests`_            | 2.22.0     | 2.22.0   | pip3             |
+------------------------+------------+----------+------------------+
| `cl-asdf`_             | 3.3        | 3.3.1    | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+
| `cl-hunchentoot`_      | 1.2.15     | 1.2.35   | Deb/Ub (apt)     |
+------------------------+------------+----------+------------------+


.. _FriCAS: https://github.com/fricas/fricas
.. _Python3: https://www.python.org/
.. _Pip3: https://pypi.org/project/pip/
.. _Jupyter: https://jupyter.org/
.. _Requests: http://python-requests.org/
.. _cl-asdf: https://tracker.debian.org/pkg/cl-asdf
.. _cl-hunchentoot: https://tracker.debian.org/pkg/hunchentoot




Install :rocket:
-------
pip3 install jupyter  (if you don't have it already)

git clone https://github.com/nilqed/jfricas.pip.git

cd jfricas.pip 

pip3 install .


Uninstall
---------
pip3 uninstall jfricas

jupyter kernelspec remove jfricas


ALL BELOW IS FOR https://github.com/nilqed/jfricas.git 
:construction:

This repo might be deleted soon !!

Notes
-----
If jupyter is already in use and ::

	$ jupyter kernelspec list

shows `ifricas` as available kernel, then you have to remove it by ::

	$ jupyter kernelspec remove ifricas

because it has been registered with the same kernel name: `FriCAS`.
Alternatively you may edit `kernel.json` of jfricas to use another name instead.
*) ifricas was the name of the old LISP based kernel.

Console(s) can be attached to the same kernel by: 
::

	$ jupyter qtconsole --existing (RETURN)

Do not use ")quit" in consoles if you do not want to terminate the kernel. 
Use <menu>/Quit instead (this will close the terminal window only).

Command completion works as usual by pressing <TAB>. 


Requirements (& tested with)
----------------------------

 * OS: debian/ubuntu
 * FriCAS (SBCL >= 1.2.6)
 * Python3 (>= 3.3)
 * pip3 
   -- pip3 install requests
   -- pip3 install jupyter
 * Jupyter (>= 4.x) 
 * cl-asdf
 * cl-hunchentoot


Details:
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

