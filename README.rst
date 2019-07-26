=====================
FriCAS Jupyter Kernel 
=====================

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


Quick Install :rocket:
-------------
Assuming the prerequisites are satisfied:
::

	pip3 install jupyter
	pip3 install jfricas
        python3 -m jfricas.install_kernel


**Detailed** installation instructions may be found in `INSTALL`_.


Install Development Version :construction:
---------------------------
To install from this repository:
::

	pip3 install jupyter
	git clone https://github.com/fricas/jfricas.git
	cd jfricas
	pip3 install .
	

Uninstall
---------
::

	pip3 uninstall jfricas
	jupyter kernelspec remove jfricas


References
----------

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
.. _INSTALL: INSTALL

:Authors:
    Ralf Hemmecke,  
    Kurt Pagani
    
    (and sundry other good-natured folks)

:Version: 0.2 of 2019/07/25

