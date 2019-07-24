#!/bin/bash

# Remove kernel
jupyter kernelspec remove jfricas
jupyter kernelspec list

# Search for Python user site 
PYUSRSITE=$(python3 -m site --user-site)

# Remove kernel and webspad
rm -v $PYUSRSITE/jfricas/fricaskernel.py
rm -v $PYUSRSITE/jfricas/webspad.lisp
rmdir -v --ignore-fail-on-non-empty $PYUSRSITE/jfricas

echo "jfricas should be completely removed."
