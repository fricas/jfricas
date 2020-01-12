#!/bin/bash

# Install kernel.json
jupyter kernelspec install ./jfricas/kspec --name='jfricas' --user
jupyter kernelspec list

# Search for Python user site
PYUSRSITE=$(python3 -m site --user-site)

# Copy kernel and webspad
mkdir -p -v $PYUSRSITE/jfricas
cp -v ./jfricas/fricaskernel.py $PYUSRSITE/jfricas
cp -v ./jfricas/webspad.lisp    $PYUSRSITE/jfricas

echo "Test: $ jupyter notebook, then  New->FriCAS"
