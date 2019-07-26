#!/bin/bash

# https://test.pypi.org/manage/project/jfricas/releases/
# pip3  install -i https://test.pypi.org/simple/ jfricas==0.2.7

python3 -m pip install --user --upgrade setuptools wheel
python3 setup.py sdist bdist_wheel
python3 -m pip install --user --upgrade twine

python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*
