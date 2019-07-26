import json
import os
import sys

from jupyter_client.kernelspec import KernelSpecManager as KSM
from IPython.utils.tempdir import TemporaryDirectory

kernel_json = {
    "argv": ["python3", "-m","jfricas.fricaskernel","-f","{connection_file}"],
    "display_name": "FriCAS",
    "language": "spad",
}

kernel_name = "jfricas"

def install_my_kernel_spec(user=True, prefix=None):
    with TemporaryDirectory() as td:
        os.chmod(td, 0o755) # Starts off as 700, not user readable
        with open(os.path.join(td, 'kernel.json'), 'w') as f:
            json.dump(kernel_json, f, sort_keys=True)
        # TODO: Copy any resources
        print('Installing Jupyter kernel spec')
        KSM().install_kernel_spec(td, kernel_name, user=user, replace=True, prefix=prefix)

user=False
prefix=sys.prefix
if prefix == '/usr':
  user=True
  prefix=None
# else we have a venv

install_my_kernel_spec(user=user, prefix=prefix)
