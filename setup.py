from setuptools import setup

def readme():
    with open('README.rst') as f:
        return f.read()

kernel_sdir = 'jfricas/kspec'
kernel_name = 'jfricas'
kernel_version = '0.2.9'
prefix = None
ldescr = readme()

setup(name=kernel_name,
      version=kernel_version,
      description='FriCAS Jupyter Kernel.',
      long_description=ldescr,
      long_description_content_type='text/x-rst',
      classifiers=[
        'Development Status :: 4 - Beta',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python :: 3.5',
        'Operating System :: POSIX :: Linux',  
        'Topic :: Scientific/Engineering :: Mathematics',
      ],
      keywords='fricas, jupyter, computer_algebra',
      url='http://github.com/fricas/jfricas',
      author='Kurt Pagani, Ralf Hemmecke',
      author_email='nilqed@gmail.com',
      license='BSD',
      packages=['jfricas'],
      install_requires=[
          'requests',
          'jupyter',
      ],
      include_package_data=True,
      zip_safe=False)


from jupyter_client.kernelspec import KernelSpecManager as KSM
from IPython.utils.tempdir import TemporaryDirectory
import json
import sys
import os

kernel_json = {
    "argv": ["python3", "-m","jfricas.fricaskernel","-f","{connection_file}"],
    "display_name": "FriCAS",
    "language": "spad",
}

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

#KSM().install_kernel_spec(kernel_sdir, kernel_name, user=user, replace=True, prefix=prefix)

install_my_kernel_spec(user=user, prefix=prefix)
