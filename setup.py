from setuptools import setup

def readme():
    with open('README.rst') as f:
        return f.read()

kernel_srcdir = 'jfricas/kspec'
kernel_name = 'jfricas'
kernel_version = '2.0.0'
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
      author_email='jfricas@hemmecke.org',
      license='BSD',
      packages=['jfricas'],
      package_data={'jfricas': ['webspad.lisp'],},
      python_requires='>=3.5',
      install_requires=[
          'requests',
          'jupyter',
      ],
      include_package_data=True,
      zip_safe=False)

# Additionally, we must install the kernelspec of jfricas for jupyter.
kernel_json = {
    "argv": ["python3", "-m","jfricas.fricaskernel","-f","{connection_file}"],
    "display_name": "FriCAS",
    "language": "spad",
}

def install_kernel_spec():
    from jupyter_client.kernelspec import KernelSpecManager as KSM
    from IPython.utils.tempdir import TemporaryDirectory
    import json
    import sys
    import os
    import getpass
    # Default is a install into $HOME/.local.
    user=True
    prefix=None
    if (getpass.getuser()=='root') or (sys.prefix != '/usr'):
        user=False
        prefix=sys.prefix

    with TemporaryDirectory() as td:
        os.chmod(td, 0o755) # Starts off as 700, not user readable
        with open(os.path.join(td, 'kernel.json'), 'w') as f:
            json.dump(kernel_json, f, sort_keys=True)
        KSM().install_kernel_spec(td, kernel_name, replace=True,
                                  user=user, prefix=prefix)

install_kernel_spec()
