from setuptools import setup

def readme():
    with open('README.rst') as f:
        return f.read()

kernel_sdir = 'jfricas/kspec'
kernel_name = 'jfricas'
kernel_version = '0.2.5'
prefix = None

setup(name=kernel_name,
      version=kernel_version,
      description='FriCAS Jupyter Kernel.',
      long_description='FriCAS Jupyter Kernel (HTTP, X11).',
      classifiers=[
        'Development Status :: 4 - Beta',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python :: 3.5',
        'Operating System :: POSIX :: Linux',  
        'Topic :: Scientific/Engineering :: Mathematics',
      ],
      keywords='fricas, jupyter, computer-algebra',
      url='http://github.com/nilqed/jfricas.pip',
      author='Kurt Pagani',
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
import sys
user=False
prefix=sys.prefix
if prefix == '/usr':
  user=True
  prefix=None
# else we have a venv

  
KSM().install_kernel_spec(kernel_sdir, kernel_name, user=user, replace=True, prefix=prefix)
