from setuptools import setup

setup(name='fortran_hls',
      version='0.1',
      description='LLVM and regex based tool that enables Fortran on AMD Xilinx FPGAs.',
      author='Gabriel Rodriguez-Canal',
      author_email='gabriel.rodcanal@ed.ac.uk',
      license='Apache 2.0',
      packages=['fortran_hls'],
      entry_points = {
          'console_scripts': ['fxx=fortran_hls.command_line:main'],
      },
      zip_safe=False)
