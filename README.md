# Fortran High-Level Synthesis
Fortran-HLS enables Fortran on AMD Xilinx devices through the integration of LLVM Flang in the Vitis ecosystem, thus enabling the use of all its features, such as simulation, profiling, and generation of hardware bitsreams. The driver of Fortran-HLS is named `f++`.

## Dependencies
`f++` depends on the AMD Xilinx's custom LLVM v7 for adapting the generated LLVM IR to the format expected by the Vitis's backend. It also depends on LLVM v16 for `flang-new` (this has not be tested with newer versions but it should work out-of-the-box or with minor adjustments). Both dependencies are provided in this repository as submodules.

You will need Python 3.7+ to run `f++` and Xilinx Vitis 2021.2.

## Installation
To install `f++` in your machine run `pip .` or `pip -e .` for developer mode. Alternative, the script `setup_virtualenv.sh` is provided that will create a virtual environment and setup the LLVM passes that are part of the tool for you. This is the recommended way of installing the tool. Check out this script if you want to install it manually.

## Usage
The following environment variables have to be defined before using `f++`:
* `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
* `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
* `FXX_FLANG_PATH`: path to the Flang binary directory.

The LLVM passes for pragma lowering must be compiled before using `f++`.

## Tests
This repository includes some tests that demonstrate the functionality of the tool that can be read by the user to develop their own applications in Fortran.

## Publication
Fortran-HLS was presented at FPL'23 with the following publication:

*Fortran High-Level Synthesis: Reducing the barriers to accelerating HPC codes on FPGAs. Rodriguez-Canal, G., Brown, N., Dykes, T., Jones, J., Haus, U. In the 33rd International Conference on Field-Programmable Logic and Applications.*

Please, consider citing us if you use Fortran-HLS in your projects.

## Contact
Should you have any questions or requests about `f++` reach out at gabriel.rodcanal@ed.ac.uk.