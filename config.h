export MOLTYPES_ROOT=$(PWD)

export CC=icc
export CXX=icpc
export FC=ifort
export F77=ifort
export CPP=icc -E
export CXXCPP=icc -E

export MPICC=mpiicc
export MPICXX=mpiicpc
export MPIFC=mpiifort
export MPIF77=mpiifort

export LIBPATH=$(MOLTYPES_ROOT)/lib
export INCPATH=$(MOLTYPES_ROOT)/include
export BINPATH=$(MOLTYPES_ROOT)/bin
