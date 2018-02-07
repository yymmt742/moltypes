export CFLAGS=-O3 -xHost -ip -no-prec-div -static-intel -fPIC
export CXXFLAGS=-O3 -xHost -ip -no-prec-div -static-intel -fPIC
export FFLAGS=-O3 -xHost -ip -no-prec-div -static-intel -fPIC
export CPPFLAGS=-I${INCPATH}
export LDFLAGS=-L${LIBPATH}
export MPICC=mpiicc
