export CFLAGS= -O3 -xHost -ip -no-prec-div -static-intel -Difort -fPIC
export CXXFLAGS=-O3 -xHost -ip -no-prec-div -static-intel -fPIC
export FFLAGS=-O3 -xHost -ip -no-prec-div -static-intel -fPIC
export CPPFLAGS=-I${INCPATH} -fPIC
export LDFLAGS=-L${LIBPATH}
export LIBS=-lhdf5_hl -lhdf5 -lz -lsz -lcurl
