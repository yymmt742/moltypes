export CFLAGS=-O3 -xHost -ip -no-prec-div -I${INCPATH} -fPIC
export CPPFLAGS=-O3 -xHost -ip -no-prec-div -I${INCPATH} -fPIC
export FCFLAGS=-O3 -xHost -ip -no-prec-div -I${INCPATH} -fPIC
export F90FLAGS=-O3 -xHost -ip -no-prec-div -I${INCPATH} -fPIC
export LDFLAGS=-L${LIBPATH}
#export LIBS=-lhdf5_hl -lhdf5 -lz -lsz -lcurl
