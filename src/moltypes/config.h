LIBPATH=$(MOLTYPES_ROOT)/lib
INCPATH=$(MOLTYPES_ROOT)/include
BINPATH=$(MOLTYPES_ROOT)/bin

OBJ_FILES = $(FSOURCES:.f=.o)
MOD_FILES = $(FSOURCES:.f=.mod)

LDFLAGS += -L$(LIBPATH) -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lcurl -lm

ifeq ($(CC),icc)
	CFLAGS += -shared -fpic -O3 -xHOST
endif

ifeq ($(FC),ifort)
	FFLAGS += -shared -fpic -O3 -xHOST -free -qopenmp -mkl -I$(INCPATH)
endif
ifeq ($(CC),gcc)
	CFLAGS += -shared -O3 -fPIC -fopenmp
endif

ifeq ($(CC),icc)
	CFLAGS += -shared -fpic -O3 -xHOST
endif
