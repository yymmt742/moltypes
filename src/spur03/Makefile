include sourcelist

TARGET=libspur03

SHARED=$(TARGET).so
STATIC=$(TARGET).a

OBJ_FILES = $(CSOURCES:.c=.o) $(FSOURCES:.f=.o)
MOD_FILES = $(FSOURCES:.f=.mod)

LDFLAGS += -L$(LIBPATH) -lfnv -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
CFLAGS  += -shared -fpic -O3 -xHOST -I$(INCPATH)
FFLAGS  += -shared -fpic -O3 -xHOST -free -qopenmp -mkl -I$(INCPATH)

all:$(SHARED) $(STATIC)

$(SHARED):$(OBJ_FILES)
	$(FC) -o $(SHARED) $(OBJ_FILES) $(LDFLAGS) $(FFLAGS)
	@mv $(SHARED) $(LIBPATH) &> /dev/null ; :
	@mv *mod $(INCPATH) &> /dev/null ; :

$(STATIC):$(OBJ_FILES)
	ar -r -o $(STATIC) $(OBJ_FILES)
	@mv $(STATIC) $(LIBPATH) &> /dev/null ; :

get_hash.o: get_hash.o
	$(CC) -c $(CFLAGS) -L$(LIBPATH) $<

.SUFFIXES:.o .f
	$(FC) -c $(FFLAGS) $(LDFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(OBJ_FILES) $(MOD_FILES) $(SHARED) $(STATIC)
