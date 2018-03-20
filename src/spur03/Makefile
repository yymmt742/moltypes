include sourcelist

TARGET=libspur03

SHARED=$(TARGET).so
STATIC=$(TARGET).a

OBJ_FILES = $(FSOURCES:.f=.o)
MOD_FILES = $(FSOURCES:.f=.mod)

LDFLAGS += -L$(LIBPATH) -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
FFLAGS  += -shared -fpic -O3 -xHOST -free -qopenmp -mkl -I$(INCPATH)

all:$(SHARED) $(STATIC)

$(SHARED):$(OBJ_FILES)
	$(FC) -o $(SHARED) $(OBJ_FILES) $(LDFLAGS) $(FFLAGS)
	@cp $(SHARED) $(LIBPATH)
	@cp *mod $(INCPATH)

$(STATIC):$(OBJ_FILES)
	ar -r -o $(STATIC) $(OBJ_FILES)
	@cp $(STATIC) $(LIBPATH)

.SUFFIXES:.o .f
	$(FC) -c $(FFLAGS) $(LDFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(OBJ_FILES) $(MOD_FILES) $(SHARED) $(STATIC)
