TARGET=libmoltypes

SHARED=$(TARGET).so
STATIC=$(TARGET).a

include sourcelist

OBJ_FILES = $(FSOURCES:.f=.o)
MOD_FILES = $(FSOURCES:.f=.mod)

LDFLAGS += -L$(LIBPATH) -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm

ifeq ($(CC),icc)
	CFLAGS += -shared -fpic -O3 -xHOST
endif

ifeq ($(FC),ifort)
	FFLAGS += -shared -fpic -O3 -xHOST -free -qopenmp -mkl -I$(INCPATH)
endif

all:$(SHARED) $(STATIC)

.SUFFIXES:.o .f
	$(FC) -c $(FFLAGS) $(LDFLAGS) $<

$(SHARED):$(OBJ_FILES)
	$(FC) -o $(SHARED) $(OBJ_FILES) $(LDFLAGS) $(FFLAGS)
	@cp $(SHARED) $(LIBPATH)
	@cp $(MOD_FILES) $(INCPATH)

$(STATIC):$(OBJ_FILES)
	ar -r -o $(STATIC) $(OBJ_FILES)
	@cp $(STATIC) $(LIBPATH)

.PHONY: test
test:
ifeq ($(FC),gfortran)
	$(FC) -o $(TEST_TARGET) $(TEST_FILES) $(LDFLAGS) -lmoltypes $(FFLAGS)
endif
ifeq ($(FC),ifort)
	$(FC) -o $(TEST_TARGET) $(TEST_FILES) $(LDFLAGS) -lmoltypes $(FFLAGS)
endif
	valgrind ./$(TEST_TARGET) 2>&1 | grep -A 1000 -n HEAP
	@cp $(TEST_TARGET) $(TEST_DIR)

.PHONY: clean
clean:
	$(RM) $(OBJ_FILES) $(MOD_FILES) $(SHARED) $(STATIC)
