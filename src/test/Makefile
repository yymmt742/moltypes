target=spur03_test

LDFLAGS += -L$(LIBPATH) -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
FFLAGS  += -O3 -xHOST -free -mkl -I$(INCPATH)

all: $(target)

$(target): $(target).o
	${FC} -o $(target) $< $(LDFLAGS)

.SUFFIXES: .o .f
	$(FC) -c $< $(FFLAGS)

.PHONY: test
test:
	./$(target)

.PHONY: clean
clean:
	$(RM) *.o
