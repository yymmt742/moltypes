TARGET1=xhamil
#TARGET2=x2prm

LDFLAGS += -L$(LIBPATH) -lmoltypes -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
FFLAGS += -O3 -xHOST -free -qopenmp -mkl

all: TARGET1

TARGET1: $(TARGET1).o
	${FC} -o $(TARGET1) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET1) $(BINPATH)

TARGET2: $(TARGET2).o
	${FC} -o $(TARGET2) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET2) $(BINPATH)

.SUFFIXES:.o .f
	$(FC) -c $(FFLAGS) $<

.PHONY: clean
clean:
	rm *.o *.mod 
