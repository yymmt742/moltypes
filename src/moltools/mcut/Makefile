mcut=mcut
pcut=pcut

LDFLAGS += -L$(LIBPATH) -lmoltypes -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
FFLAGS += -O3 -xHOST -free -traceback

all: $(mcut) $(pcut)

$(pcut): $(pcut).o
	${FC} -o $(pcut) $< ${FFLAGS} ${LDFLAGS}
	cp $(pcut) $(BINPATH)

$(mcut): $(mcut).o
	${FC} -o $(mcut) $< ${FFLAGS} ${LDFLAGS}
	cp $(mcut) $(BINPATH)

.SUFFIXES: .o .f
	$(FC) -c $(FFLAGS) $<

.PHONY: clean
clean:
	$(RM) *.o
