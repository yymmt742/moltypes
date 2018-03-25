TARGET1= msd
TARGET2= rcorr

LDFLAGS += -L$(LIBPATH) -lmoltypes -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm

ifeq (${FC},ifort)
	FFLAGS  += -O3 -xHOST -free -qopenmp
endif

.SUFFIXES:.o .f
	$(FC) -c $< ${FFLAGS} ${LDFLAGS}

all: $(TARGET1) $(TARGET2)

$(TARGET1): $(TARGET1).o
	${FC} -o $(TARGET1) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET1) $(BINPATH)

$(TARGET2): $(TARGET2).o
	${FC} -o $(TARGET2) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET2) $(BINPATH)

.PHONY: clean
clean:
	$(RM) *.o
