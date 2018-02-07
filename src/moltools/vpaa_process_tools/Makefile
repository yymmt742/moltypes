TARGET1= vpaa_trjcut
TARGET2= vpaa_local
TARGET3= vpaa_rmsd

LDFLAGS += -L$(LIBPATH) -lmoltypes -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm

ifeq (${FC},gfortran)
	FFLAGS += -O3 -ffree-form -fimplicit-none -fbounds-check -fopenmp
endif

ifeq (${FC},ifort)
	FFLAGS  += -O3 -xHOST -free -qopenmp
endif

.SUFFIXES:.o .f
	$(FC) -c $< ${FFLAGS} ${LDFLAGS}

all: $(TARGET1) $(TARGET2) $(TARGET3)

$(TARGET1): $(TARGET1).o
	${FC} -o $(TARGET1) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET1) $(BINPATH)

$(TARGET2): $(TARGET2).o
	${FC} -o $(TARGET2) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET2) $(BINPATH)

$(TARGET3): $(TARGET3).o
	${FC} -o $(TARGET3) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET3) $(BINPATH)

.PHONY: clean
clean:
	$(RM) $(TARGET1).o $(TARGET2).o $(TARGET3).o
