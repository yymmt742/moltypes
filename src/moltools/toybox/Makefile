TARGET1 = thsh3d
TARGET2 = textharf
TARGET3 = addh
TARGET4 = msd
TARGET5 = vcorr
TARGET6 = autosdf
TARGET7 = dimgen
TARGET8 = ilocal

LDFLAGS += -L$(LIBPATH) -lmoltypes -lspur03 -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lsz -lcurl -lm
FFLAGS += -O3 -xHOST -free

all: TARGET7
#all: TARGET1 TARGET2 TARGET3 TARGET4 TARGET5 TARGET6 TARGET7 TARGET8

.SUFFIXES:.o .f
	$(FC) -c $< ${FFLAGS} ${LDFLAGS}

TARGET1: $(TARGET1).o
	${FC} -o $(TARGET1) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET1) $(BINPATH)

TARGET2: $(TARGET2).o
	${FC} -o $(TARGET2) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET2) $(BINPATH)

TARGET3: $(TARGET3).o
	${FC} -o $(TARGET3) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET3) $(BINPATH)

TARGET4: $(TARGET4).o
	${FC} -o $(TARGET4) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET4) $(BINPATH)

TARGET5: $(TARGET5).o
	${FC} -o $(TARGET5) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET5) $(BINPATH)

TARGET6: $(TARGET6).o
	${FC} -o $(TARGET6) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET6) $(BINPATH)

TARGET7: $(TARGET7).o
	${FC} -o $(TARGET7) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET7) $(BINPATH)

TARGET8: $(TARGET8).o
	${FC} -o $(TARGET8) $< ${FFLAGS} ${LDFLAGS}
	@cp $(TARGET8) $(BINPATH)

.PHONY: clean
clean:
	$(RM) $(TARGET1).o $(TARGET2).o $(TARGET3).o $(TARGET4).o $(TARGET5).o $(TARGET6).o $(TARGET7).o $(TARGET8).o
