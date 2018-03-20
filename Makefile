CONFIG_FILE = config.h
include $(CONFIG_FILE)

all: setup

setup: spur03 moltypes moltools

netcdf:
#	(cd src/zlib && make)
#	(cd src/szip && make)
#	(cd src/hdf5 && make)
	(cd src/netcdf && make)

spur03:
	(cd src/spur03 && make)

moltypes:
	(cd src/moltypes && make)

moltools:
	(cd src/moltools && make)

test:
	(cd src/test && make && make test)

.PHONY: clean
clean:
	(cd src/spur03 && make clean)
	(cd src/moltypes && make clean)
	(cd src/moltools && make clean)
