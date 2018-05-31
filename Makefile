CONFIG_FILE = config.h
include $(CONFIG_FILE)

all: setup

setup: spur03 moltypes

zlib:
	(cd src/zlib && make)

szip:
	(cd src/szip && make)

hdf5:
	(cd src/hdf5 && make)

netcdf:
	(cd src/netcdf && make)

fnv:
	(cd src/fnv && make && make check && make install)

spur03:
	(cd src/spur03 && make)

moltypes:
	(cd src/moltypes && make)

test:
	(cd src/test && make && make test)

.PHONY: clean
clean:
	(cd src/spur03 && make clean)
	(cd src/moltypes && make clean)
