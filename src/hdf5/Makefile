CONFIG_FILE = config_hdf5.h
include $(CONFIG_FILE)

all: hdf5

hdf5:
	(cd hdf5-1.10.1 && CC=${MPICC} ./configure --with-zlib=${MOLTYPES_ROOT} --enable-parallel --prefix=${MOLTYPES_ROOT} && make && make install)

.PHONY: clean
clean:
	(cd hdf5-1.10.1 && make clean)
