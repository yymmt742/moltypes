CONFIG_FILE = config_netcdf.h
include $(CONFIG_FILE)

all: netcdf_c netcdf_fortran

netcdf_c:
	(cd netcdf-4.6.1 && ./configure --with-hdf5=${MOLTYPES_ROOT} --with-zlib=${MOLTYPES_ROOT} --with-szlib=${MOLTYPES_ROOT} --prefix=${MOLTYPES_ROOT} && make && make check && make install)

netcdf_fortran:
	(cd netcdf-fortran-4.4.4 && ./configure --with-hdf5=${MOLTYPES_ROOT} --with-zlib=${MOLTYPES_ROOT} --with-szlib=${MOLTYPES_ROOT} --prefix=${MOLTYPES_ROOT}  && make && make install)

.PHONY: clean
clean:
	(cd netcdf-c-4.5.0 && make clean)
	(cd netcdf-fortran-4.4.4 && make clean)
