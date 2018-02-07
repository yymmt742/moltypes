CONFIG_FILE = config_netcdf.h
include $(CONFIG_FILE)

all: netcdf_c netcdf_cxx netcdf_fortran

netcdf_c:
	(cd netcdf-c-4.5.0 && CC=${MPICC} ./configure --enable-netcdf-4 --with-hdf5=${MOLTYPES_ROOT} --with-zlib=${MOLTYPES_ROOT} --with-szlib=${MOLTYPES_ROOT} --enable-parallel-tests --prefix=${MOLTYPES_ROOT} && make && make check && make install)

netcdf_cxx:
	(cd netcdf-cxx4-4.3.0 && ./configure --enable-netcdf-4 --with-hdf5=${MOLTYPES_ROOT} --with-zlib=${MOLTYPES_ROOT} --with-szlib=${MOLTYPES_ROOT} --prefix=${MOLTYPES_ROOT} && make && make check && make install)

netcdf_fortran:
	(cd netcdf-fortran-4.4.4 && CC=${MPICC} FC=${MPIFC} F77=${MPIFC} F90=${MPIFC} ./configure --enable-netcdf-4 --with-hdf5=${MOLTYPES_ROOT} --with-zlib=${MOLTYPES_ROOT} --with-szlib=${MOLTYPES_ROOT} --prefix=${MOLTYPES_ROOT} --enable-parallel-tests && make && make install)

.PHONY: clean
clean:
	(cd netcdf-c-4.5.0 && make clean)
	(cd netcdf-cxx4-4.3.0 && make clean)
	(cd netcdf-fortran-4.4.4 && make clean)
