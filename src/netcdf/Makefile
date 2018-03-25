CONFIG_FILE = config_netcdf.h
include $(CONFIG_FILE)

all: netcdf_c netcdf_cxx netcdf_fortran

netcdf_c:
	(cd netcdf-c-4.5.0 && ./configure --disable-netcdf-4 --prefix=${MOLTYPES_ROOT} && make && make check && make install)

netcdf_cxx:
	(cd netcdf-cxx4-4.3.0 && ./configure --prefix=${MOLTYPES_ROOT} && make && make check && make install)

netcdf_fortran:
	(cd netcdf-fortran-4.4.4 && ./configure --prefix=${MOLTYPES_ROOT} && make && make install)

.PHONY: clean
clean:
	(cd netcdf-c-4.5.0 && make clean)
	(cd netcdf-cxx4-4.3.0 && make clean)
	(cd netcdf-fortran-4.4.4 && make clean)
