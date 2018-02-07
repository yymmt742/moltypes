CONFIG_FILE = config.h
include $(CONFIG_FILE)

all: szip

szip:
	(cd szip-2.1 && ./configure --prefix=${MOLTYPES_ROOT} && make && make check && make install)

.PHONY: clean
clean:
	(cd szip-2.1 && make clean)
