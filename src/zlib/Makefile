CONFIG_FILE = config.h
include $(CONFIG_FILE)

all: zlib

zlib:
	(cd zlib-1.2.10 && ./configure --prefix=${MOLTYPES_ROOT} && make && make check && make install)

.PHONY: clean
clean:
	(cd zlib-1.2.10 && make clean)
