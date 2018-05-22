include config.h

all: setup

setup:
	(cd mcut && make)
	(cd vpaa_process_tools && make)
	(cd lqtools && make)
#	(cd toybox && make)

.PHONY: clean
clean:
	(cd mcut && make clean)
	(cd vpaa_process_tools && make clean)
	(cd lqtools && make clean)
#	(cd toybox && make clean)
