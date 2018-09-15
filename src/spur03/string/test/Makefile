target=test_spur_string

LDFLAGS += -L$(LIBPATH) -lspur03
FFLAGS  += -free -warn -traceback -axAVX -I$(INCPATH)

all: test

test:
	$(FC) -c $(target).f $(FFLAGS)
	${FC} -o $(target) $(target).o $(LDFLAGS)
	valgrind -v --leak-check=full ./$(target)

.SUFFIXES: .o .f

.PHONY: clean
clean:
	$(RM) *.o
