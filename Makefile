MPICC = mpicc
CFLAGS = -O2 -ggdb -fopenmp -fPIC
LDFLAGS = -fPIC
CC = gcc
LD = ld
FC = gfortran
FCFLAGS = -O2

.PHONY: clean

default: raw_wrapper

sha256: sha256.c
	$(CC) $(CFLAGS) -c $< $(LDFLAGS)

data: data.c
	$(MPICC) $(CFLAGS) -c $< $(LDFLAGS)

raw_data: raw_data.c data
	$(MPICC) $(CFLAGS) -c $< $(LDFLAGS)

f90gen_data: f90gen_data.c data
	$(MPICC) $(CFLAGS) -c $< $(LDFLAGS)
	$(MPICC) f90gen_data.o data.o sha256.o $(LDFLAGS) -shared -o f90gen_data.so

raw_wrapper: raw_wrapper.c sha256 raw_data
	$(MPICC) $(CFLAGS) -c $<
	$(MPICC) raw_wrapper.o data.o sha256.o $(LDFLAGS) -shared -o librawwrapper.so

interface: interface.c sha256 data raw_wrapper
	$(CC) $(CFLAGS) -c $<
	$(LD) -r interface.o sha256.o data.o raw_wrapper.o raw_data.o -o librawwrapperi.o
	ar rcs librawwrapperi.a librawwrapperi.o

raw_wrapper_f90: raw_wrapper.f90
	$(FC) $(FCFLAGS) -c $< -o raw_wrapper_f90.o

clean:
	rm *.o
