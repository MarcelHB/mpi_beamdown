MPIFORT = mpifort
FCFLAGS = -ggdb -cpp -I../
FC_WRAP_FLAGS = -DMPI_WRAPPED
LDFLAGS = ../rraw_wrapper_f90.o -Wl,-Bstatic -L../ -lrawwrapperi -Wl,-Bdynamic -ldl

mpi_wrapper:
	cd .. && \
	$(MAKE) interface && \
	cd tests

mpi_wrapper_f90: mpi_wrapper
	cd ../raw_wrapper && \
	$(MAKE) raw_wrapper_f90 && \
	cd ../tests

ping_pong: ping_pong.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $< -o bin/$@
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@_wrapped

ping_pong_i: ping_pong_i.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $< -o bin/$@
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@_wrapped

circus_allreduce: circus_allreduce.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $< -o bin/$@
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@_wrapped

packing: packing.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $< -o bin/$@
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@_wrapped

capture_and_replay: capture_and_replay.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@

customize_replay: customize_replay.f90 mpi_wrapper_f90
	mkdir -p bin
	$(MPIFORT) $(FCFLAGS) $(FC_WRAP_FLAGS) $< $(LDFLAGS) -o bin/$@

PROGRAMS = ping_pong ping_pong_i circus_allreduce packing capture_and_replay

clean:
	rm -f bin/*
	rm -f *.o

all: $(PROGRAMS)

default: all

