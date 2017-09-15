# MPI beamdown: a prototype MPI wrapper for capture and replay

## Motivation

Think of writing unit tests of Fortran `SUBROUTINE`s containing
MPI-calls. To facilitate offline runs related to MPI, this wrapper is a
prototype to both capture MPI-data by rank and replay MPI-data for a
single and specified rank -- the beam-down of a distributed MPI run
down to one participant.

Is has been evaluated to work well with
[FTG](https://github.com/fortesg/fortrantestgenerator), a Fortran unit
test generator tool.

This tool was part of a mandatory project at University of Hamburg.

## Limitations

So far, capture and replay is limited to a minimal subset of MPI v3.1
provided calls, mostly those required to run certain [ICON](https://www.mpimet.mpg.de/en/science/models/icon.html)
subroutines.

Capturing methods include: `MPI_Recv`, `MPI_Irecv`, `MPI_Reduce`,
`MPI_Allreduce`.

In addition, replay-compatibility exists for: `MPI_Send`, `MPI_Isend`,
`MPI_Wait`.

Return values of the following reflection methods can be faked to a
certain degree: `MPI_Comm_rank`, `MPI_Comm_size`.

## Requirements

* An MPI v3.1 compliant implementation with default compiler wrappers
  (`mpifort`, `mpicc`).
* GNU C/Fortran v6.0 or newer.

## Setup

1. `make interface` to create `libwrawwrapperi.o`.
2. C: `#include "raw_wrapper.h"` and pick the `rwi_` calls.
3. Fortran: Inline the `INTERFACE` subset of `raw_wrapper.f90` and pick
   your calls.
4. Statically link it into your application, e. g. by flags
   `-Wl,-Bstatic  -L$(PATH_TO_LIBRAWWRAPPERI) -lrawwrapperi
   -Wl,-Bdynamic`
5. Run by `mpiexec` and the desired number of processes.

## Use

For performance reasons, the wrapper is currently disabled by default.
After successfully including the language's interface, the following
calls are available:

* `rwi_reset_count ()`: Capturing/replay is being reset. Useful for
  in-application switching from capturing to replay, to replay from the
  first captured byte.
* `rwi_set_comm_size(int s)`: The value to be returned by
  `MPI_Comm_size`.
* `rwi_set_enabled (int s)`: If `s` is non-zero, the current wrapper mode
  turns to be enabled.
* `rwi_set_replay_data (int8_t *buf, size_t s)`: Provide `s` bytes of `buf`
  customized MPI data inline, in replay mode. This data is used over
  recordings.
* `rwi_set_replay_filter (void (*f)(void *buf, const size_t s)`: Register
  a function `f`, accepting `buf` of length `s` of packed MPI-data to
  apply manipulation in replay mode.
* `rwi_set_replay_enabled (int s, int r)`: If `s` is non-zero, the
  wrapper will switch to replay mode for rank `r`, otherwise capturing.

Binary dumps of received data are stored inside an `out/` directory
relative to the application's launch path.

## Backends

By default, all relevant data is bin-dumped onto the disk. The tool
allows other backends for different purposes, such as generating
Fortran-replay code. The calls required to implement such a backend can
be found at the bottom `raw_wrapper.h`.

In order to run such a backend, create a shared object and set its path
to the env. variable `RAW_WRAPPER_BACKEND`.

`$ make f90gen_data` as an example.

## Example

### Capturing (Fortran)

Prepend:

```fortran
CALL rwi_set_enabled(1)
```

to where to start capturing from.

### Replay (Fortran)

Prepend:

```fortran
CALL rwi_set_enabled(1)
CALL rwi_set_replay_enabled(1, 0)
CALL rwi_set_comm_size(4)
```

to where to start replaying from. In this case we pretend to be rank 0,
facing an MPI setup of 4 processes in total.

### Tests

For more samples, please dig into `tests/`.

## License

The source code is subject to the MIT license.

`sha256.c` and `sha256.h` are released into public domain by Brand
Conte.

## References

> C. Hovy and J. Kunkel, "Towards Automatic and Flexible Unit Test Generation for Legacy HPC Code," *2016 Fourth International Workshop on Software Engineering for High Performance Computing in Computational Science and Engineering (SE-HPCCSE)*, Salt Lake City, UT, 2016, pp. 1-8.
> [DOI: 10.1109/SE-HPCCSE.2016.005](http://dx.doi.org/10.1109/SE-HPCCSE.2016.005) ([Download PDF](http://conferences.computer.org/sehpccse/2016/papers/5224a001.pdf))
