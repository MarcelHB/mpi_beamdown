PROGRAM capture_and_replay
  USE mpi
  USE raw_wrapper
  USE, INTRINSIC :: ISO_C_BINDING

  TYPE(C_FUNPTR) :: fptr

  INTEGER :: ierr, rank, value = 2
  INTEGER :: status(MPI_STATUS_SIZE)

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL rwi_set_enabled(1)

  IF (rank .eq. 0) THEN
    value = 1
    CALL MPI_Send(value, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, ierr)
  ELSE IF (rank .eq. 1) THEN
    CALL MPI_Recv(value, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
    PRINT *, ' Native result: ', value
  END IF

  fptr = C_FUNLOC(everything_one)

  CALL rwi_set_replay_filter(fptr)

  IF (rank .eq. 1) THEN
    CALL rwi_reset_counter
    CALL rwi_set_replay_enabled(1, 1)
    CALL MPI_Recv(value, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
    PRINT *, ' Replayed and filtered result: ', value
  END IF

  CALL MPI_Finalize(ierr)
CONTAINS
  SUBROUTINE everything_one(ptr, size) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    TYPE(C_PTR), VALUE :: ptr
    INTEGER(KIND=C_SIZE_T), VALUE :: size
    BYTE, POINTER :: f_ptr(:)
    INTEGER :: i, j = 0

    CALL C_F_POINTER(ptr, f_ptr, [size])

    FORALL(i = 1:size) f_ptr(i) = IEOR(f_ptr(i), 1)
  END SUBROUTINE
END PROGRAM capture_and_replay


