PROGRAM package
  USE mpi
#ifdef MPI_WRAPPED
  USE raw_wrapper
#endif
  IMPLICIT NONE

  INTEGER :: ierr, rank, status(MPI_STATUS_SIZE)
  INTEGER :: i = 0
  INTEGER :: dt_column_tmp, dt_column, typesize
  INTEGER, PARAMETER :: N = 10
  INTEGER(kind=MPI_ADDRESS_KIND) :: extent, lb = 1
  REAL :: values(N, N) = (0)

#ifdef MPI_WRAPPED
  CALL rwi_set_enabled(1)
#endif

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Type_vector(N, 1, N, MPI_REAL, dt_column_tmp, ierr)
  CALL MPI_Type_size(MPI_REAL, typesize, ierr)
  extent = typesize
  CALL MPI_Type_create_resized(dt_column_tmp, lb, extent, dt_column, ierr)
  CALL MPI_Type_commit(dt_column, ierr)

  IF (rank .EQ. 0) THEN
    values = TRANSPOSE(RESHAPE((/(i, i=1,100, 1)/), (/N,N/)))
    CALL MPI_Send(values, 1, dt_column, 1, 0, MPI_COMM_WORLD, ierr)
    PRINT *, values
  ELSE IF (rank .EQ. 1) THEN
    CALL MPI_Recv(values, 1, dt_column, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
    PRINT *, values
  END IF

  CALL MPI_Type_free(dt_column, ierr)

  CALL MPI_Finalize(ierr)
END PROGRAM package
