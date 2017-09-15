PROGRAM customize_replay
  USE mpi
  USE raw_wrapper
  IMPLICIT NONE

  INTEGER :: ierr, rank
  INTEGER :: rounds = 3, i = 0, value = 0
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER(1) :: first_row(12) = (/1,0,0,0,2,0,0,0,3,0,0,0/), second_row(12) = (/4,0,0,0,5,0,0,0,6,0,0,0/)

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL rwi_set_enabled(1)

  DO WHILE (i .LT. rounds)
    IF (rank .EQ. 0) THEN
      value = 1
      CALL MPI_Send(value, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, ierr)
    ELSE IF (rank .EQ. 1) THEN
      CALL MPI_Recv(value, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
      PRINT *, value
    END IF
    i = i + 1
  END DO

  i = 0

  IF (rank .EQ. 1) THEN
    CALL rwi_reset_counter()
    CALL rwi_set_replay_enabled(1, 1)
    CALL rwi_set_replay_data_vb(first_row)

    DO WHILE (i .LT. rounds)
      CALL MPI_Recv(value, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
      i = i + 1
      PRINT *, value
    END DO

    i = 0
    CALL rwi_set_replay_data_vb(second_row)

    DO WHILE (i .LT. rounds)
      CALL MPI_Recv(value, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
      i = i + 1
      PRINT *, value
    END DO
  END IF

  CALL MPI_Finalize(ierr)
END PROGRAM customize_replay
