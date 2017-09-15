INTEGER FUNCTION next_destination(pool_size, own_rank)
  IMPLICIT NONE
  INTEGER, INTENT(in) :: pool_size, own_rank
  INTEGER :: result_value

  IF (own_rank .EQ. pool_size - 1) THEN
    result_value = 0
  ELSE
    result_value = own_rank + 1
  END IF

  next_destination = result_value
END FUNCTION next_destination

PROGRAM ping_pong
  USE mpi
#ifdef MPI_WRAPPED
  USE raw_wrapper
#endif
  IMPLICIT NONE

  INTEGER :: ierr
  INTEGER :: rank
  INTEGER :: status(MPI_STATUS_SIZE)

  INTEGER :: ping_count = 0
  INTEGER :: pool_size = 0
  INTEGER :: ping_state = 0
  INTEGER :: next_destination
  INTEGER :: pong_dest
  INTEGER :: last_count

  CHARACTER(SIZEOF(ping_count)) :: buf
  INTEGER :: message_size

  message_size = SIZEOF(buf)

#ifdef MPI_WRAPPED
  CALL rwi_set_enabled(1)
#endif

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD, pool_size, ierr)

  DO WHILE(ping_state .NE. 3)
    IF (ping_state .EQ. 0) THEN
      IF (rank .EQ. 0) THEN
        pong_dest = next_destination(pool_size, rank)
        PRINT *, 'Ping: ', rank, ': ', pong_dest
        WRITE (buf(1:4), '(A)') ping_count
        CALL MPI_Send(buf, message_size, MPI_CHAR, pong_dest, 0, MPI_COMM_WORLD, ierr)

        ping_count = ping_count + 1
        ping_state = 1
      ELSE
        CALL MPI_Recv(buf, message_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
        READ (buf(1:4), '(A)') last_count
        PRINT *, 'Pong: ', rank, ', ', last_count

        ping_state = 2
      END IF
    ELSE IF (ping_state .EQ. 1) THEN
      CALL MPI_Recv(buf, message_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
      READ (buf(1:4), '(A)') last_count
      PRINT *, 'Pong: ', rank, ', ', last_count

      ping_state = 2
    ELSE IF (ping_state .EQ. 2) THEN
      pong_dest = next_destination(pool_size, rank)
      PRINT *, 'Ping: ', rank, ': ', pong_dest
      WRITE (buf(1:4), '(A)') ping_count
      CALL MPI_Send(buf, message_size, MPI_CHAR, pong_dest, 0, MPI_COMM_WORLD, ierr)

      ping_count = ping_count + 1

      IF (ping_count .EQ. 100) THEN
        ping_state = 3
      ELSE
        ping_state = 1
      END IF
    END IF
  END DO

  ! Primtive good bye.
  IF (rank .EQ. 0) THEN
    CALL MPI_Recv(buf, message_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
  END IF

  CALL MPI_Finalize(ierr)
END PROGRAM ping_pong
