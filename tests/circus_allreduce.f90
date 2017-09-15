PROGRAM circus_allreduce
  USE mpi
#ifdef MPI_WRAPPED
  USE raw_wrapper
#endif
  IMPLICIT NONE

  INTEGER :: ierr
  INTEGER :: rank, rank_in_place
  INTEGER :: rank_sum

#ifdef MPI_WRAPPED
  CALL rwi_set_enabled(1)
#endif

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  rank_in_place = rank

  PRINT *, 'Rank: ', rank, ' - ', rank_in_place

  CALL MPI_Allreduce(rank, rank_sum, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, ierr)

  CALL MPI_Allreduce(MPI_IN_PLACE, rank_in_place, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, ierr)

  PRINT *, 'Sum: ', rank_sum, ' - ', rank_in_place

  rank_sum = 0

#ifdef MPI_WRAPPED
  CALL rwi_reset_counter()
#endif

  CALL MPI_Reduce(rank, rank_sum, 1, MPI_INT, MPI_SUM, 1, MPI_COMM_WORLD, ierr)

#ifdef MPI_WRAPPED
  CALL rwi_reset_counter()
  CALL rwi_set_replay_enabled(1, 1)
#endif

  CALL MPI_Reduce(rank, rank_sum, 1, MPI_INT, MPI_SUM, 1, MPI_COMM_WORLD, ierr)

  IF(rank .EQ. 1) THEN
    PRINT *, 'Sum: ', rank_sum
  END IF

  CALL MPI_Finalize(ierr)
END PROGRAM circus_allreduce

