MODULE raw_wrapper
  INTERFACE
    SUBROUTINE rwi_reset_counter()
    END SUBROUTINE

    SUBROUTINE rwi_set_enabled(state)
      IMPLICIT NONE
      INTEGER :: state
    END SUBROUTINE

    SUBROUTINE rwi_set_replay_filter(filter_ptr)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_FUNPTR) :: filter_ptr
    END SUBROUTINE

    SUBROUTINE rwi_set_replay_data(array, size)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      BYTE :: array(:)
      INTEGER(C_SIZE_T) :: size
    END SUBROUTINE

    SUBROUTINE rwi_set_replay_enabled(state, rank)
      IMPLICIT NONE
      INTEGER :: state, rank
    END SUBROUTINE
  END INTERFACE
CONTAINS
  SUBROUTINE rwi_set_replay_data_vb(array)
    IMPLICIT NONE
    BYTE :: array(:)

    CALL rwi_set_replay_data(array, SIZEOF(array))
  END SUBROUTINE
END MODULE raw_wrapper
