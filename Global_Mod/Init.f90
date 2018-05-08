!==============================================================================!
  subroutine Global_Mod_Init
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer :: i
!------------------------------------------------------------------------------!

  ! Calculate some global variables
  n_var      = 4
  n_cell_tot = N_CELL_SUB * n_proc

  ! Create mapping
  do i = 1, N_CELL_SUB 
    map(i) = (this_proc-1) * N_CELL_SUB + i - 1
  end do

  end subroutine
