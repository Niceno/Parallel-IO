!==============================================================================!
  function Offset(var)
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer(LI) :: Offset
  integer(LI) :: var
!==============================================================================!

  Offset = (TEXT_SIZE + LI) * N_VAR  &  ! size of the entire header
         + n_cell_tot * (var-1) * LR    ! size taken by variables before this

  end function
