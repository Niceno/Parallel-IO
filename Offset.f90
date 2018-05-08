!==============================================================================!
  integer         function Offset(var)
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer         :: var
!==============================================================================!

  Offset = (TEXT_SIZE + INT_SIZE) * n_var  &  ! Size of the entire header
         + n_cell_tot * (var-1) * DATA_SIZE   ! Taken by variables before this

  end function
