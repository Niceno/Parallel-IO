!==============================================================================!
  subroutine Write_File_Seq
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer                  :: i, var
  integer                  :: offset
  real, allocatable        :: s_buff(:)
  character(len=TEXT_SIZE) :: var_name
!------------------------------------------------------------------------------!

  if(this_proc .ne. 1) return
 
  allocate(s_buff(N_CELL_SUB * n_proc))

  ! Fill (local) buffer with some values
  do i = 1, n_cell_tot
    s_buff(i) = i 
  end do 

  ! Create file sequentially 
  open(9, file='testfile', access='stream', form='unformatted')

  !----------------------------------------------------------------!
  !   Write header; that is all variable names and their offsets   !
  !----------------------------------------------------------------!
  do var = 1, n_var

    ! Set variable name
    var_name = 'variable_'
    write(var_name(10:11),'(i2.2)') var

    ! Write variable name
    write(9) var_name

    ! Write variable offset
    write(9) Offset(var)

  end do

  !-------------------------------!
  !   Write all variable values   !
  !-------------------------------!
  do var = 1, n_var

    ! Set different values for different variables
    s_buff = s_buff + 1000

    ! Store data
    write(9) (s_buff(i), i=1, n_cell_tot)

  end do

  ! Close file
  close(9)
  
  end subroutine
