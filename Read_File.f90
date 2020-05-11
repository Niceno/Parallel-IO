!==============================================================================!
  subroutine Read_File(var_name)
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(len=*) :: var_name
!------------------------------------------------------------------------------!
  integer(LI)              :: i, j, var, error, file_handle, new_type
  real(LR)                 :: r_buff(n_cell_sub)
  integer(MPI_OFFSET_KIND) :: disp
  integer(LI)              :: var_offset, curr_offset
  character(len=TEXT_SIZE) :: curr_var_name, val
  character(1), parameter  :: LF = char(10)
!------------------------------------------------------------------------------!

  ! Open file with MPI
  call Mpi_File_Open(MPI_COMM_WORLD,   &
                     'testfile',       & 
                     MPI_MODE_RDONLY,  & 
                     MPI_INFO_NULL,    &
                     file_handle,      &
                     error)

  ! Browse through all variables
  do var = 1, N_VAR

    ! Read the variabe name
    call Mpi_File_Read_All(file_handle,        &
                           curr_var_name,      &
                           TEXT_SIZE,          &
                           MPI_CHARACTER,      &
                           MPI_STATUS_IGNORE,  &
                           error)
    print *, 'Found variable: ', curr_var_name

    ! Read variable's offset
    call Mpi_File_Read_All(file_handle,        &
                           curr_offset,        &
                           1,                  &
                           MPI_INTEGER8,       &
                           MPI_STATUS_IGNORE,  &
                           error)
    print *, ' ... with offset:  ', curr_offset

    if(curr_var_name == var_name) then
      var_offset = curr_offset
    end if
  end do

  ! Create new type and commit it
  call Mpi_Type_Create_Indexed_Block(n_cell_sub,   &  ! integer count (length of map?)
                                     1,            &  ! size of the block
                                     map,          &  ! integer array of displacements 
                                     MPI_DOUBLE,   &  ! integer old data type
                                     new_type,     &  ! integer new data type 
                                     error)           ! integer error
  call Mpi_Type_Commit(new_type,  &
                       error)

  ! You must set displacement in variable, sending 0 doesn't work
  disp = var_offset

  ! Set view 
  call Mpi_File_Set_View(file_handle,    &
                         disp,           &
                         MPI_DOUBLE,     &
                         new_type,       &
                         'native',       &
                         MPI_INFO_NULL,  &
                         error) 

  ! Read the file
  call Mpi_File_Read(file_handle,        &
                     r_buff,             &
                     n_cell_sub,         &
                     MPI_DOUBLE,         &
                     MPI_STATUS_IGNORE,  &
                     error)

  ! Close the file
  call Mpi_File_Close(file_handle, error)

  ! Write out the buffer contents
  print *, '------------------------------------------' //&
           '------------------------------------------'
  do j = 1, n_proc
    if(j == this_proc) then
      do i = 1, n_cell_sub, 8
        write(*,'(i3, i5,a1,i3, 8f9.2)')  j, i, '-', i+7, r_buff(i:i+7)
      end do
      print *, '------------------------------------------' //&
               '------------------------------------------'
    end if
    call Mpi_Barrier(MPI_COMM_WORLD, error)
  end do

  end subroutine
