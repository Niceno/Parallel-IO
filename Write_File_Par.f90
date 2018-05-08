!==============================================================================!
  subroutine Write_File_Par
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer         :: Offset
!------------------------------------------------------------------------------!
  integer                       :: i, var, error, file_handle, new_type
  real                          :: s_buff(N_CELL_SUB)
  integer(kind=MPI_OFFSET_KIND) :: disp 
  character(len=TEXT_SIZE)      :: var_name
!------------------------------------------------------------------------------!

  ! Fill (local) buffer with some values
  do i = 1, N_CELL_SUB 
    s_buff(i) = (this_proc-1) * N_CELL_SUB + i 
  end do 

  ! Open file with MPI
  call Mpi_File_Open(MPI_COMM_WORLD,                    &
                     'testfile',                        & 
                     MPI_MODE_WRONLY + MPI_MODE_CREATE, & 
                     MPI_INFO_NULL,                     &
                     file_handle,                       &
                     error) 

  !----------------------------------------------------------------!
  !   Write header; that is all variable names and their offsets   !
  !----------------------------------------------------------------!
  do var = 1, n_var

    ! Set variable name
    var_name = 'variable_'
    write(var_name(10:11),'(i2.2)') var

    ! Write variable name
    call Mpi_File_Write(file_handle,        &
                        var_name,           &
                        TEXT_SIZE,          &
                        MPI_CHARACTER,      & 
                        MPI_STATUS_IGNORE,  &
                        error) 

    ! Write variable offset
    call Mpi_File_Write(file_handle,        &
                        Offset(var),        &
                        1,                  &
                        MPI_INTEGER8,       & 
                        MPI_STATUS_IGNORE,  &
                        error) 
  end do

  !-----------------------------------!
  !   Create new type and commit it   !
  !-----------------------------------!
  call Mpi_Type_Create_Indexed_Block(N_CELL_SUB,   &  ! integer count (length of map?)
                                     1,            &  ! size of the block
                                     map,          &  ! integer array of displacements 
                                     MPI_DOUBLE,   &  ! integer old data type
                                     new_type,     &  ! integer new data type 
                                     error)           ! integer error
  call Mpi_Type_Commit(new_type,  &
                       error)

  !-------------------------------!
  !   Write all variable values   !
  !-------------------------------!
  do var = 1, n_var

    ! Set different values for different variables
    s_buff = s_buff + 1000

    disp = Offset(var)

    ! Set view 
    call Mpi_File_Set_View(file_handle,    &
                           disp,           &
                           MPI_DOUBLE,     & 
                           new_type,       &
                           'native',       & 
                           MPI_INFO_NULL,  &
                           error) 
    ! Store data
    call Mpi_File_Write(file_handle,        &
                        s_buff,             &
                        N_CELL_SUB,         &
                        MPI_DOUBLE,         & 
                        MPI_STATUS_IGNORE,  &
                        error) 

  end do

  ! Close the file
  call Mpi_File_Close(file_handle, error) 

  end subroutine
