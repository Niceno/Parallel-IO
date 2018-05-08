!==============================================================================!
  program Demo_Parallel_Write_And_Read
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer         :: error
!------------------------------------------------------------------------------!

  ! Start MPI run
  call Mpi_Init(error) 
  call Mpi_Comm_Size(MPI_COMM_WORLD, n_proc, error) 
  call Mpi_Comm_Rank(MPI_COMM_WORLD, this_proc, error) 
  this_proc = this_proc + 1

  call Global_Mod_Init()

  ! Write ...
  call Write_File_Seq()
! call Write_File_Par()

  ! ... and read
  call Read_File('variable_01')

  ! Finish MPI run
  call Mpi_Finalize(error) 
 
  end program
