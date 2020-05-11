!==============================================================================!
  program Demo_Parallel_Write_And_Read
!------------------------------------------------------------------------------!
  use Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer(LI) :: i, error
!------------------------------------------------------------------------------!

  ! Start MPI run
  call Mpi_Init(error)
  call Mpi_Comm_Size(MPI_COMM_WORLD, n_proc, error) 
  call Mpi_Comm_Rank(MPI_COMM_WORLD, this_proc, error) 
  this_proc = this_proc + 1

  ! Work out number of cells in each processor
  if(mod(N_CELL_TOT, n_proc) .ne. 0) then
    if(this_proc < 2) then
      print *, 'Total number of cells', N_CELL_TOT,  &
               'can''t be divided by the number of processors ', n_proc, '.'
      print *, 'Try to run on a different number of processors.'
    end if
    call Mpi_Barrier(MPI_COMM_WORLD, error)
    call Mpi_Finalize(error)
  end if
  n_cell_sub = N_CELL_TOT / n_proc

  ! Create mapping
  allocate(map(n_cell_sub))
  do i = 1, n_cell_sub
    map(i) = (this_proc-1) * n_cell_sub + i - 1
  end do

  ! Write ...
  call Write_File_Seq()
! call Write_File_Par()

  ! ... and read
  call Read_File('variable_03')

  ! Finish MPI run
  call Mpi_Finalize(error)

  end program
