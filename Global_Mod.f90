!==============================================================================!
  module Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  include 'mpif.h' 
!------------------------------------------------------------------------------!
  integer(8),  parameter   :: LI         =   8  ! long integer size
  integer(8),  parameter   :: SI         =   4  ! short integer size
  integer(8),  parameter   :: LR         =   8  ! double precision real
  integer(8),  parameter   :: SR         =   4  ! single precision real
  integer(LI), parameter   :: N_CELL_TOT = 720  ! 2*3*4*5*6
  integer(LI), parameter   :: TEXT_SIZE  =  80
  integer(LI), parameter   :: N_VAR      =   4
  integer(SI), allocatable :: map(:)
  integer(LI)              :: this_proc
  integer(LI)              :: n_proc
  integer(LI)              :: n_cell_sub
!------------------------------------------------------------------------------!

  end module
