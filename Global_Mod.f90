!==============================================================================!
  module Global_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  include 'mpif.h' 
!------------------------------------------------------------------------------!
  integer        , parameter :: N_CELL_SUB = 10
  integer        , parameter :: TEXT_SIZE  = 80
  integer        , parameter :: INT_SIZE   =  8
  integer        , parameter :: DATA_SIZE  =  8
  integer(kind=4)    :: map(N_CELL_SUB)
  integer            :: this_proc
  integer            :: n_proc
  integer            :: n_var
  integer            :: n_cell_tot
!------------------------------------------------------------------------------!

  contains

  include 'Global_Mod/Init.f90'

  end module
