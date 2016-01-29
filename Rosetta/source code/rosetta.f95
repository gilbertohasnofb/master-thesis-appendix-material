! **********************************************************************************************************************************************************
!
! "Rosetta"
! for solo marimba
!
! by Gilberto Agostinho
!
! 29/01/2016
!
! **********************************************************************************************************************************************************

! to compile, open the terminal, navigate to this folder and execute:
! $ gfortran rosetta.f95 lilypondLibrary.o -o rosetta.out
!
! to run, use:
! $ ./rosetta.out

include "./rosetta.subroutines.f95"

program rosetta

use lilypondLibrary
use rosetta_subroutines
implicit none

integer :: N_cells
integer, dimension(:), allocatable :: cell_type
real, dimension(3,3) :: markov_chain
integer :: i, previous_cell, previous_dynamic, previous_pitch
real :: x

! **********************************************************************************************************************************************************
! Initializing values

! initializing the random seed, allocating of vectors, defining the size of the compostion (in terms of elements), defining markov chain
include "rosetta.initialization.f95"

! **********************************************************************************************************************************************************
! Generating structure

include "rosetta.structure.f95"

! **********************************************************************************************************************************************************
! Generating lilypond score
include "rosetta.score.f95"

end program rosetta
