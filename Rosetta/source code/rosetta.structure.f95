! rosetta.structure.f95

! first cell must be random
call RANDOM_NUMBER(x)

if (x < 0.333333) then ! that is, 33% of the time first cell will be A
  cell_type(1) = 1
  else if (x < 0.666666) then ! also 33% of the time it will be B
    cell_type(1) = 2
  else ! also 33% of the time it will be C
    cell_type(1) = 3
endif

do i=2,N_cells

  previous_cell = cell_type(i-1) ! previous cell, 1, 2 or 3

  call RANDOM_NUMBER(x)
  
  if (x < markov_chain(previous_cell,1)) then
    cell_type(i) = 1
    else if (x < (markov_chain(previous_cell,1) + markov_chain(previous_cell,2))) then
      cell_type(i) = 2
    else
      cell_type(i) = 3
  endif
  
enddo

! now the vector cell_type will have N_cells number of values (100 in this case), with values being either 1, 2 or 3. It will be used to select which cells are selected
