! rosetta.initialization.f95

call INIT_RANDOM_SEED() ! initializes with a random seed (using the CPU clock to randomize it) for the so that RANDOM_NUMBER outputs different results at each execution

N_cells = 100 ! total number of cells (effectively ~ size of the composition)

markov_chain(1,1) = 0.7 ! that is, the route A->A has 70% of chance of being selected from an element A
markov_chain(1,2) = 0.2 ! that is, the route A->B has 20% of chance of being selected from an element A
markov_chain(1,3) = 0.1 ! that is, the route A->C has 10% of chance of being selected from an element A
markov_chain(2,1) = 0.5 ! that is, the route B->A has 50% of chance of being selected from an element B
markov_chain(2,2) = 0.1 ! that is, the route B->B has 10% of chance of being selected from an element B
markov_chain(2,3) = 0.4 ! that is, the route B->C has 40% of chance of being selected from an element B
markov_chain(3,1) = 0.1 ! that is, the route C->A has 10% of chance of being selected from an element C
markov_chain(3,2) = 0.1 ! that is, the route C->B has 10% of chance of being selected from an element C
markov_chain(3,3) = 0.8 ! that is, the route C->C has 80% of chance of being selected from an element C

allocate(cell_type(N_cells))

previous_dynamic = 0 ! there is no previous dynamic for the first note, so this forces the first cell to output a dynamic
previous_pitch = 60 ! there is no previous pitch class for the first note, so this gives it an initial value to move from
