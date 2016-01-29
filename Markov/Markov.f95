program Markov
implicit none

character (LEN=1) :: state
integer :: i
real :: x

state = "A"

print*, state

do i=1,30

  call RANDOM_NUMBER(x) ! a random number between 0.0 and 1.0
  
  select case(state)
  
    case("A")
      if (x <= 0.7) then ! 70% of the time
        state = "A"
        else if (x <= 0.9) then ! 20% of the time
          state = "B"
        else ! 10% of the time
          state = "C"
      endif
    
    case("B")
      if (x <= 0.25) then ! 25% of the time
        state = "A"
        else if (x <= 0.75) then ! 75% of the time
          state = "B"
        else ! 25% of the time
          state = "C"
      endif
    
    case("C")
      state = "A" ! 100% of the time

  end select

  print*, state

enddo

end program Markov
