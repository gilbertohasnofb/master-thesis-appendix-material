program LLN
implicit none

integer :: i
real :: x, average

! average of 3 random numbers between 0.0 and 1.0

average = 0.0
do i=1,3
  call RANDOM_NUMBER(x)
  average = average + x
enddo
average = average / 3.0
print*, average

! average of 50 random numbers between 0.0 and 1.0

average = 0.0
do i=1,50
  call RANDOM_NUMBER(x)
  average = average + x
enddo
average = average / 50.0
print*, average

! average of 1000000 random numbers between 0.0 and 1.0

average = 0.0
do i=1,1000000
  call RANDOM_NUMBER(x)
  average = average + x
enddo
average = average / 1000000.0
print*, average

end program LLN
