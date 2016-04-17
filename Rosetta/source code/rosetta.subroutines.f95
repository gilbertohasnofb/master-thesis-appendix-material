! rosetta.subroutines.f95

module rosetta_subroutines
use lilypondLibrary
implicit none

contains

! **********************************************************************************************************************************************************

  ! Subroutine INIT_RANDOM_SEED: selects a random value for RANDOM_SEED by using the computer clock, which assures that RANDOM will output different numbers each run
  ! Adapted from: http://fortranwiki.org/fortran/show/random_seed
  subroutine INIT_RANDOM_SEED()
    integer :: i, N, x
    integer, dimension(:), allocatable :: seed

    call SYSTEM_CLOCK(count=x)

    call RANDOM_SEED(size=N)
    allocate(seed(N))

    seed=x+37*(/ (i - 1, i = 1, n) /)

    call RANDOM_SEED(put=seed)

    deallocate(seed)

  end subroutine INIT_RANDOM_SEED

! **********************************************************************************************************************************************************
! Bubble sort: sorting algorithm, reorganize a vector from the smallest to the largest value

  subroutine BUBBLE_SORT(vector)
    integer, intent(INOUT), dimension(:) :: vector
    real :: aux
    integer :: i, j, vector_size
    logical :: swapped
    
    vector_size = SIZE(vector)

    do j = (vector_size-1), 1, -1 ! that is, go from the element before the last to the first backwards
      swapped = .FALSE.
      do i = 1, j
        if (vector(i) > vector(i+1)) then ! if the current element is larger than the next, swap it
          aux = vector(i)
          vector(i) = vector(i+1)
          vector(i+1) = aux
          swapped = .TRUE.
        endif
      enddo
      if (.NOT. swapped) exit
    enddo

  end subroutine BUBBLE_SORT
	
! **********************************************************************************************************************************************************

  subroutine RANDOM_PITCH(pitch)
    integer, intent(OUT) :: pitch
    real :: x
        
    call RANDOM_NUMBER(x) ! random x will be: 0.0 <= x < 1.0 (that is, x can never be exactly 1.0!)
    pitch = floor(x * 44 + 53) ! results between 53 and 96, or F3 and C7 in MIDI numbers      
    
  end subroutine RANDOM_PITCH
  
! **********************************************************************************************************************************************************

  subroutine RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
    integer, intent(OUT) :: pitch
    integer, intent(INOUT) :: previous_pitch
    integer :: previous_pitch_class

    previous_pitch_class = MOD(previous_pitch,12)    
    pitch = previous_pitch ! unwanted condition
    
    do while ((MOD(pitch,12) == previous_pitch_class) .OR. (ABS(previous_pitch - pitch) > 9)) ! no repeated pitch classes and not intervals larger than a major 6th
      call RANDOM_PITCH(pitch)
    enddo
    
    previous_pitch = pitch
    
  end subroutine RANDOM_PITCH_NO_REPETITION

! **********************************************************************************************************************************************************

  subroutine RANDOM_DURATION(duration)
    integer, intent(OUT) :: duration
    real :: x
    
    call RANDOM_NUMBER(x) ! random x will be: 0.0 <= x < 1.0 (that is, x can never be exactly 1.0!) 
    duration = floor(x * 7 + 2) ! results between 2 and 8, or from 2 sixteenth notes to 8 sixteenth notes
    
  end subroutine RANDOM_DURATION
  
! **********************************************************************************************************************************************************

  subroutine RANDOM_DYNAMIC(dynamic,dynamic_string,previous_dynamic)
    integer, intent(OUT) :: dynamic
    character (LEN=3), intent(OUT) :: dynamic_string
    integer, intent(INOUT) :: previous_dynamic
    real :: x
    
    call RANDOM_NUMBER(x) ! random x will be: 0.0 <= x < 1.0 (that is, x can never be exactly 1.0!) 
    dynamic = floor(x * 4 + 1) ! results between 1 and 4
    
    ! mapping results as: 1 = p, 2 = mp, 3=mf, 4 = f
    select case(dynamic)
      case(1)
        dynamic_string = "\p "
      case(2)
        dynamic_string = "\mp"
      case(3)
        dynamic_string = "\mf"
      case(4)
        dynamic_string = "\f "
    end select
    
    if (dynamic == previous_dynamic) dynamic_string = "   " ! if the new dynamic is the same as he previous one it won't be written again
    
    previous_dynamic = dynamic
    
  end subroutine RANDOM_DYNAMIC
  
! **********************************************************************************************************************************************************

  subroutine RANDOM_CHORD(pitch_vector)
    integer, dimension(4), intent(OUT) :: pitch_vector
    integer :: i, j
    logical :: unique_pitch_classes
    integer :: intervalA, intervalB, intervalInner
    
    intervalA = 0 ! unwanted values, so the subroutine enters the first loop below
    intervalB = 0
    
    do while ((intervalA > 9) .OR. (intervalA <= 1) .OR. (intervalB > 9) .OR. (intervalB <= 1) .OR. (intervalInner <= 1)) ! the pitches of the left hand and of the right hand must be within a major sixth but larger than a minor second, and the interval between the two inner notes can't be a minor second
    
      unique_pitch_classes = .FALSE. ! starting condition for the loop below
      
      ! selecting four pitches under the conditions: no repeated pitch classes
      do while (.NOT. unique_pitch_classes)
        pitch_vector = 0 ! resetting vector
        unique_pitch_classes = .TRUE.
        do i=1,4
          call RANDOM_PITCH(pitch_vector(i))
          do j=1,(i-1)
            if (MOD((pitch_vector(i) - pitch_vector(i-j)),12) == 0) unique_pitch_classes = .FALSE. ! checking for repeated pitch classes
          enddo
        enddo
      enddo
      
      call BUBBLE_SORT(pitch_vector) ! sorting pitches in order
        
      intervalA = pitch_vector(2) - pitch_vector(1) ! interval of the left hand pitches, used to check if they are within a major sixth but larger than a minor second
      intervalB = pitch_vector(4) - pitch_vector(3) ! interval of the right hand pitches, used to check if they are within a major sixth but larger than a minor second
      intervalInner = pitch_vector(3) - pitch_vector(2) ! interval of the inner two notes, used to check if the distance between both hands is at least a major second
      
    enddo
    
  end subroutine RANDOM_CHORD

! **********************************************************************************************************************************************************
  ! cell A: chord cell
  subroutine CELL_A(previous_dynamic)
    integer, intent(INOUT) :: previous_dynamic
    integer, dimension(4) :: pitch_vector
    integer :: duration
    integer :: dynamic
    character (LEN=3) :: dynamic_string
    logical :: four_note_chord, grace_note, arpeggioUp, arpeggioDown
    real :: x
    
    four_note_chord = .TRUE. ! in principle, this will be a 4 note chord
    grace_note = .FALSE. ! in principle, this chord has no grace notes
    arpeggioUp = .FALSE. ! in principle, this chord has no arpeggio
    arpeggioDown = .FALSE.
    
    ! receiving a random chord of 4 notes
    call RANDOM_CHORD(pitch_vector)
    
    ! selecting a random duration
    call RANDOM_DURATION(duration)
    
    ! selecting a random dynamic
    call RANDOM_DYNAMIC(dynamic,dynamic_string,previous_dynamic)
    
    ! selecting between 3 or 4 note chords
    call RANDOM_NUMBER(x)
    if (x < 0.5) then ! 50% of the time the top note is removed, creating a 3-note chord
      four_note_chord = .FALSE. 
      pitch_vector(4) = 0       
    endif
    
    if (four_note_chord) then ! only if this is a 4-note chord
      call RANDOM_NUMBER(x)
      if (x < 0.2) then ! 20% of the time
        grace_note = .TRUE. ! bottom note is a tied grace note
        else if (x < 0.35) then ! 15% of the time
          arpeggioUp = .TRUE.
        else if (x < 0.5) then ! 15% of the time
          arpeggioDown = .TRUE.
      endif
    endif
    
    call TIME(duration,16) ! time signature
    
    if (grace_note) then ! if grace note
      call GRACE("\slashedGrace")
      call NOTE(pitch_vector(1),"8",TRIM(dynamic_string))
      call TIE()
      call END_GRACE()
      dynamic_string = "" ! erasing dynamic since it appears below the grace anyway
    endif
    
    if (arpeggioUp) call COMMAND("  \arpeggioArrowUp") ! arpeggio up
    if (arpeggioDown) call COMMAND("  \arpeggioArrowDown") ! arpeggio down
    
    select case (duration)
      case(2)
        call CHORD(pitch_vector(1:4),"8",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
      case(3)
        call CHORD(pitch_vector(1:4),"8.",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
      case(4)
        call CHORD(pitch_vector(1:4),"4",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
      case(5)
        call CHORD(pitch_vector(1:4),"4",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
        call TIE()
        call CHORD(pitch_vector(1:4),"16")
      case(6)
        call CHORD(pitch_vector(1:4),"4.",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
      case(7)
        call CHORD(pitch_vector(1:4),"4..",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
      case(8)
        call CHORD(pitch_vector(1:4),"2",TRIM(dynamic_string))
        if (arpeggioUP .OR. arpeggioDown) call ARPEGGIO()
    end select
    
  end subroutine CELL_A
  
! **********************************************************************************************************************************************************
  ! cell B: tremolo cell
  subroutine CELL_B(previous_dynamic,previous_pitch)
    integer, intent(INOUT) :: previous_dynamic
    integer, intent(INOUT) :: previous_pitch
    integer :: pitch
    integer :: duration
    integer :: dynamic
    character (LEN=3) :: dynamic_string
    integer :: graces
    integer :: i
    real :: x
        
    ! selecting a random duration
    call RANDOM_DURATION(duration)
    
    ! selecting a random dynamic
    call RANDOM_DYNAMIC(dynamic,dynamic_string,previous_dynamic)
    
    call TIME(duration,16) ! time signature

    call RANDOM_NUMBER(x)
    if (x< 0.5) then ! 50% of the time there will be a grace note
      call RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
      call GRACE("\slashedGrace")
      call NOTE(pitch,"8",TRIM(dynamic_string))
      dynamic_string = ""
      call END_GRACE()
    endif
    
    call RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
    select case (duration)
      case(2)
        call NOTE(pitch,"8:32",TRIM(dynamic_string))
      case(3)
        call NOTE(pitch,"8.:32",TRIM(dynamic_string))
      case(4)
        call NOTE(pitch,"4:32",TRIM(dynamic_string))
      case(5)
        call NOTE(pitch,"4:32",TRIM(dynamic_string),S="(")
        call OMIT("Score.Accidental",once=.TRUE.)
        call NOTE(pitch,"16:32",S=")")
      case(6)
        call NOTE(pitch,"4.:32",TRIM(dynamic_string))
      case(7)
        call NOTE(pitch,"4..:32",TRIM(dynamic_string))
      case(8)
        call NOTE(pitch,"2:32",TRIM(dynamic_string))
    end select
  
  end subroutine CELL_B
  
! **********************************************************************************************************************************************************
  ! cell C: rhythmic cell
  subroutine CELL_C(previous_dynamic,previous_pitch)
    integer, intent(INOUT) :: previous_dynamic
    integer, intent(INOUT) :: previous_pitch
    integer :: pitch
    integer :: duration
    integer :: dynamic
    integer :: N_notes
    character (LEN=3) :: dynamic_string
    integer :: i
    real :: x
    
    ! selecting a random dynamic
    call RANDOM_DYNAMIC(dynamic,dynamic_string,previous_dynamic)
    
    ! number of notes
    call RANDOM_NUMBER(x)
    N_notes = floor(x*3 + 3) ! that is, N_notes is either 3, 4 or 5
    
    call TIME(1,4)
    
    select case(N_notes)
    
      case(3)
        call TUPLET(N_notes,2)
        do i=1,N_notes
          call RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
          call NOTE(pitch,"8",TRIM(dynamic_string))
          dynamic_string = ""      
        enddo
        call END_TUPLET()
        
      case(4)
        do i=1,N_notes
          call RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
          call NOTE(pitch,"16",TRIM(dynamic_string))
          dynamic_string = ""      
        enddo
        
      case(5)
        call TUPLET(N_notes,4)
        do i=1,N_notes
          call RANDOM_PITCH_NO_REPETITION(pitch,previous_pitch)
          call NOTE(pitch,"16",TRIM(dynamic_string))
          dynamic_string = ""      
        enddo
        call END_TUPLET()
        
   end select
  
  end subroutine CELL_C
  
! **********************************************************************************************************************************************************

end module rosetta_subroutines
