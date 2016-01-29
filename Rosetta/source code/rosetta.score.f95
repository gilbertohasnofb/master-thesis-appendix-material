! rosetta.score.f95

call HEADER("Rosetta","Gilberto Agostinho",subtitle="for solo marimba",version="2.19.32",filename="rosetta.ly",articulate=.TRUE.)

call SNIPPET(graceOnBeat=.TRUE.,graceMidiDuration="1/3")

call PAPER(topMargin=2.0,bottomMargin=2.6,leftMargin=1.7,rightMargin=1.7,raggedLastBottom = .TRUE.,printPageNumber=.FALSE.)

  call STAFF()

    call TEMPO("16",320)
    
    call STYLE(dodecaphonic=.TRUE.) 
    call OMIT("Score.TimeSignature")
    call OMIT("Score.MetronomeMark")
    call OMIT("Score.BarLine")
    call OMIT("Score.BarNumber")
    call COMMAND("  \override Score.TrillSpanner.to-barline = ##t")
    call COMMAND("  \override Score.Beam.auto-knee-gap = ##f")
    call COMMAND("  \override Score.Flag.stencil = #modern-straight-flag")
    call COMMAND("  \override Score.Hairpin.to-barline = ##f")
    call COMMAND("  \slurDashed")
    
    call COMMAND("  \once \override Score.RehearsalMark.self-alignment-X = #LEFT")
    call COMMAND("  \override Score.RehearsalMark.padding = 3.0")
    call COMMAND("  \override Score.RehearsalMark.extra-offset = #'(0 . 1.3)") ! helps avoid collisions with the first grace beams, since the noteheads are recognized by the skyline but not the beams...
    
    call MARK(markup="\concat \normal-text \normalsize {\override #'(flag-style . modern-straight-flag)"// &
    '\general-align #Y #DOWN \note #"16" #1 = \italic " ca."'//" \hspace #0.25 320}")
        
    do i=1,N_cells
      select case (cell_type(i))
        case(1)
          call CELL_A(previous_dynamic)
        case(2)
          call CELL_B(previous_dynamic,previous_pitch)
        case(3)
          call CELL_C(previous_dynamic,previous_pitch)
      end select
    enddo
    
    call OMIT("Score.BarLine",undo=.TRUE.)
    call BAR("|.")

  call END_STAFF()
  
call SCORE(autoCompile=.TRUE.,MIDI=.TRUE.)
