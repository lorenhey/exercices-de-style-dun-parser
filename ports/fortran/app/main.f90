program main
  use iso_fortran_env, only: error_unit
  use nlp_parser_v7,  only: parser_t
  implicit none

  type(parser_t) :: p
  character(len=:), allocatable :: grammar_path, lexicon_path, line, tree
  integer :: beam, ios
  logical :: ok

  ! Defaults (asumimos tu layout “resources/”)
  grammar_path = "resources/grammar.json"
  lexicon_path = "resources/lexicon.json"
  beam = 8

  call parse_args(grammar_path, lexicon_path, beam)

  ok = p%load_resources(grammar_path, lexicon_path, beam)
  if (.not. ok) then
    write(error_unit,'(a)') "ERROR: no pude cargar grammar/lexicon. Revisá paths y JSON."
    stop 1
  end if

  do
    call read_line_stdin(line, ios)
    if (ios /= 0) exit
    if (len_trim(line) == 0) cycle

    tree = p%parse_sentence(line)
    if (len_trim(tree) == 0) then
      write(*,'(a)') "(NO-PARSE)"
    else
      write(*,'(a)') trim(tree)
    end if
  end do

contains

  subroutine read_line_stdin(s, ios)
    character(len=:), allocatable, intent(out) :: s
    integer, intent(out) :: ios
    character(len=8192) :: buf
    read(*,'(A)',iostat=ios) buf
    if (ios /= 0) then
      s = ""
      return
    end if
    s = trim(buf)
  end subroutine

  subroutine parse_args(gpath, lpath, beam)
    character(len=:), allocatable, intent(inout) :: gpath, lpath
    integer, intent(inout) :: beam
    integer :: n, i
    character(len=1024) :: arg

    n = command_argument_count()
    i = 1
    do while (i <= n)
      call get_command_argument(i, arg)
      select case (trim(arg))
      case ("--grammar")
        if (i+1 <= n) then
          call get_command_argument(i+1, arg)
          gpath = trim(arg); i = i + 2
        else
          i = i + 1
        end if
      case ("--lexicon")
        if (i+1 <= n) then
          call get_command_argument(i+1, arg)
          lpath = trim(arg); i = i + 2
        else
          i = i + 1
        end if
      case ("--beam")
        if (i+1 <= n) then
          call get_command_argument(i+1, arg)
          read(arg,*,end=10,err=10) beam
10        continue
          if (beam < 1) beam = 1
          i = i + 2
        else
          i = i + 1
        end if
      case default
        i = i + 1
      end select
    end do
  end subroutine

end program main
