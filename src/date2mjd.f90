module print_help
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  implicit none 
contains

  ! print USAGE and exit. Default exit status is 1.
  subroutine print_help_exit(exitstatus)
    implicit none
    character(len=*), parameter :: HelpMsg = 'USAGE: date2mjd [-h|-v] YYYY/YY MM DD [hh mm ss millisec(3-digits-Integer)]'
    integer, intent(in), optional :: exitstatus
    integer :: my_exitstatus = 1

    if (present(exitstatus)) then
      my_exitstatus = exitstatus
    else
      my_exitstatus = 1
    end if

    if (my_exitstatus == 0) then
      write(*,'(A)')      HelpMsg
    else
      write(stderr,'(A)') HelpMsg
    end if
    call EXIT(my_exitstatus)
  end subroutine print_help_exit
end module print_help

program date2mjd

  use iso_fortran_env, only : stderr=>ERROR_UNIT   
  use print_help

  !use err_exit       ! NOTE: this is required in asm_fits_common
  use fort_util
  use asm_fits_common ! for type(t_argv), dump_all_argv()

  implicit none

  real(kind=8) :: mjd
  integer(kind=4), dimension(7) :: times
  integer :: i, j, istart_main
  character(len=*), parameter :: Subname = 'main'
  character(len=LEN_T_ARGV) :: arg, comname, msg
  type(t_argv), dimension(:), allocatable :: allargv
  logical :: is_verbose = .false.

  times(:) = 0  ! NOTE: Hour and finer precisions are not mandatory, but initialized.
  is_verbose = .false.
  allocate(allargv(command_argument_count()))  ! F2003 standard  (_excluding_ $0)
  if (size(allargv) == 0) call print_help_exit(exitstatus=1)

  istart_main = -1  ! Index where the main arugment starts.
  do i=0, size(allargv)
    call get_command_argument(i, arg)  ! Fortran 2003
    if (i == 0) then  ! i=0 is for $0
      comname = trim(arg)
      cycle
    end if
    ! if (len_trim(arg) == 0) exit

    if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
      call print_help_exit(exitstatus=0)
      STOP  ! redundant
    end if

    if (istart_main < 0) istart_main = i
    j = i-istart_main+1
    allargv(i) = t_argv(key='main'//ladjusted_int(j), val=arg)  ! Default
    select case(j)
    case(1,2,3,4,5,6)
      if (trim(arg) == '-v') then
        is_verbose = .true.
        istart_main = -1  ! it hasn't reached the main argument.
        allargv(i) = t_argv(key='is_verbose', val='TRUE')
        cycle
      end if
      read(arg, '(I4)') times(j)
    case(7)
      if (len_trim(arg) .ne. 3) then
        write(stderr,'(A)') 'ERROR: millisec must be specified with exactly 3 digits, like "012".'
        call print_help_exit(exitstatus=1)
      end if
      read(arg, '(I3)') times(j)
    case default
      write(stderr,'(A)') 'ERROR: The number of the main arguments must be between 3 and 7, but given more.'
      call print_help_exit(exitstatus=1)
    end select
  end do

  if (any(times < 0) .or. (times(3) == 0)) then
    call dump_all_argv(allargv)
    if (times(3) == 0) then
      msg = 'Month and Day are mandatory.'
    else
      msg = 'All specified numbers must be non-negative.'
    end if
    write(stderr,'("ERROR: ",A)') trim(msg)
    call print_help_exit(exitstatus=1)
  end if

  if (is_verbose) call dump_all_argv(allargv) ! Verbose information

  call MJULIA(times, mjd)  ! in mjd.f in ginga_tools

  write(*,'(E20.14," (",F21.14," )")') mjd, mjd
end program date2mjd

!! ----- Tests  ----- !!
! ./date2mjd -h
! ./date2mjd    1988 3 14 13 37 46 35    ! Error, exitstatus=1
! ./date2mjd    1988 3 14 13 37 46 035
! ./date2mjd    1988 3 14 13 37 46 035 9 ! Error, exitstatus=1
! ./date2mjd -v 1988 03 14
! ./date2mjd      88  3 14
! ./date2mjd      88  3 14 15
! ./date2mjd      88  3 14 99  ! This passes at the moment (b/c no check is performed), though it should raise an error.
! ./date2mjd    2022 3                   ! Error, exitstatus=1
! ./date2mjd    2022 3 -2                ! Error, exitstatus=1
! ./date2mjd                             ! Usage, exitstatus=1

