module print_help
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  implicit none 

  !integer, parameter :: LEN_READABLE_KEY = 32  ! Char-length of maximum human-readable key name
  !integer, parameter :: LEN_T_ARGV = 1024

  !type t_argv  ! defined in asm_fits_common
  !  character(len=LEN_READABLE_KEY) :: key;
  !  character(len=LEN_T_ARGV) :: val = '';
  !end type t_argv
contains

  ! print USAGE and exit. Default exit status is 1.
  subroutine print_help_exit(exitstatus)
    implicit none
    character(len=*), parameter :: HelpMsg = 'USAGE: mjd2date [-h|-v] MJD'
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

program mjd2date

  use iso_fortran_env, only : stderr=>ERROR_UNIT   
  use print_help

  !use err_exit       ! NOTE: this is required in asm_fits_common
  use fort_util
  use asm_fits_common ! for dump_all_argv()

  implicit none

  real(kind=8) :: mjd
  integer(kind=4), dimension(7) :: times
  integer :: i, j, istart_main
  character(len=*), parameter :: Subname = 'main'
  character(len=LEN_T_ARGV) :: arg, comname
  type(t_argv), dimension(:), allocatable :: allargv
  logical :: is_verbose = .false.

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
    select case(j)
    case(1)
      if (trim(arg) == '-v') then
        is_verbose = .true.
        istart_main = -1  ! it hasn't reached the main argument.
        allargv(i) = t_argv(key='is_verbose', val='TRUE')
        cycle
      end if
      read(arg, '(E60.50)') mjd
      allargv(i) = t_argv(key='main'//ladjusted_int(j), val=arg)
    case default
      write(stderr,'(A)') 'ERROR: The number of the main arguments must be exactly 1, but given more.'
      call print_help_exit(exitstatus=1)
    end select
  end do

!  if (istart_main == -1) then
!    n_mainargs = size(allargv)
!  else
!    n_mainargs = size(allargv) - istart_main + 1
!  end if
!  if (n_mainargs < 3) call err_exit_with_msg( &
!     'The number of the main arguments must be at least 3, but given only '//trim(ladjusted_int(n_mainargs)))
!if (IS_DEBUG()) print *,'DEBUG:024: istart_main=',istart_main, ' n_mainargs=',n_mainargs

  if (is_verbose) call dump_all_argv(allargv) ! Verbose information

  call MJDATE(mjd, times)  ! in mjd.f in ginga_tools

  write(*,'(I0.4,"-",I0.2,"-",I0.2,"T",I0.2,":",I0.2,":",I0.2,".",I0.3)') &
     times(1), times(2), times(3), times(4), times(5), times(6), times(7)
end program mjd2date

!! ----- Tests  ----- !!
! ./mjd2date -h
! ./mjd2date    47234.567893923
! ./mjd2date -v 47234.567893923
! ./mjd2date -v 47234.567893923 32  ! Error, exitstatus=1
! ./mjd2date    47234.567893923 32  ! Error, exitstatus=1
! ./mjd2date                        ! Usage, exitstatus=1

