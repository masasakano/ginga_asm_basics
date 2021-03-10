! Subroutines for exit due to an error
!
module err_exit
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  implicit none 

  !real, parameter, public :: pi = 3.1415926536  
  !real, parameter, public :: e = 2.7182818285 
   
contains
  ! If FITSIO status is non-zero, exit(1)
  subroutine err_exit_if_status(status, usermsg)
    implicit none

    character(len=*), parameter :: Prefix = 'ERROR: FITSIO-status is '

    integer,          intent(in) :: status
    character(len=*), intent(in) :: usermsg  ! User Error message.  !! TODO: make it optional
    character(len=30) :: errtext
    character(len=1024) :: sout

    if (status .ne. 0) then
      call FTGERR(status, errtext)
      write(sout, '(a, i8.2, " (", a, ") ", a)') Prefix, status, trim(errtext), trim(usermsg)
      call err_exit_with_msg(sout)
      return ! redundant
    end if
  end subroutine err_exit_if_status

  ! exit(1)
  subroutine err_exit_with_msg(usermsg)
    implicit none
    character(len=*), parameter :: Prefix = 'FATAL: '
    character(len=*), intent(in) :: usermsg  ! User Error message.

    write(stderr,'(a, a)') Prefix, trim(usermsg)
    call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    ! stop 1      ! redundant. Anyway not yet standard? (works for gfortran?)
    return ! redundant
  end subroutine err_exit_with_msg

  ! exit(1), playing safe
  subroutine err_exit_play_safe(usermsg)
    implicit none
    character(len=*), parameter :: Def_msg = 'Should not happen. Contact the code developer.'
    character(len=*), intent(in), optional :: usermsg  ! optional additional User Error message.

    if (present(usermsg)) then
      call err_exit_with_msg(Def_msg//' '//trim(usermsg))
    else
      call err_exit_with_msg(Def_msg)
    end if
    return ! redundant
  end subroutine err_exit_play_safe
end module err_exit

