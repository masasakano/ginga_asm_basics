module testmod
  implicit none 

  integer, parameter :: dp = kind(1.d0)
  integer, parameter, public :: max_fits_char = 68
  integer, parameter, private :: tform_len = 16  ! maximum character length for TFORM (nb., 14 for 'Y23CH15_Y23H07')

  type t_asm_header_comment
    character(len=max_fits_char) :: title='', telescop='', instrume='';
    character(len=17) :: date_obs = '[day] Observation date';
  end type t_asm_header_comment

  type(t_asm_header_comment), parameter :: asm_comm = t_asm_header_comment()
contains
  subroutine test_f90_param()
    !type(t_asm_header_comment), parameter :: asm_comm_sub = t_asm_header_comment()
    type(t_asm_header_comment) :: asm_comm_sub2
    type :: t_pair
      integer :: i = 1
      real    :: x = 0.5
    end type t_pair

    type(t_pair) :: pair
    pair = t_pair()     
    print *, 'TEST: pair=', pair
    print *, 'TEST: asm_comm=', asm_comm
    asm_comm_sub2 = t_asm_header_comment()
    print *, 'TEST: asm_comm_sub2=', asm_comm_sub2
  end subroutine test_f90_param

  character(len=tform_len) function get_tform(index) result(ret)
    implicit none
    integer, intent(in) :: index
    ret = 'Normal return from get_tform().'  ! trimmed due to length.
  end function get_tform
end module testmod

program f90test1
  use testmod
  implicit none

  ! write(*,'("   -1=''",B64.64,"''")') -1.0_dp     !    -1='1011111111110000...0'
  ! write(*,'("   -2=''",B64.64,"''")') -2.0_dp     !    -2='1100000000000000...0'
  ! write(*,'("   -4=''",B64.64,"''")') -4.0_dp     !    -4='1100000000010000...0'
  ! write(*,'("  -15=''",B64.64,"''")') -15.0_dp    !   -15='1100000000101110...0'
  ! write(*,'("  -16=''",B64.64,"''")') -16.0_dp    !   -16='1100000000110000...0'
  ! write(*,'("-1024=''",B64.64,"''")') -1024.0_dp  ! -1024='1100000010010000...0'

  print *, '(f90test1:Run) ', trim(get_tform(5))
end program f90test1

