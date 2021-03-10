module unittest
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  use fort_util
  !use err_exit

  implicit none 
  integer, parameter, private :: dp = kind(1.d0) ! defined in asm_fits_common
  character(len=*), parameter, private :: prefix_fmt_test_fail = '("Failed(", a, ") [exp/act] ", '

  interface assert_equal	! Handle functions regardless of Types.
    module procedure assert_equalja, assert_equalba, assert_equaljba, assert_equalj, assert_equals &
       , assert_equalas &
       , assert_equaljua, assert_equalii, assert_equalij, assert_equalji
  end interface assert_equal

  interface assert_in_delta	! Handle functions regardless of Types.
    module procedure assert_in_deltarr, assert_in_deltadd
  end interface assert_in_delta

  interface assert_smaller_than	! Handle functions regardless of Types.
    module procedure assert_smaller_thanjj, assert_smaller_thandd
  end interface assert_smaller_than
  
  interface assert_greater_than	! Handle functions regardless of Types.
    module procedure assert_greater_thanjj, assert_greater_thandd
  end interface assert_greater_than

contains
  ! Internal routine to write a message
  subroutine write_msg_optmsg(msg, optmsg)
    character(len=*), intent(in) :: msg
    character(len=*), intent(in), optional :: optmsg

    if (present(optmsg) .and. (len(trim(optmsg)) > 0)) then
      write(stderr,'(a, " ", a)') trim(msg), trim(optmsg)
    else
      write(stderr,'(a)')         trim(msg)
    end if
  end subroutine write_msg_optmsg

  !-----------------------------------------
  ! assert and assert_not
  !-----------------------------------------
  
  ! If true, returns .true. (The result should be TRUE)
  logical function assert(res, subname, optmsg) result(succeed)
    logical, intent(in) :: res
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (res) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'"Not Truthy as it should be.")') trim(subname)
    call write_msg_optmsg(msg, optmsg)
  end function assert
  
  ! If False, returns .true (The result should be FALSE).
  logical function assert_not(res, subname, optmsg) result(succeed)
    logical, intent(in) :: res
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (.not. res) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'"Not Falsy as it should be.")') trim(subname)
    call write_msg_optmsg(msg, optmsg)
  end function assert_not

  !-----------------------------------------
  ! assert_equal  (n.b., exp() is a built-in function, hence the name of variable "exq")
  !-----------------------------------------
  
  ! Compare 2 Characters exq and act and returns True/False
  logical function assert_equals(exq, act, subname, optmsg) result(succeed)
    character(len=*), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (trim(exq) == trim(act)) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'a, " != ", a)') trim(subname), '"'//trim(exq)//'"', '"'//trim(act)//'"'
    call write_msg_optmsg(msg, optmsg)
  end function assert_equals

  ! Compare 2 Character Arrays exq and act and returns True/False
  !
  !!!!!!!!!! WARNING: Experimental -- may not work correctly !!!!!!!!!!!!!
  !
  logical function assert_equalas(exq, act, subname, optmsg) result(succeed)
    character(len=*), dimension(:), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    integer :: i, isiz

    succeed = .true.
    isiz = size(exq)
    if (isiz .ne. size(act)) then
      succeed = .false.
      write(msg, prefix_fmt_test_fail//'"Sizes of Character arrays differ")')
      call write_msg_optmsg(msg, optmsg)
      return
    end if
    
    do i=1, isiz
      if (trim(exq(i)) .ne. trim(act(1))) then
        succeed = .false.
        write(msg, prefix_fmt_test_fail//'"At least a pair of the elements of Character arrays differ")')
        call write_msg_optmsg(msg, optmsg)
      end if
    end do
  end function assert_equalas

  ! Compare Integer4(exq) with Integer1(act) and returns True/False
  logical function assert_equalii(exq, act, subname, optmsg) result(succeed)
    integer(kind=1), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg

    if (present(optmsg)) then
      succeed = assert_equalj(int(exq), int(act), subname, optmsg)
    else
      succeed = assert_equalj(int(exq), int(act), subname)
    end if
  end function assert_equalii

  ! Compare Integer4(exq) with Integer1(act) and returns True/False
  logical function assert_equalij(exq, act, subname, optmsg) result(succeed)
    integer(kind=1), intent(in) :: exq
    integer(kind=4), intent(in) :: act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg

    if (present(optmsg)) then
      succeed = assert_equalj(int(exq), act, subname, optmsg)
    else
      succeed = assert_equalj(int(exq), act, subname)
    end if
  end function assert_equalij

  ! Compare Integer4(exq) with Integer1(act) and returns True/False
  logical function assert_equalji(exq, act, subname, optmsg) result(succeed)
    integer(kind=4), intent(in) :: exq
    integer(kind=1), intent(in) :: act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg

    if (present(optmsg)) then
      succeed = assert_equalj(exq, int(act), subname, optmsg)
    else
      succeed = assert_equalj(exq, int(act), subname)
    end if
  end function assert_equalji

  ! Compare 2 Integers exq and act and returns True/False
  logical function assert_equalj(exq, act, subname, optmsg) result(succeed)
    integer, intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (exq == act) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'i6, " != ", i6)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_equalj

  ! Compare 2 Integer arrays exq and act and returns True/False
  logical function assert_equalja(exq, act, subname, optmsg) result(succeed)
    integer, dimension(:), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (all(exq == act)) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'i6, " != ", i6)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_equalja

  ! Compare 2 Integer(1) arrays exq and act and returns True/False
  logical function assert_equalba(exq, act, subname, optmsg) result(succeed)
    integer(kind=1), dimension(:), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (all(exq == act)) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'i6, " != ", i6)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_equalba

  ! Compare exqected Int_4 arrays with actual Int_1 and returns True/False
  !logical function assert_equaljba(exq, act, subname, optmsg, is_unsigned) result(succeed)
  logical function assert_equaljba(exq, act, subname, optmsg) result(succeed)
    integer,         dimension(:), intent(in) :: exq
    integer(kind=1), dimension(:), intent(in) :: act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    !logical,          intent(in), optional :: is_unsigned  ! For some reason, if this option is unspecified by caller, SIGSEGV is raised.
    character(len=1024) :: fmt !, msg

    succeed = .true.
    !if (present(is_unsigned) .and. (is_unsigned)) then
    !  ! Convert the 1-byte Integer as a byte Array into Integer*4
    !  if (all(exq == unsigned1_to_int4(act))) return
    !else
      if (all(exq == act)) return
    !end if

    succeed = .false.
    fmt = prefix_fmt_test_fail//'a)'
    if (present(optmsg) .and. (len(trim(optmsg)) > 0)) then
      write(*, fmt) trim(subname), optmsg
    else
      write(*, fmt) trim(subname), ''
    end if
    print *, exq, ' !='
    print *, act
    ! call write_msg_optmsg(msg, optmsg)
  end function assert_equaljba

  ! Compare exqected Int_4 arrays with actual Int_1 and returns True/False
  !
  ! For some reason, SIGABRT ("malloc(): invalid size (unsorted)") is raised
  ! if is_unsigned is the last one and optional.  So, I change it this way...
  logical function assert_equaljua(exq, act, subname, is_unsigned, optmsg) result(succeed)
    integer,         dimension(:), intent(in) :: exq
    integer(kind=1), dimension(:), intent(in) :: act
    character(len=*), intent(in) :: subname
    logical,          intent(in) :: is_unsigned
    character(len=*), intent(in), optional :: optmsg

    integer, dimension(size(act)) :: act4
    character(len=1024) :: fmt !, msg

    succeed = .true.
    if (is_unsigned) then
      ! Convert the 1-byte Integer as a byte Array into Integer*4
      act4 = unsigned1_to_int4(act)
      if (all(exq == act4)) return
      ! if (all(exq == unsigned1_to_int4(act))) return  ! This causes SIGABRT
    else
      if (all(exq == act)) return
    end if

    succeed = .false.
    fmt = prefix_fmt_test_fail//'a)'
    if (present(optmsg) .and. (len(trim(optmsg)) > 0)) then
      write(*, fmt) trim(subname), optmsg
    else
      write(*, fmt) trim(subname), ''
    end if
    print *, exq, ' !='
    print *, act
    ! call write_msg_optmsg(msg, optmsg)
  end function assert_equaljua

  !-----------------------------------------
  ! assert_smaller_than  ("Actual" must be smaller than "Expected")
  !-----------------------------------------
  
  ! Compare 2 Integers exq and act and returns True/False
  logical function assert_smaller_thanjj(exq, act, subname, optmsg) result(succeed)
    integer, intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (exq > act) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'i8, " is NOT >", i8)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_smaller_thanjj

  ! Compare 2 Integers exq and act and returns True/False
  logical function assert_smaller_thandd(exq, act, subname, optmsg) result(succeed)
    real(dp), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (exq > act) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'es16.9, " is NOT > ", es16.9)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_smaller_thandd

  !-----------------------------------------
  ! assert_greater_than  ("Actual" must be greater than "Expected")
  !-----------------------------------------
  
  ! Compare 2 Integers exq and act and returns True/False
  logical function assert_greater_thanjj(exq, act, subname, optmsg) result(succeed)
    integer, intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (exq < act) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'i8, " is NOT <", i8)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_greater_thanjj

  ! Compare 2 Integers exq and act and returns True/False
  logical function assert_greater_thandd(exq, act, subname, optmsg) result(succeed)
    real(dp), intent(in) :: exq, act
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (exq < act) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail//'es16.9, " is NOT < ", es16.9)') trim(subname), exq, act
    call write_msg_optmsg(msg, optmsg)
  end function assert_greater_thandd

  !-----------------------------------------
  ! assert_in_delta
  !-----------------------------------------
  
  logical function assert_in_deltarr(exq, act, delta, subname, optmsg) result(succeed)
    real, intent(in) :: exq, act, delta
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = assert_in_deltadd(real(exq, kind=dp), real(act, kind=dp), real(delta, kind=dp), subname, optmsg)
    !succeed = .true.
    !if (abs(exq-act) .le. abs(delta)) return
    !succeed = .false.
    !write(msg, prefix_fmt_test_fail//'es16.9, " != ", es16.9)') trim(subname), exq, act
    !call write_msg_optmsg(msg, optmsg)
  end function assert_in_deltarr

  logical function assert_in_deltadd(exq, act, delta, subname, optmsg) result(succeed)
    real(dp), intent(in) :: exq, act, delta
    character(len=*), intent(in) :: subname
    character(len=*), intent(in), optional :: optmsg
    character(len=1024) :: msg

    succeed = .true.
    if (abs(exq-act) .le. abs(delta)) return

    succeed = .false.
    write(msg, prefix_fmt_test_fail &
       //'es16.9, " !~ ", es16.9, " (Delta(Expected)=", es16.9, " < ", es16.9, ")")') &
       trim(subname), exq, act, delta, abs(exq-act)
    call write_msg_optmsg(msg, optmsg)
  end function assert_in_deltadd
end module unittest

