! Fortran general utility routines
!
module fort_util
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  implicit none 

  real(kind=8), parameter, public :: PI = atan(1.0d0)*4
  !          == 3.141592653589793116 (in REAL64)
  ! True value: 3.1415926535897932384626433832795028841971693993751...

  ! Convert a (Scalar/Array) Integer*1 as a byte (namely unsigned) into the (default) Integer*4
  !
  ! DESCRIPTION:
  !   The integers in the range (0:127) are the same.
  !   (-127:-1) must be added with 256 (e.g., -1_1 is 255 in byte).
  interface unsigned1_to_int4
    module procedure unsigned1_to_int4_scalar, unsigned1_to_int4_array
  end interface unsigned1_to_int4

  interface dump_int_binary
    module procedure dump_int1_binary, dump_int4_binary
  end interface dump_int_binary

  interface dump_int_lowest_8bits
    module procedure dump_int1_lowest_8bits, dump_int4_lowest_8bits
  end interface dump_int_lowest_8bits

  interface int4_to_unsigned1
    module procedure int4_to_unsigned1_scalar
  end interface int4_to_unsigned1

  interface btest_int4_as_1byte
    module procedure btest_int4_as_1byte
  end interface btest_int4_as_1byte
contains

  ! Radian to degree
  real(kind=8) function rad2deg(rad)
    real(kind=8), intent(in) :: rad

    !PI = 3.141592653589793238462643383279
    !PI = 3.141592653589793238462643383279502884197169399375105820974944592307816406286 
    rad2deg = rad * 180.0d0/PI
  end function rad2deg

  ! Radian to degree
  real(kind=8) function deg2rad(deg)
    real(kind=8), intent(in) :: deg

    !PI = 3.141592653589793238462643383279
    !PI = 3.141592653589793238462643383279502884197169399375105820974944592307816406286 
    deg2rad = deg * PI/180.0d0
  end function deg2rad

  ! Returns a character of left-adjusted integer (with prefix if specified)
  !
  ! if len_trim(prefix) is too long, the returned character is an error message!
  !
  ! TODO: This should be a subroutine, using intent(inout), so the character length
  !       would be taken care of by the caller.
  function ladjusted_int(i, prefix) result(retchar)
    integer, parameter :: Len_max_int = 20  ! Maximum length of chars required for Integer*64
      ! NOTE: if you change this value, make sure to change the value in write() in the code below.
    integer, parameter :: Len_max_prefix = 256  ! Maximum length of len_trim(prefix)
    character(len=*), parameter :: Error_msg_prefix = 'ERROR - len_trim(prefix) is too long: Int='

    integer, intent(in) :: i  ! Integer to be left-adjusted
    character(len=*), intent(in), optional :: prefix  ! eg., if 'TTYPE', returns, eg., 'TTYPE5'; max 256 chars

    character(len=Len_max_int) :: chari
    character(len=Len_max_prefix+Len_max_int) :: retchar ! OK up to Integer*64 for a length of trim(prefix) of 256

    write(chari, '(I20)') i
    chari = adjustl(chari)
    if (present(prefix)) then
      if (len_trim(prefix) > Len_max_prefix) then
        retchar = Error_msg_prefix//trim(chari)
      else
        retchar = trim(prefix)//trim(chari)
      end if
    else
      retchar = trim(chari)
    end if
  end function ladjusted_int


  !-----------------------------------------
  ! unsigned1_to_int4
  !-----------------------------------------

  ! Convert a Scalar Integer*1 as a byte (namely unsigned) into the default Integer*4
  integer function unsigned1_to_int4_scalar(ibyte) result(iret)
    integer(kind=1), intent(in) :: ibyte

    iret = -9999
    if (ibyte < 0) then
      iret = ibyte + 256
    else
      iret = ibyte
    end if
  end function unsigned1_to_int4_scalar

  ! Convert an Array Integer*1 as a byte (namely unsigned) into the default Integer*4
  function unsigned1_to_int4_array(ibytes) result(irets)
    integer(kind=1), dimension(:), intent(in) :: ibytes
    integer,         dimension(size(ibytes))  :: irets  ! return
    integer :: i

    do i=1, size(ibytes)
      irets(i) = unsigned1_to_int4_scalar(ibytes(i))
    end do
  end function unsigned1_to_int4_array

  !-----------------------------------------
  ! dump_int_binary
  !-----------------------------------------

  ! Returns a character(len=8) of a binary for the given Integer*1
  !
  ! == Examples ==
  !
  !   ch = dump_int1_binary(127_1) ! => '01111111'
  !   ch = dump_int1_binary(-1_1)  ! => '11111111'
  function dump_int1_binary(i1) result(retc)
    integer(kind=1), intent(in) :: i1
    character(len=8) :: retc
    integer :: j, k

    retc(:) = '00000000'
    do j=0, 7
      k = 8-j
      if (btest(i1, j)) retc(k:k) = '1'
    end do
  end function dump_int1_binary
    
  ! Returns a character(len=8) of all 32 bits (8 bits x 4) of the given Integer*4
  !
  ! == Examples ==
  !
  !   ch = dump_int1_binary(127_4) ! => '00000000_00000000_00000000_01111111'
  !   ch = dump_int1_binary(383_4) ! => '00000000_00000000_00000001_01111111' ! 383 == 256+127
  !   ch = dump_int1_binary(-1_1)  ! => '11111111_11111111_11111111_11111111'
  function dump_int4_binary(i4) result(retc)
    integer(kind=4), intent(in) :: i4
    character(len=8), dimension(4) :: tmpc
    character(len=35) :: retc
    integer :: ir, j, k

    tmpc(:) = '00000000'
    do ir=1, 4
      do j=0, 7
        k = 8-j
        if (btest(i4, j+(ir-1)*8)) tmpc(ir)(k:k) = '1'
      end do
    end do
    retc = tmpc(4)//'_'//tmpc(3)//'_'//tmpc(2)//'_'//tmpc(1)
  end function dump_int4_binary
    
  !-----------------------------------------
  ! dump_int_lowest_8bits
  !-----------------------------------------

  ! Returns a character(len=8) of the lowest 8 bits of the given Integer*1
  !
  ! This (for Integer*1) is identical to dump_int1_binary(i1)
  !
  function dump_int1_lowest_8bits(i) result(retc)
    integer(kind=1), intent(in) :: i
    character(len=8) :: retc

    retc = dump_int1_binary(i)
  end function dump_int1_lowest_8bits

  ! Returns a character(len=8) of the lowest 8 bits of the given Integer*4
  !
  ! The algorithm is identical to dump_int1_binary(i1)
  !
  ! == Examples ==
  !
  !   ch = dump_int4_lowest_8bits(127_4) ! => '01111111'
  !   ch = dump_int4_lowest_8bits(383_4) ! => '01111111' ! 383 == 256+127
  !   ch = dump_int4_lowest_8bits(-1_4)  ! => '11111111'
  !   ch = dump_int4_lowest_8bits(-1_4, ensure_range=.true.)  ! => 'UNDEF'
  function dump_int4_lowest_8bits(i, ensure_range) result(retc)
    integer(kind=4), intent(in) :: i
    logical, intent(in), optional :: ensure_range
    character(len=8) :: retc
    integer :: j, k

    if (present(ensure_range)) then
      if (ensure_range .and. ((i < 0) .or. (i > 255))) then
        retc = 'UNDEF'
        return
      end if
    end if

    retc = '00000000'
    do j=0, 7
      k = 8-j
      if (btest(i, j)) retc(k:k) = '1'
    end do
  end function dump_int4_lowest_8bits

  !-----------------------------------------
  ! int4_to_unsigned1
  !-----------------------------------------

  ! Convert a Scalar Integer*4 into the Integer*1 AS A BYTE (namely unsigned)
  !
  ! Make sure the given argument is between 0 and 255.
  !
  ! The difference from  int(i, kind=1)  is how to convert the Integer*4
  ! number below 0 and between 128 and 255. This abnormally exits with the former
  ! whereas the standard int() would raise an error or warning with the latter.
  ! Also, this EXIT(1) when the argument is out of range.
  function int4_to_unsigned1_scalar(i4) result(ibyte)
    integer, intent(in) :: i4
    integer(kind=1) :: ibyte

    if ((i4 < 0) .or. (255 < i4)) then
      write(stderr, &
         '("ERROR(int4_to_unsigned1_scalar): Value=(",I11,") is out of the range [0:255] to convert.")') i4
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if

    if (i4 > 127) then
      ibyte = int(i4 - 256, kind=1)
    else
      ibyte = int(i4, kind=1)
    end if
  end function int4_to_unsigned1_scalar

  !-----------------------------------------
  ! btest_int4_as_1byte
  !-----------------------------------------

  ! BTEST(int4, pos_from_most_significant) 
  !
  ! Fortran95 standard BTEST() for Integer*4, where the position
  ! is counted from the most significant bit of a single byte,
  ! as opposed to the standard BTEST(), in which pos is counted
  ! from the least significant. The argument pos starts from 0.
  ! NOTE that the Integer*4 is treated as unsigned; the valid range
  ! is therefore [1:255], as opposed to the standard [-127:127].
  ! This EXIT(1) when the argument is out of range.
  !
  ! == Examples ==
  !
  !   MY_BTEST(  1, 7) ! TRUE  (b00000001)
  !                                     ^
  !   MY_BTEST(  2, 7) ! FALSE (b00000010)
  !                                     ^
  !   MY_BTEST(  2, 6) ! TRUE  (b00000010)
  !                                    ^ 
  !   MY_BTEST(254, 7) ! FALSE (b11111110)
  !                                     ^
  !   MY_BTEST(255, 7) ! TRUE  (b11111111)
  !                                     ^
  logical function btest_int4_as_1byte(i4, pos_reverse)
    integer, intent(in) :: i4, pos_reverse

    if ((i4 < 0) .or. (255 < i4)) then
      write(stderr, &
         '("ERROR(int4_to_unsigned1_scalar): Value=(",I11,") is out of the range [0:255] to convert.")') i4
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if

    btest_int4_as_1byte = btest(int4_to_unsigned1_scalar(i4), 7-pos_reverse)
  end function btest_int4_as_1byte

  ! Similar to btest_int4_as_1byte() but returns Integer (0 or 1)
  integer function btest2int_int4_as_1byte(i4, pos_reverse) result(iret)
    integer, intent(in) :: i4, pos_reverse

    iret = 0
    if (btest_int4_as_1byte(i4, pos_reverse)) iret = 1
  end function btest2int_int4_as_1byte

  !-----------------------------------------
  ! basename
  !-----------------------------------------

  ! Return the basename of the given filename (with the same len())
  function basename(fname) result(retchar)
    character(len=1), parameter :: DELIMETER = '/'
    character(len=*), intent(in) :: fname
    character(len=len(fname)) :: retchar

    integer :: pos

    retchar = fname
    pos = scan(trim(retchar), DELIMETER, BACK=.true.)
    if (pos .le. 0) return  ! No delimiter is found.
    retchar = fname(pos+1:len_trim(retchar))
  end function basename
end module fort_util

