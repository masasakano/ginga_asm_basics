! Auxiliary routines for handling ASM-related data
!
module asm_aux
  use asm_fits_common
  implicit none 

  ! Get a word or bit in a frame, like F16W14B3
  interface get_frame_word
    module procedure get_frame_word_bit, get_frame_word_nobit
  end interface get_frame_word

  ! Sabu-frame (2-bits) and frame (6-bits) numbers in INTEGER*1 (signed 1-byte Integer)
  interface get_sf4_fr64
    module procedure get_sf4_fr64_telem, get_sf4_fr64_unsigned1 !, get_sf4_fr64_int4
  end interface get_sf4_fr64

  !! Get a word or bit in a frame, like F16W14B3
  !interface get_frame_word
  !  integer function get_frame_word_bit(irow, telems, iframe, iword, ibit)
  !    implicit none
  !    integer, intent(in) :: irow, iframe, iword, ibit
  !    integer(kind=1), dimension(:, :), intent(in) :: telems
  !  end function get_frame_word_bit

  !  integer function get_frame_word_nobit(irow, telems, iframe, iword)
  !    implicit none
  !    integer, intent(in) :: irow, iframe, iword
  !    integer(kind=1), dimension(:, :), intent(in) :: telems
  !  end function get_frame_word_nobit
  !end interface get_frame_word

contains
  ! Get a word or bit in a frame, like F16W14B3
  !!interface get_frame_word
  ! Returns (signed) Integer. If negative, the frame does not exist.
  integer function get_frame_word_nobit(irow, telems, iframe, iword) !result(ret)
    implicit none
    integer, intent(in) :: irow          ! starts from 1.
    integer(kind=1), dimension(:, :), intent(in) :: telems
    integer, intent(in) :: iframe, iword ! starts from 0.

    integer(kind=1), dimension(size(telems, 1)) :: cur_telem
    integer,         dimension(nframes_per_sf) :: frame_nums
    integer :: nframes  ! Number of rows/frames in this SF. eg., W2 means the 3rd word (=byte).
    integer :: crow

    call get_frame_nums(irow, telems, frame_nums, nframes)
    cur_telem(:) = telems(irow, :)
    crow = findloc(frame_nums, iframe, DIM=1)
    if (crow .eq. 0) then
      get_frame_word_nobit = -1
      return
    end if

    get_frame_word_nobit = telems(crow, iword+1)
    return
  end function get_frame_word_nobit

  ! Returns 0 or 1. If negative, the frame does not exist.
  integer function get_frame_word_bit(irow, telems, iframe, iword, ibit)
    implicit none
    integer, intent(in) :: irow          ! starts from 1.
    integer(kind=1), dimension(:, :), intent(in) :: telems
    integer, intent(in) :: iframe, iword, ibit ! starts from 0.

    integer :: iret
    integer(kind=1) :: cword

    iret = get_frame_word_nobit(irow, telems, iframe, iword)
    if (iret .lt. 0) then
      get_frame_word_bit = iret
      return
    end if

    cword = iret
    get_frame_word_bit = 0
    if (btest(iret, 7 - ibit)) get_frame_word_bit = 1
    return
  end function get_frame_word_bit
  !!end interface get_frame_word

  ! Returns ARRAY(irow) => FrameNo
  subroutine get_frame_nums(irow, telems, frame_nums, nframes)
    implicit none
    integer, intent(in) :: irow
    integer(kind=1), dimension(:, :), intent(in) :: telems
    integer,         dimension(nframes_per_sf), intent(out) :: frame_nums
    integer, intent(out) :: nframes  ! Number of rows/frames in this SF.

    integer(kind=1) :: sf4_orig, sf4, fr64
    integer :: i

    nframes = 0 
    frame_nums(:) = 129  ! Larger than 63 means undefined.
    call get_sf4_fr64(irow, telems, sf4_orig, fr64)
    do i=irow, irow+63
      call get_sf4_fr64(irow, telems, sf4, fr64)
      if (sf4_orig .ne. sf4) exit
      nframes = nframes + 1
      frame_nums(nframes) = fr64
    end do
  end subroutine get_frame_nums

  !---------------------------
  ! INTERFACE: get_sf4_fr64
  !---------------------------

  ! Gets current sabu-frame (between 0..3) and frame (0..63) numbers
  subroutine get_sf4_fr64_telem(irow, telems, sf4, fr64)
    implicit none
    integer, intent(in) :: irow
    integer(kind=1), dimension(:, :), intent(in) :: telems
    integer(kind=1), intent(out) :: sf4, fr64

    call get_sf4_fr64_unsigned1(telems(w_no('fi', from1=.true.), irow), sf4, fr64)  ! FI (W3)
  end subroutine get_sf4_fr64_telem

  ! Gets current sabu-frame (between 0..3) and frame (0..63) numbers
  subroutine get_sf4_fr64_unsigned1(ibyte, sf4, fr64)
    implicit none
    integer(kind=1), intent(in)  :: ibyte
    integer(kind=1), intent(out) :: sf4, fr64

    fr64 = ibyte
    sf4  = fr64
    sf4  = ISHFT(sf4, -6) ! sabu-frame of (0..3)
    fr64 = IBCLR(fr64, 7)
    fr64 = IBCLR(fr64, 6) ! Now, fr64 is the frame index number
  end subroutine get_sf4_fr64_unsigned1
 
  !---------------------------
  ! INTERFACE: get_bit_from_word
  !---------------------------

  ! Returns (Integer*4) 1 or 0 of a particular bit of the given (Integer*1) word
  integer function get_bit_from_word_int1(word, pos) result(iret)
    implicit none
    integer(kind=1), intent(in)  :: word, pos

    iret = 0
    if (btest(iret, 7 - pos)) iret = 1  ! btest() from Fortran95
  end function get_bit_from_word_int1

  ! Returns (Integer*4) 1 or 0 of a particular bit of the given (Integer*4) word
  integer function get_bit_from_word_int4(word, pos) result(iret)
    implicit none
    integer(kind=4), intent(in)  :: word, pos

    iret = 0
    if (btest(iret, 7 - pos)) iret = 1  ! btest() from Fortran95
  end function get_bit_from_word_int4
end module asm_aux
