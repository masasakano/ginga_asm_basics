!! Subroutines for exit due to an error
!module err_exit
!  implicit none 
!
!  !real, parameter, public :: pi = 3.1415926536  
!  !real, parameter, public :: e = 2.7182818285 
!   
!contains
!  ! If FITSIO status is non-zero, exit(1)
!  subroutine err_exit_if_status(status, usermsg)
!    implicit none
!
!    character(len=*), parameter :: prefix = 'FITSIO-status is '
!
!    integer,          intent(in) :: status
!    character(len=*), intent(in) :: usermsg  ! User Error message.
!    character(len=30) :: errtext
!    character(len=1024) :: sout
!
!    if (status .ne. 0) then
!      call FTGERR(status, errtext)
!      write(sout, '(a, i8.2, " (", a, ") ", a)') prefix, status, trim(errtext), trim(usermsg)
!      call err_exit_with_msg(sout)
!      return ! redundant
!    end if
!  end subroutine err_exit_if_status
!
!  ! exit(1)
!  subroutine err_exit_with_msg(usermsg)
!    use iso_fortran_env, only : stderr=>ERROR_UNIT   
!    implicit none
!    character(len=*), parameter :: prefix = 'ERROR: '
!    character(len=*), intent(in) :: usermsg  ! User Error message.
!
!    write(stderr,'(a, a)') prefix, trim(usermsg)
!    call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
!    ! stop 1      ! not yet standard? (works for gfortran?)
!    return ! redundant
!  end subroutine err_exit_with_msg
!end module err_exit

!module asm_fits_common
!  implicit none 
!
!  integer, parameter :: dp = kind(1.d0)
!  integer, parameter :: n_all_fields = 10  ! Number of fields to read and output
!  integer, parameter, public :: max_fits_char = 68
!  integer, parameter, public :: nchar_extname = 8
!  integer, parameter, public :: nframes_per_sf = 64  ! Number of Frames per SABU-Frame
!  integer, parameter, public :: num_instr = 6, nchan_pha = 16, nchan_time = 8  ! Number of channels in each mode 
!  integer, parameter, public :: nwords_main = num_instr*nchan_pha  ! Number of words (=bytes) of Main (mode-dependent) section in a frame
!  character(len=*), parameter, public :: tunit_main = 'count'
!  integer, parameter, public :: tfields_asm = 104  !!!!=========== Check!
!
!  character(len=max_fits_char), dimension(n_all_fields) :: &
!     tmtypes, tmcomms, tmforms, tmunits
!
!  character(len=max_fits_char), dimension(:, :), allocatable :: tmcolss  ! (i-th-column(1:n_all_fields), i-th-row)
!  integer(kind=1),              dimension(:, :), allocatable :: tmcolsi
!  integer,                      dimension(:, :), allocatable :: tmcolsj
!  real(kind=dp),                dimension(:, :), allocatable :: tmcolsd
!  character(len=max_fits_char), dimension(:),    allocatable :: outcolss ! (i-th-row)
!  integer(kind=1),              dimension(:),    allocatable :: outcolsi
!  integer,                      dimension(:),    allocatable :: outcolsj
!  real(kind=dp),                dimension(:),    allocatable :: outcolsd
!
!  integer, private :: i
!
!  DATA tmtypes(1:2) / 'SF_NO2B', 'FRAME_NO' /
!
!!* tmtypes / (TeleMetry-TYPES) Field names derived from Telemetry
!!* tmcomms / (TeleMetry-COMMentS) Comments for outputs for Field names that is derived from Telemetry 
!!* tmforms / (TeleMetry-FORMS) FORMAT to output (and input)
!!* tmunits / (TeleMetry-UNITS) Unit to output
!!* tmnframes / (TeleMetry-I-th-FRAMES) Which n-th frame in n+offset (1 may mean all), eg., 16 for 24n+3 (like 3, 27, 51)
!!* tmoframes / (TeleMetry-I-th-FRAMES) Which offset frame in n+offset (0 may mean all), eg., 3 for 24n+3 (like 3, 27, 51)
!!* tmiiwords / (TeleMetry-I-th-Initial-WORDS) Starting at which word (1..128)
!!* tmifwords / (TeleMetry-I-th-Final-WORDS) End at which word (1..128)
!!* tmiibits / (TeleMetry-I-th-Initial-BITS) Starting at which bit (1..8) (0 means all)
!!* tmifbits / (TeleMetry-I-th-Initial-BITS) End at which bit (1..8) (0 means all)
!!* tmcols / (TeleMetry-COLumnS) Type
!
!  type asm_header
!    integer :: i_sframe, i_frame;
!    character(len=18) :: title    = 'ASM extracted data';
!    character(len=5)  :: telescop = 'Ginga';
!    character(len=3)  :: instrume = 'ASM';
!    character(len=max_fits_char) :: filename, date_obs;
!    real    :: tsttasmm, tendasmm, tstts360, tends360;
!    logical :: existdat;
!    real, dimension(3) :: euler_s, euler_e;
!    integer :: mode_dp;  ! 1/0 if ASM Mode is 'ON/OFF' (F4W66B3)
!    integer :: mode_asm; ! 1/0 if ASM-PHA/Time Mode    (F4W66B4), ignoring F56W66B4
!    character(len=max_fits_char) :: stat_asm, stat_asa, stat_amc;           ! ON/OFF
!    character(len=max_fits_char) :: stat_hv1, stat_hv2, stat_rbm, stat_bdr; ! ENA/DIS
!    integer :: bitrate;
!  end type asm_header
!
!  type t_asm_header_comment
!    character(len=max_fits_char) :: title = '', telescop = '', instrume = '';
!    character(len=17) :: date_obs = '[day] Observation date';
!    character(len=17) :: filename = 'Original filename';
!    character(len=22) :: tsttasmm = 'Start time of ASM Mode';
!    character(len=20) :: tendasmm = 'End time of ASM Mode';
!    character(len=26) :: tstts360 = 'Start time of Slew360 Mode';
!    character(len=24) :: tends360 = 'End time of Slew360 Mode';
!    character(len=30) :: existdat = 'True if the file contains data';
!    character(len=37) :: fmt_euler_s  = '("[deg] Euler ", i1, " at the start")';
!    character(len=35) :: fmt_euler_e  = '("[deg] Euler ", i1, " at the end")';
!    character(len=35) :: mode_dp  = '1/0 if ASM Mode is ON/OFF (F4W66B3)';
!    character(len=34) :: mode_asm = '1/0 if ASM-PHA/Time Mode (F4W66B4)';
!    character(len=23) :: stat_asm = 'ON/OFF for ASM F15W65B1';
!    character(len=24) :: stat_asa = 'ON/OFF for ASMA F15W65B2';
!    character(len=23) :: stat_amc = 'ON/OFF for AMC F15W65B3';
!    character(len=24) :: stat_hv1 = 'ENA/DIS for HV1 F15W65B4';
!    character(len=24) :: stat_hv2 = 'ENA/DIS for HV2 F15W65B5';
!    character(len=24) :: stat_rbm = 'ENA/DIS for RBM F15W65B6';
!    character(len=24) :: stat_bdr = 'ENA/DIS for BDR F15W65B7';
!    character(len=32) :: bitrate  = '[/s] Telemetry bit rate (F16W66)';
!  end type t_asm_header_comment
!
!  type(t_asm_header_comment), parameter :: asm_comm = t_asm_header_comment()
!
!  integer, parameter :: tfields_def = 9  ! TFIELDS in the output FITS
!
!  ! ASM Table as read from a Telemetry file; row-based, ie., each row constitutes 1 variable
!  type asm_table_row
!    real    :: tstart;
!    integer(kind=1) :: sf_2bit;   ! 2-bit info of the SF in Telemetry
!    integer(kind=1) :: i_frame;   ! i-th frame in the current SF
!    integer(kind=1) :: cur_fr64;  ! Current frame number as recorded in Telemetry
!    real, dimension(3) :: eulers, angles_intn;  ! Internal angle x,y,z
!    integer(kind=1), dimension(3) :: acss;      ! acss = 3 bytes
!    integer(kind=1), dimension(nwords_main) :: arasm;    ! Main ASM data; Y[1-6](ch[0-15]) for PHA mode, Y[1-6][LH][0-7] for Time mode
!  end type asm_table_row
!
!  ! ASM Table to output as a FITS (i.e., column-based)
!  !
!  ! (x,y) is (row-number, internal-number); e.g., eulerss(9, 2) is the 9th row of Euler_2, and eulerss(:, 2) is the column array of Euler_2
!  ! For arasms, dimension(:, nwords_main)
!  ! NOTE: it is faster for Fortran to access eulerss(:, 2) than eulerss(1, :)
!  type asm_table_col
!    real,            dimension(:, :), allocatable :: tstarts;
!    integer,         dimension(:, :), allocatable :: sfs;  ! i-th SF in the FRF
!    integer(kind=1), dimension(:, :), allocatable :: sf_2bits;  ! 2-bit info of the SF in Telemetry
!    integer(kind=1), dimension(:, :), allocatable :: i_frames;  ! i-th frame in the current SF
!    integer(kind=1), dimension(:, :), allocatable :: cur_fr64s; ! Current frame number as recorded in Telemetry
!    real,            dimension(:, :), allocatable :: eulerss, &
!                                                     angles_intns;  ! Internal angle x,y,z
!    integer(kind=1), dimension(:, :), allocatable :: acsss;     ! acss = 3 bytes
!    integer(kind=1), dimension(:, :), allocatable :: arasms;    ! Main ASM data; Y[1-6](ch[0-15]) for PHA mode, Y[1-6][LH][0-7] for Time mode
!  end type asm_table_col
!
!  ! Specification of the ASM Table Types in FITS
!  type form_unit
!    character(len=max_fits_char) :: form;
!    character(len=max_fits_char) :: unit;
!  end type form_unit
!
!  ! Specification of the ASM Table Types in FITS
!  type asm_index
!    integer(kind=1) :: tstart   = 1;
!    integer(kind=1) :: sf_2bit  = 2;
!    integer(kind=1) :: i_frame  = 3;
!    integer(kind=1) :: cur_fr64 = 4;
!    integer(kind=1), dimension(3) :: eulers = [5, 6, 7];
!    integer(kind=1), dimension(3) :: angles_intn = [8, 9, 10];  ! Internal angle x,y,z
!    integer(kind=1), dimension(3) :: acss   = [11, 12, 13];      ! acss = 3 bytes
!    integer(kind=1), dimension(nwords_main) :: arasm = (/ (I,I=14,14+nwords_main-1) /);	! Array
!  end type asm_index
!
!  ! Specification of the ASM Table in FITS
!  type asm_table_spec
!    type(asm_index) :: i = asm_index();
!    type(form_unit) :: tstart   = form_unit('1D', 'MJD');
!    type(form_unit) :: sf_2bit  = form_unit('1I', '');
!    type(form_unit) :: i_frame  = form_unit('1I', '');
!    type(form_unit) :: cur_fr64 = form_unit('1I', '');
!    type(form_unit), dimension(3) :: eulers = form_unit(form='1E', unit='deg');
!    !type(form_unit), dimension(3) :: eulers = form_unit('1E', 'deg');
!    type(form_unit), dimension(3) :: angles_intn = form_unit('1B', 'deg');  ! Internal angle x,y,z
!    type(form_unit), dimension(3) :: acss = form_unit('1B', 'binary');      ! acss = 3 bytes
!    type(form_unit), dimension(nwords_main) :: asms = form_unit('1I', 'count');	! Array
!  end type asm_table_spec
!
!  integer, parameter, private :: tform_len = 16  ! maximum character length for TFORM (nb., 14 for 'Y23CH15/Y23H07')
!
!contains
!
!  ! Return the string for TFORM for the given index [1:] (eg, TFORM5).
!  !
!  ! The first 96 (TFORM1--TFORM96) is for the ASM data like TFORM96='Y23CH15/Y23H07',
!  !   TFORM1= 'Y11CH00/Y11L00',
!  !   TFORM17='Y21CH00/Y21L00',
!  !   TFORM32='Y21CH15/Y21H15',
!  !   TFORM33='Y12CH00/Y12L00',
!  !   TFORM96='Y23CH15/Y23H07',
!  ! The first 96 (TFORM1--TFORM96) is for the ASM data like TFORM96='Y23CH15/Y23H07',
!  ! followed by the others like TFORM97='Tstart'
!  character(len=tform_len) function get_tform(index) result(ret)
!    implicit none
!    integer, intent(in) :: index
!
!    !character(len=tform_len), dimension(tfields_def), parameter :: tnames_first = (/&
!    character(len=tform_len), dimension(13), parameter :: tnames_first = (/&
!        'Tstart      '&
!      , 'sf_2bit     '&
!      , 'i_frame     '&
!      , 'cur_fr64    '&
!      , 'Euler1      '&
!      , 'Euler2      '&
!      , 'Euler3      '&
!      , 'angles_intn1'&
!      , 'angles_intn2'&
!      , 'angles_intn3'&
!      , 'acs1        '&
!      , 'acs2        '&
!      , 'acs3        '&
!     /)
!
!    integer :: iy1, ifw, ifwmod, ichp, icht  ! ichp for PHA-mode, icht=mod(ichp, 8) for TIME-mode
!    character(len=1) :: low_high
!
!    if (index .gt. nwords_main) then
!      ret = tnames_first(index - nwords_main)  ! nwords_main = 96
!    else
!      ! Y11, Y21, Y12, Y22, Y13, Y23
!      ifw    =     (index-1)/(nwords_main/3) + 1 ! FW1--FW3
!      ifwmod = mod((index-1), nwords_main/3)
!      iy1    =    ifwmod/(nwords_main/2) + 1 ! Y1--Y2
!      ichp   = mod(ifwmod,nwords_main/3/2)   ! CH0--CH15 for PHA-mode [0-15]
!      icht   = mod(ichp,  nchan_pha/2)       ! CH0--CH07 for TIME-mode [0-7]
!
!      write(ret, '("Y", I1, I1, "CH", I2.2, "/Y", I1, I1, A1, I2.2)') &
!         iy1, ifw, icht, iy1, ifw, low_high, icht
!    end if
!  end function get_tform
!
!
!  ! Returns String like 'ASMSF002' for the num-th extension
!  character(len=nchar_extname) function get_extname(num)
!    integer, intent(in) :: num  ! starts from 1, i.e., num==1 for the 1st extension
!
!    write(get_extname, '("ASMSF", i0.3)') num
!    return
!  end function get_extname
!
!  ! Get ttype character Array
!  subroutine get_ttypes(tret)
!    character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret
!
!    tret(:) = ''
!    tret(1:7) = 'E15.8E2'  ! TSTART, Euler x 2
!    tret(8:) = 'I1'
!  end subroutine get_ttypes
!
!  ! Get tunit character Array
!  subroutine get_tunits(tret)
!    character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret
!
!    tret(:) = ''
!    tret(1) = 's'      ! TSTART
!    tret(2:7) = 'deg'  ! Euler x 2
!    tret(8:) = 'count'
!  end subroutine get_tunits
!
!  ! Get tform character Array
!  subroutine get_tforms(tret)
!    character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret
!    integer :: i, k, ini = 1
!
!    tret(:) = ''
!    tret(ini) = 'Tstart'
!
!    do i=ini+1, ini+3
!      write(tret(i), '("Euler", i1)') i
!      write(tret(i+3), '("F16W", i1)') (i-1)*2+1 ! Internal angle x,y,z
!    end do
!
!    do i=ini+3*2+1, tfields_asm ! to the last one
!      tret(i) = trim(get_tform(i-6-ini))
!    end do
!  end subroutine get_tforms
!
!  ! Convert an Array of asm_table_row into asm_out_col
!  subroutine convert_row2col(rows, colout)
!    type(asm_table_row), dimension(:), allocatable, intent(in) :: rows
!    type(asm_table_col),                            intent(out) :: colout
!    integer :: i, nrows
!
!    nrows = size(rows)
!
!    allocate( colout%tstarts(  nrows, 1) )
!    allocate( colout%sfs(      nrows, 1) )
!    allocate( colout%sf_2bits( nrows, 1) )
!    allocate( colout%i_frames( nrows, 1) )
!    allocate( colout%cur_fr64s(nrows, 1) )
!    allocate( colout%eulerss(  nrows, 3) )
!    allocate( colout%acsss(    nrows, 3) )
!    allocate( colout%arasms(nrows, num_instr*nchan_pha) )
!
!    do i = 1, nrows
!      colout%tstarts(  i, 1) = rows(i)%tstart
!      !colout%sfs(      i, 1) = rows(i)%sf
!      !colout%sf_2bits( i, 1) = rows(i)%sf_2bit
!      !colout%i_frames( i, 1) = rows(i)%
!      colout%cur_fr64s(i, 1) = rows(i)%cur_fr64
!      colout%eulerss(  i, :) = rows(i)%eulers
!      !colout%acsss(    i, :) = rows(i)%acss
!      colout%arasms(   i, :) = rows(i)%arasm
!    end do
!  end subroutine convert_row2col
!
!  ! Get (ttype, tform, tunit) character Array
!  ! Get (ttype, tform, tunit) character Array
!      !  character(len=64), dimension(:), parameter :: ttype
!      !, tform, tunit, extname, varidat, status)
!
!end module asm_fits_common

!module asm_aux
!  use asm_fits_common
!  implicit none 
!
!  interface get_frame_word
!    module procedure get_frame_word_bit, get_frame_word_nobit
!  end interface get_frame_word
!
!  !! Get a word or bit in a frame, like F16W14B3
!  !interface get_frame_word
!  !  integer function get_frame_word_bit(irow, headers, iframe, iword, ibit)
!  !    implicit none
!  !    integer, intent(in) :: irow, iframe, iword, ibit
!  !    integer(kind=1), dimension(:, :), intent(in) :: headers
!  !  end function get_frame_word_bit
!
!  !  integer function get_frame_word_nobit(irow, headers, iframe, iword)
!  !    implicit none
!  !    integer, intent(in) :: irow, iframe, iword
!  !    integer(kind=1), dimension(:, :), intent(in) :: headers
!  !  end function get_frame_word_nobit
!  !end interface get_frame_word
!
!contains
!  ! Get a word or bit in a frame, like F16W14B3
!  !!interface get_frame_word
!  ! Returns (signed) Integer. If negative, the frame does not exist.
!  integer function get_frame_word_nobit(irow, headers, iframe, iword) !result(ret)
!    implicit none
!    integer, intent(in) :: irow          ! starts from 1.
!    integer(kind=1), dimension(:, :), intent(in) :: headers
!    integer, intent(in) :: iframe, iword ! starts from 0.
!
!    integer(kind=1), dimension(size(headers, 1)) :: cur_header
!    integer,         dimension(nframes_per_sf) :: frame_nums
!    integer :: nframes  ! Number of rows/frames in this SF. eg., W2 means the 3rd word (=byte).
!    integer :: crow
!
!    call get_frame_nums(irow, headers, frame_nums, nframes)
!    cur_header(:) = headers(irow, :)
!    crow = findloc(frame_nums, iframe, DIM=1)
!    if (crow .eq. 0) then
!      get_frame_word_nobit = -1
!      return
!    end if
!
!    get_frame_word_nobit = headers(crow, iword+1)
!    return
!  end function get_frame_word_nobit
!
!  ! Returns 0 or 1. If negative, the frame does not exist.
!  integer function get_frame_word_bit(irow, headers, iframe, iword, ibit)
!    implicit none
!    integer, intent(in) :: irow          ! starts from 1.
!    integer(kind=1), dimension(:, :), intent(in) :: headers
!    integer, intent(in) :: iframe, iword, ibit ! starts from 0.
!
!    integer :: iret
!    integer(kind=1) :: cword
!
!    iret = get_frame_word_nobit(irow, headers, iframe, iword)
!    if (iret .lt. 0) then
!      get_frame_word_bit = iret
!      return
!    end if
!
!    cword = iret
!    get_frame_word_bit = 0
!    if (btest(iret, 7 - ibit)) get_frame_word_bit = 1
!    return
!  end function get_frame_word_bit
!  !!end interface get_frame_word
!
!  ! Gets current sabu-frame (between 0..3) and frame (0..63) numbers
!  subroutine get_sf4_fr64(irow, headers, sf4, fr64)
!    implicit none
!    integer, intent(in) :: irow
!    integer(kind=1), dimension(:, :), intent(in) :: headers
!    integer(kind=1), intent(out) :: sf4, fr64
!
!    fr64 = headers(irow, 4)  ! FI (W3)
!    sf4 = fr64
!    sf4  = ISHFT(sf4, -6) ! sabu-frame of (0..3)
!    fr64 = IBCLR(fr64, 7)
!    fr64 = IBCLR(fr64, 6) ! Now, fr64 is the frame index number
!  end subroutine get_sf4_fr64
!        
!  ! Returns ARRAY(irow) => FrameNo
!  subroutine get_frame_nums(irow, headers, frame_nums, nframes)
!    implicit none
!    integer, intent(in) :: irow
!    integer(kind=1), dimension(:, :), intent(in) :: headers
!    integer,         dimension(nframes_per_sf), intent(out) :: frame_nums
!    integer, intent(out) :: nframes  ! Number of rows/frames in this SF.
!
!    integer(kind=1) :: sf4_orig, sf4, fr64
!    integer :: i
!
!    nframes = 0 
!    frame_nums(:) = 129  ! Larger than 63 means undefined.
!    call get_sf4_fr64(irow, headers, sf4_orig, fr64)
!    do i=irow, irow+63
!      call get_sf4_fr64(irow, headers, sf4, fr64)
!      if (sf4_orig .ne. sf4) exit
!      nframes = nframes + 1
!      frame_nums(nframes) = fr64
!    end do
!  end subroutine get_frame_nums
!
!end module asm_aux
!
!module asm_fitsout
!  use err_exit
!  use asm_fits_common
!  implicit none 
!contains
!
!  ! Table5.5.5-6 (pp.233-234)
!  subroutine fill_asm_one(isabu, itotrow, i_fr64, telems, arout)
!    implicit none
!    integer, intent(in) :: isabu, itotrow, i_fr64
!    integer(kind=1), dimension(:, :),  intent(in) :: telems
!    type(asm_table_row), dimension(:, :), intent(inout) :: arout  ! (iSabuFrame, Row)
!
!    integer :: idet, ich, i_tele, i_out
!
!    do idet=1, num_instr  ! =6
!      do ich=1, nchan_time  ! =8
!        i_tele = (idet-1)*2 + (ich-1)*16 + 5
!        i_out = (idet-1)*nchan_pha + ich
!        arout(isabu, i_fr64)%arasm(i_out) = telems(itotrow, i_tele)
!            
!        i_tele = (idet-1)*2 + (ich-1)*16 + 6
!        i_out = (idet-1)*nchan_pha + ich + 8
!        arout(isabu, i_fr64)%arasm(i_out) = telems(itotrow, i_tele)
!      end do
!    end do
!  end subroutine fill_asm_one
!
!  ! Output FITS header in writing the ASM data FITS file
!  subroutine write_asm_fits_header(unit, fitshead, status)
!    implicit none
!    integer, intent(in) :: unit
!    type(asm_header), dimension(:), intent(in) :: fitshead  ! (ExtensionNo), primary is 1
!    integer, intent(out) :: status
!
!    call FTPKYS(unit, 'TITLE',    fitshead%title,    asm_comm%title,    status)
!    call FTPKYS(unit, 'TELESCOP', fitshead%telescop, asm_comm%telescop, status)
!    call FTPKYS(unit, 'INSTRUME', fitshead%instrume, asm_comm%instrume, status)
!    call ftpkys(unit, 'FILENAME', fitshead%filename, asm_comm%filename, status) ! asm_comm defined in asm_fits_common
!    !!call FTPKY[JKLS](funit, 'date', '????', '[day] Creation date of this file', status)
!    ! .........
!  end subroutine write_asm_fits_header
!
!  ! Output FITS table in writing the ASM data FITS file
!  subroutine write_asm_fits_table(unit, tables, status)
!    implicit none
!    type(asm_table_spec), parameter :: tspec = asm_table_spec()
!
!    integer, intent(in) :: unit
!    type(asm_table_col), intent(in) :: tables
!    integer, intent(out) :: status
!
!    integer :: colnum
!
!    !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
!    !! Write elements into an ASCII or binary table column. The ‘felem’ parameter applies only to vector columns in binary tables and is ignored when writing to ASCII tables.
!    colnum = tspec%i%tstart
!    !call FTPCLD(unit, colnum, 1, 1, 2, dvalues, status)  ! colnum = 1
!    !call ftpkys(unit, 'FILENAME', fitshead%filename, asm_comm%filename, status) ! asm_comm defined in asm_fits_common
!    ! .........
!  end subroutine write_asm_fits_table
!
!  ! Output FITS file of the ASM data
!  subroutine write_asm_fits(fname, fitshead, tables, status)
!    implicit none
!    character(len=*), parameter :: extname = 'ASMtable'
!
!    character(len=*), intent(in) :: fname
!    type(asm_header),    dimension(:),    intent(in) :: fitshead  ! (ExtensionNo), primary is 1
!    type(asm_table_col), dimension(:, :), intent(in) :: tables ! (Row)
!    integer, intent(out) :: status
!
!    integer :: unit, bitpix, naxis = 2
!    integer :: blocksize, naxes(2)
!    integer :: group,fpixel,nelements,array(300,200) ! i,j,
!    logical :: simple =.true., extend = .true.
!    integer :: nhdu
!    character(len=80), dimension(2) :: ttypes = (/ 'TIME', 'Char' /), tunits, &
!         tforms = (/ '1D ', '20A' /)  ! 20A20?
!         !tforms = (/ '1D   ', '20A20' /)  ! 20A20?
!         !tforms = (/ '1D   ', '1D   ' /)
!    real(kind=dp), dimension(2) :: dvalues = (/ 12345.6, 7.89 /), d2values = (/ 0.023, 0.0045 /)
!    character(len=20), dimension(2) :: svalues = (/ 'saisho', 'tsugi1' /)
!    character(len=80) :: comment, keyword, snull, msg
!    character(len=30) :: errtext
!
!    bitpix=16  ! signed 2-byte, -32: real, -64: double
!    naxis=2
!
!    ! Get an unused Logical Unit Number to use to create the FITS file
!    call ftgiou(unit, status)
!
!    ! create the new empty FITS file blocksize=1
!    call ftinit(unit, fname, blocksize, status)
!    call FTGERR(status, errtext)
!    call FTGHDN(unit, nhdu)  ! CHDU: Current HDU
!    print *,'DEBUG:write-open1-status=',status,' / HDU=',nhdu,' / ',trim(errtext)
!    call ftphpr(unit,simple,bitpix,0,naxes,0,1,extend,status)
!    !call ftpkyj(unit,'EXPOSURE',1500,'Total Exposure Time',status)
!
!    !!! Extension
!    
!    !ttypes = (/ 'TIME', 'String' /)
!    !tforms = (/ '1D', '20A' /)  ! Double
!    !tunits = (/ 'sec', '' /)
!    tunits(1) = 'sec'
!    tunits(2) = ''
!
!    !call FTIBIN(unit,nrows,tfields,ttype,tform,tunit,extname,varidat > status) ! nrows should be 0
!    call FTIBIN(unit, 0, size(ttypes) &
!              , ttypes, tforms, tunits, extname, .true., status) ! Creates an extension with basic header and moves to it.
!    ! Status check...
!    call FTGHDN(unit, nhdu)
!    call FTGERR(status, errtext)
!    if (status .ne. 0) then
!      print *,'ERROR: Failed to create a table: status=',status,' / HDU=',nhdu,' / ',trim(errtext)
!    end if
!
!    call write_asm_fits_header(unit, fitshead, status)
!
!    ! Write Table (double precision)
!    !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
!    call FTPCLD(unit,1,1,1,2,dvalues, status)  ! colnum = 1
!    call FTGHDN(unit, nhdu)
!    call FTGERR(status, errtext)
!    print *,'DEBUG: dval-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)
!    
!    call ftpkys(unit,'TDIM2','(20,1)','for Character in Binary table',status)
!    call FTPCLS(unit,2,1,1,2,svalues, status)  ! colnum = 2
!    !call FTPCLD(unit,2,1,1,2,d2values, status)
!    call FTGHDN(unit, nhdu)
!    call FTGERR(status, errtext)
!    print *,'DEBUG: char-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)  ! If wrong, 309  / not an ASCII (A) column
!  end subroutine write_asm_fits
!
!
!  ! SF-based old style
!  subroutine out_asm_fits_old(fname, fitshead, ardata, nrows)
!    character(len=*), intent(in) :: fname
!    type(asm_header), dimension(:), intent(in) :: fitshead  ! (iSabuFrame), None for primary header
!    type(asm_table_row),  dimension(:, :), intent(in) :: ardata ! (iSabuFrame, Row)
!    integer,          dimension(:), intent(in) :: nrows     ! Number-of-significant-rows(iSabuFrame)
!
!    !type(asm_header_comment) :: hcomm
!    integer(kind=1),  dimension(maxval(nrows)) :: aryi
!    integer,          dimension(maxval(nrows)) :: aryj
!    real,             dimension(maxval(nrows)) :: arye
!    character(len=max_fits_char), dimension(maxval(nrows)) :: arys
!
!    character(len=1024) :: errmsg
!    character(len=80) :: comment = '', keyword, snull, msg
!    integer :: funit, status=-999, blocksize, hdutype, nframes, naxis1
!    integer :: isabu, varidat, felem
!    integer :: i, j, ini
!    character(len=max_fits_char), dimension(tfields_asm) :: ttypes, tforms, tunits
!
!    call ftgiou(funit, status)
!    if (status .ne. 0) then
!      print *, 'DEBUG23: funit=', funit
!      funit = 177
!    !  errmsg = 'Failed to get a safe file unit to output file: ' // trim(fname)
!    !  call err_exit_if_status(status, errmsg) ! in Module err_exit
!    !  stop  ! redundant
!    end if
!
!    !--------- Test
!    call ftopen(399, '/home/parallels/try0.fits', 1, blocksize, status)
!    call FTPKYS(399, 'title', 'ASM extracted data', '', status)
!    call FTPKYS(399, 'filename', '/home/parallels/try0.fits', status)
!    call ftclos(399, status)
!    !---------
!
!    !call ftopen(funit, fname, 1, blocksize, status)
!    call ftinit(funit, fname, blocksize, status)
!    if (status .ne. 0) then
!      print *, 'DEBUG24: funit=', funit
!    end if
!    !  errmsg = 'Failed to open the output file: ' // trim(fname)
!    !  call err_exit_if_status(status, errmsg) ! in Module err_exit
!    !  stop  ! redundant
!    !end if
!
!    call FTPKYS(funit, 'title', 'ASM extracted data', '', status)
!    !call FTPKY[JKLS](funit, 'date', '????', '[day] Creation date of this file', status)
!    call FTPKYS(funit, 'telescop', 'Ginga', '', status)
!    call FTPKYS(funit, 'instrume', 'ASM', '', status)
!    call FTPKYS(funit, 'filename', trim(fname), 'Original filename', status)
!
!print *, 'DEBUG25: '
!    call get_ttypes(ttypes)
!    call get_tforms(tforms)
!    call get_tunits(tunits)
!
!print *, 'DEBUG26: size(fitshead)=', size(fitshead)
!
!    do isabu=1, size(fitshead)
!      call FTIBIN(funit, nrows(isabu), tfields_asm, ttypes, tforms, tunits, get_extname(isabu), varidat, status) ! tfields_asm defined in asm_fits_common
!write (*, '("DEBUG27: nrows(",i12,")=",i15)') isabu,nrows(isabu)
!
!      !call FTMAHD(funit, isabu+1, hdutype, status)           ! Move to the next extention
!      call FTPKYS(funit, 'telescop', 'Ginga', '', status)
!      call FTPKYS(funit, 'instrume', 'ASM', '', status)
!
!      !! Write elements into an ASCII or binary table column. The ‘felem’ parameter applies only to vector columns in binary tables and is ignored when writing to ASCII tables.
!      ini = 1  ! column number
!      arye(:) = 0
!      do i=1, nrows(isabu)
!        arye(i) = ardata(isabu, i)%tstart
!      end do
!      call FTPCLE(funit, 1, ini, 1, 1, nrows(isabu), arye, status)
!
!print *, 'DEBUG28: '
!
!      do j=1, 3  ! 3-dimensions
!        arye(:) = 0
!        do i=1, nrows(isabu)
!          arye(i) = ardata(isabu, i)%eulers(j)
!        end do
!        call FTPCLE(funit, 1, ini+j, 1, 1, nrows(isabu), arye, status)
!
!        arye(:) = 0
!        do i=1, nrows(isabu)
!          arye(i) = ardata(isabu, i)%angles_intn(j)
!        end do
!        call FTPCLE(funit, 1, ini+3+j, 1, 1, nrows(isabu), arye, status)
!      end do
!
!print *, 'DEBUG38: '
!      !!!!! tfields must increase by 3 to take this into account
!      !! integer(kind=1), dimension(3) :: acss;      ! acss = 3 bytes
!      
!      do j=1, num_instr*nchan_pha  ! ==96
!        aryi(:) = 0
!        do i=1, nrows(isabu)
!          aryi(i) = ardata(isabu, i)%arasm(j)
!        end do
!        call FTPCLE(funit, 1, ini+3*2+j, 1, 1, nrows(isabu), aryi, status)
!      end do
!print *, 'DEBUG48: '
!    end do
!
!    call ftclos(funit, status)
!    call ftfiou(funit, status)
!  end subroutine out_asm_fits_old
!end module asm_fitsout
!

PROGRAM   ASMDUMP

!!! F90 program for dumping ASM data for QDP
!
! USAGE: asmdump TELEMETRY_FITS FRF_FITS > STDOUT
!
! EXAMPLE:
!   ./asmdump ginga_sirius_R199006190617.fits FR880428.S0220.fits
! 
! Nomenclature:
!   * W16 means "Word-16", where a word is equivalent to a byte and it starts from W0.
!   * Hence W16 is the 17-th word (aka byte) in a telemetry frame.
!   * Frame is similar; F0, F1, ... (see, for a real example, Table 5.1.14, where F0 exists.)
!   * SABU-Frame starts from 0 or 1??  maybe 0?
! 
! DESCRIPTION (for the variable telems(:, :)):
! 
! Format overview: Sec.5.5 pp.198
! 
! * Word starts from 0: W0, W1, ...
! * Frame starts from 0: F0, F1, ... (because of Table 5.1.14, where F0 exists.)
!
! ASM Data: ((W16*n+4)..(W16*n+15)), i.e., 5th to 16th, 21th to 32nd, etc.
!         : cf. Table 5.1.1, pp.200 (for Basics)
!         :   AND, 48..51, 112..115 (when n starts from 0).
! 
! Common Part:
!   * sync code (W0-2): Always &xFAF320
!   * FI (W3): Frame-info(SABU-frame No., Frame No.)
!   * CMD (W32): CMD/AGC (whatever it means...)
!   * ACS data (W33-35): Attitude, STT, IRU, NSAS, etc (depending on the frame)
!   * ASM data (W48-51)
!   * AHK (W64): House-keeping (HV, temperature, ...)
!   * status (W65): (Slew369 Mode: F32n+10 W65 B3)
!   * DP (W66): (ASM Mode: F8n+4 W66 B3; (ON/OFF <=> 1/0)) (B4: PHA/Time <-> 0/1)
!   * PI MON (W67) / principal-instrument monitor??
!   * ASM data (W112-115)
! 
! ASM in Common Part: (W48-51, 112-115, and PI-MON (W67 in F8n+1,5))
!         : mentioned at Sec.5.1 Item-6b (pp.197)
!         : cf. Table 5.1.14, pp.216 (for ASM-Y1/2 CAL-PH/FW[1-3]-PC )
!         : i.e., F16: W48( Y1 CAL-PH 0ch),  W49(Y1 FW1-PC),  W50(Y1 FW2-PC),  W51(Y1 FW3-PC)
!         :          : W112(Y2 CAL-PH 0ch), W113(Y2 FW1-PC), W113(Y2 FW2-PC), W114(Y2 FW3-PC)
!         : i.e., F17: W48( Y1 CAL-PH 1ch),  W49(Y1 FW1-PC),  W50(Y1 FW2-PC),  W51(Y1 FW3-PC)
!         :          : W112(Y2 CAL-PH 1ch), W113(Y2 FW1-PC), W113(Y2 FW2-PC), W114(Y2 FW3-PC)
!         :  ===QUESTION===: FW1/2/3 has channels, too??
!         : cf. Table 5.1.17, pp.220 (for ASM-CAL-PH, ASM-FW-PC)
!         : cf. Table 5.1.17, pp.221 (for ASM-Y1/2 at W67(PI-MON) in F8n+1(Y1), F8n+5(Y2)) 
! 
! DP Block Command (PICM): Sec.4.2 Item-4 Table 4.2.4 DV-01/Bit4
!         : cf. Table 4.2.5, pp.191 for W66 for details of OS (PI block commands) (I don't understand this!)
! 
! Mode of operation: 
!         : Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
!         : Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
!         :      NOTE: ON/OFF <=> 1/0, TIME/PHA <=> 1/0
!         :   Also, F56 W66(=DP) (="DP OS1")  B4: TIME/PHA <=> 1/0  (redundant?)
! 
! ASM-Mode Byte-bit: Table 5.5.7 (pp.235), describing how a byte consists of bits (the most significant bit is the largest digit.)
! 
! ASM PHA Mode Data: (0..15ch) F8n+4 W66 Bit3,4=="10"
!         : cf. Table 5.5.5, pp.233
! 
! ASM Time Mode Data: (0..15ch) F8n+4 W66 Bit3,4=="11"
!         : cf. Table 5.5.6, pp.234
!
! Frame: W3 (4th-word) means SABU_frame(2bits) and Frame(6bits) (0..255)
!         : cf. Table 5.1.3, pp.201
! 
! ASM status: F32n+15 (W65 (66th word), DP) Flags of ASM ON/OFF etc
!         : cf. Table 5.1.10, pp.207 (frame numbers) => F32n+15
!         : cf. Table 5.1.12, pp.213
! ASM DP: F32n+14 (W66 (67th word), DP) Flags of ASM ON/OFF etc
! ASM OS (what is OS?): F16n+15 (W66 (67th word), DP) Flags of ASM OS
!         : cf. Table 5.1.11, pp.207 (frame numbers) => F16n+15
!         : cf. Table 5.1.12, pp.214
!
! PI monitor (W67): Table 5.1.16, pp.218 (for ASM Y1/2)
! 
! ASM Mode: Sec.6.7, pp.246
!         : sends DV-1. and sets OS-4 (=5th bit), depending on PHA/Time modes
!         : cf, also Sec.8.3, pp.259
! 
!!! Telemetry header (16 bytes=words)
! 
!   Byte00=month
!   Byte01=day
!   Byte02=hour
!   Byte03=minute
!   Byte04=second
!   Byte05+06=millisecond
!     millisec = byte05 * 256 + byte06 (readfits_SF_WD.c による)
!   Byte07=Counter_A1
!   Byte08=Counter_A2
!   Byte09=Counter_B1
!   Byte10=Counter_B2
!   Byte11=real(1) or stored(2)
!   Byte12=bit-rate-low(0) or high(1)
!   Byte13, 14, 15: not used
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
! *** Algorithm ***
! 
! * Get number of frames (FITS header)
! * Read all Telemetry (telems): iSF, iFrame, ACS(3), Status, DP, asm(96)
! * Read all associated Header (tel_heads): Mon, Day, h, m, s, ms, iSF, bitrate, iFrame, iTimeCounter(3)
! * Make (telem_sf(SF)) = SF(indexStart, indexEnd) : telem_sf(2)==(65, 128) (second SF starts at index=65, ends at 128 in telems)
! * Make (flag_asm(SF))  = SF(T/F) : e.g., SF(1:3)=False and SF(4:)=True then ASM is switched on at 4th frame
! * Make (flag_slew(SF)) = SF(T/F) : e.g., SF(1:3)=False and SF(4:)=True then Slew is switched on at 4th frame
! * Make (flag_is_time(SF)) = SF(T/F) : e.g., True if it is Time-mode as opposed to PHA mode.
! * Make (flag_complete_sf(SF)) = SF(T/F) : e.g., True if Frame is complete within the SF.
!   
! Conditions  
!   
! * SM mode must be switched on only once.
! * Slew mode must be on while ASM mode. 
! 
! Routine
! 
! * Determine if Frame is complete
!   * If not, they are not used.
! * Determine in which range of indices ASM mode is ON/OFF
! * Determine in which range of indices Slew mode is ON/OFF
! * Determine whether PHA or TIME
!   * This must not change.
! 
! * Load FRF
!   * arfrf%sf(    nFrames, 1)
!   * arfrf%frame( nFrames, 1)
!   * arfrf%mjds(  nFrames, 4)
!   * arfrf%euler( nFrames, 4)  ! euler.dot_euler
!   * arfrf%height( nFrames, 4)
!   * arfrf%longitude( nFrames, 4)
!   * arfrf%latitude(  nFrames, 4)
!   * arfrf%earth_alpha(  nFrames, 4) ! Earth center
!   * arfrf%earth_delta(  nFrames, 4) ! Earth center
!   * arfrf%cor(  nFrames, 4) ! Earth center
!   * arfrf%magnet_alpha(  nFrames, 4)
!   * arfrf%magnet_delta(  nFrames, 4)
!   * arfrf%sun_alpha(  nFrames, 4)
!   * arfrf%sun_delta(  nFrames, 4)
!   * arfrf%sunps(  nFrames, 4) ! Sun presence
!   * arfrf%elvys(  nFrames, 4) ! ELEVATION OF YAXIS FROM THE EARTH EDGE
!   * arfrf%elags(  nFrames, 4) ! CONDITION OF THE EARTH OCCULTATION
!   *                           ! 0: NOT OCCULTED, 1: OCCULTED BY THE DARK EARTH
!   *                           ! 2: OCCULTED BY SUN SHONE EARTH
!   * arfrf%nsampl(nFrames) ! NUMBER OF THE ORBIT AND ATTITUDE DATA: NSAMPL=1 FOR BITRATE H,M ,  =4 FOR BITRATE L
! 
! * Match with FRF
!   * Make others like frf_altitude, frf_earths(alpha, delta)
! 
! * Allocate the output arrays with (64 * Valid_ASM_complete_frames)
! * Create the data with telemetry
!   * arout%sf(    nFrames)
!   * arout%frame( nFrames)
!   * arout%tstart(nFrames)
!   * arout%euler( 3, nFrames)  (from FRF, fill the SF with the same Euler)
!   * arout%atdata(6, nFrames)
!   * arout%asm(  96, nFrames)
! 
! 
! 
! 

  use iso_fortran_env, only : stderr=>ERROR_UNIT   

  use err_exit
  use asm_fits_common
  use asm_aux
  use asm_read_telemetry
  use asm_fitsout

  implicit none

!integer, parameter :: dp = kind(1.d0) ! defined in asm_fits_common
      integer :: maxaxes, nbytepercard, nbyteforheader
      parameter(maxaxes = 2, nbytepercard = 144, nbyteforheader = 16 )
      !CHARACTER PATHID*11                                               
      !INTEGER   FU/33/,RECN                                             
      !CHARACTER DEFFIL*26/'SBSG010.LACPLOT.DATA '/,NUMBER*3,PARFIL*36   
      !CHARACTER PATH*10/'8702010000'/,ATTFIL*36                         
      !INTEGER   BMUNIT/20/
      INTEGER   SFN !,M,KND/0/,SFI,TIME(7)
      INTEGER   SYNC(0:63),LOSTF,BITRAT,RELSTR ! BITRAT: 0(H), 1(M), 2(L)
      !INTEGER   MODE(0:7),MOD1,LACST(0:13)                              
      !INTEGER   OSS,FLAG(6),LACOS(0:30,0:7)                             
      !INTEGER   GBDST(0:5),RBFLAG,GRP,LACWD(0:7,0:7,0:7),PIMN(0:3,0:7)  
      !INTEGER   SCPC(0:31,0:1),SOL2(0:7),ASMWD(0:1,0:2,0:63)/384*0/     
      !INTEGER   LACA(0:95,0:7,0:7),LACB(0:95,0:7,0:7)                   
      INTEGER   SUNPS(4),EFLAGS(4),NSAMPL                               
      !INTEGER   PLUNIT/16/,NBINMX,NGRAPH,SF1,SF2,SF0                    
      !INTEGER   CNTL
      INTEGER   COND,TIMES(7) !,TCNL,TCHK                            
      !INTEGER   J,LDIS/32/,OSF                                          
      !REAL      HV(0:7),TEMP(0:7)                                       
      !REAL*8    RBUFF(17,4),ELVYS(4)                                    
      !REAL*8    MJD,MJDS(4)                                             
      !CHARACTER GKBUFF(20)*72,DUMMY*72,NAMTAG(2)*10                     
      !REAL      ALPTAG(2),DELTAG(2)                                     

      !INTEGER   BITRAT,SUNPS(4),EFLAGS(4),NSAMPL                        
      DOUBLE PRECISION    MJD,MJDS(4)                                             
      DOUBLE PRECISION    RBUFFS(17,4),ELVYS(4)                                   

      INTEGER :: i
      character(len=1024) :: errmsg, arg, fname = '', frffil = '', outfil = '', s!, s1, s2
      !character(len=80) :: comment, keyword, snull, msg
      character(len=30) :: errtext

      integer :: funit, status=-999, blocksize !, hdutype, nframes, naxis1
      !integer :: datacode, repeat, num_axis, width
      integer, dimension(maxaxes) :: naxes
      !integer :: ncols = -99, nrows = -99

      !integer(kind=1), dimension(:), allocatable :: ardata
      !integer(kind=1), dimension(:, :), allocatable :: headers, telems ! (word(=byte), row)
      !integer(kind=4) :: nbytes

      !type(asm_header), dimension(:), allocatable :: fitshead  ! (iSabuFrame), None for primary header
      !type(asm_telem_row),  dimension(:, :), allocatable :: arout  ! (iSabuFrame, Row)
      !integer,          dimension(:), allocatable :: arnrows   ! Number-of-significant-rows(iSabuFrame)

      !integer :: isabu, saved_sf4, i_fr64, itotrow, numsfs
      !integer(kind=1) :: sf4, fr_6bits

!---------- TEST      
      integer unit,bitpix,naxis !status,blocksize,naxes(2)
      !integer group,fpixel,nelements,array(300,200) ! i,j,
      character(len=80) :: filename
      logical simple,extend
      integer :: nhdu
      character(len=80), dimension(2) :: ttypes = (/ 'TIME', 'Char' /), tunits, &
         tforms = (/ '1D ', '20A' /)  ! 20A20?
         !tforms = (/ '1D   ', '20A20' /)  ! 20A20?
         !tforms = (/ '1D   ', '1D   ' /)
    real(kind=dp8), dimension(2) :: dvalues = (/ 12345.6, 7.89 /) !, d2values = (/ 0.023, 0.0045 /)
    character(len=20), dimension(2) :: svalues = (/ 'saisho', 'tsugi1' /)
      !DOUBLE PRECISION    dvalues(2)
!---------- TEST       up to here

  i = -1  ! i=0 is for $0
  do
    i = i+1
    call get_command_argument(i, arg)
    if (i == 0) cycle
    if (len_trim(arg) == 0) exit
    if (i == 1) then
      fname = arg  ! Telemetry
      write (*,*) 'Telemetry: '//trim(arg)
    else if (i == 2) then
      frffil = arg ! FRF
      write (*,*) 'FRF:       '//trim(arg)
    else if (i == 3) then
      outfil = arg ! FRF
      write (*,*) 'Outfile:   '//trim(arg)
    end if
  end do

  WRITE (*,*) 'fname=' // TRIM(fname)  ! maybe syntax error strictly?


  ! Get (fill) headers, telems: raw byte Array(word(byte), row)
!  call read_telemetry(fname, headers, telems) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! ******** read FRF ************

      WRITE (*,*) 'FRF-n=' // TRIM(frffil)
      
      call ftgiou(funit, status)
      if (status .ne. 0) then
        errmsg = 'Failed to get a safe file unit for FRF'
        call err_exit_if_status(status, errmsg) ! in Module err_exit
        stop  ! redundant
      end if

      call OPNFRF(funit,frffil,cond)
      call SFGET(cond)  ! Next SABU-Frame  ! cond is set 0(Normal) or 9(EOF, data are not set)

      ! do while(cond .eq. 0)  ! cond is set at the end of the last loop.
      do i=1, 3  !!!!!!!!!!!!!!! 1--3 for testing
        call GETOAT(MJD, BITRAT, MJDS,RBUFFS,SUNPS,ELVYS,EFLAGS,NSAMPL)
          !!! From frfread.F
          !  MJDS  (OUT): MJD OF THE ORBIT AND ATTITUDE                           
          !  RBUFFS(OUT): RBUFF(J,*)  *=1,NSAMPL                                  
          !               J= 1- 3: EURLER ANGLES  (Z-Y-Z)                         
          !               J= 4- 6: DOT EURLER ANGLES                              
          !               J= 7- 9: HEIGHT, LONGITUDE, LATTITUDE                   
          !               J=10   : DISTANCE FROM THE EARTH CENTER                 
          !               J=11-12  ALPHA,DELTA OF THE EARTH CENTER (1950 EQUINOX) 
          !               J=13   : CUT OFF RIGIDITY                               
          !               J=14-15: ALPHA,DELTA OF THE MAGNETIC FIELD              
          !               J=16-17: ALPHA,DELTA OF THE SUN                         
          !  SUNPS (OUT): PRESENCE OF SUNSHINE, 1/0=YES/NO                        
          !  ELVYS (OUT): ELEVATION OF YAXIS FROM THE EARTH EDGE                  
          !  ELAGS (OUT): CONDITION OF THE EARTH OCCULTATION                      
          !                0: NOT OCCULTED, 1: OCCULTED BY THE DARK EARTH         
          !                2: OCCULTED BY SUN SHONE EARTH                         
          !  NSAMPL (OUT): NUMBER OF THE ORBIT AND ATTITUDE DATA                  
          !                NSAMPL=1 FOR BITRATE H,M ,  =4 FOR BITRATE L           
        call SFCHCK(sync, lostf, sfn, bitrat, relstr, times)
        WRITE (s,'("19",i2,"-",i0.2,"-",i2," ",i0.2,":",i2,":",i2,".",i0.3)') times(1:7)
        WRITE (*,'("--- Iteration: ", i1, " SFN=", i4, " MJD= ",a)') i, sfn, trim(s)
        WRITE (*,'("MJDs=(", es17.9, ",", es17.9, ",", es17.9, ",", es17.9, ")")') mjds
        WRITE (*,'("Euler1/", i1, "(1)=", f10.5)') nsampl, rbuffs(1,nsampl)
        WRITE (*,'("Euler1/", i1, "(2)=", f10.5)') nsampl, rbuffs(2,nsampl)
        WRITE (*,'("Euler1/", i1, "(3)=", f10.5)') nsampl, rbuffs(3,nsampl)
        call SFGET(cond)  ! Next SABU-Frame  ! cond is set 0(Normal) or 9(EOF, data are not set)
      end do

      call CLSFRF(cond)
      call ftfiou(funit, status)

      !!! Comment: For some reason, error is raised in deallocate::  free(): invalid size
      !print *, 'DEBUG(21): before-dealloc '
      !deallocate(headers, STAT=status)
      !deallocate(telems,  STAT=status)
      !deallocate(ardata, STAT=status)
      !print *, 'DEBUG(24): after-dealloc '

! ******** write test fits **********

!program writeimage
      status=0
! Name of the FITS file to be created:
filename='!'//'ATESTFILE.FITS'
! Get an unused Logical Unit Number to use to create the FITS file
call ftgiou(unit,status)
! create the new empty FITS file blocksize=1
call ftinit(unit,filename,blocksize,status)
call FTGERR(status, errtext)
call FTGHDN(unit, nhdu)  ! CHDU: Current HDU
print *,'test-open1-status=',status,' / HDU=',nhdu,' / ',trim(errtext)

!call FTOPEN(unit,filename,1, blocksize,status) ! read-wrte
!call FTGERR(status, errtext)
!print *,'test-open2-status=',status,' / ',trim(errtext)

! initialize parameters about the FITS image (300 x 200 16-bit integers)
simple=.true.
      bitpix=16  ! signed 2-byte, -32: real, -64: double
      naxis=2
      naxes(1)=300
      naxes(2)=200
extend=.true.
! write the required header keywords
call ftphpr(unit,simple,bitpix,0,naxes,0,1,extend,status)
!call ftphpr(unit,simple,bitpix,naxis,naxes,0,1,extend,status)
!! initialize the values in the image with a linear ramp function
!do j=1,naxes(2)
!          do i=1,naxes(1)
!              array(i,j)=i+j
!            end do
!          end do
!      
!! write the array to the FITS file group=1
!fpixel=1  ! First pixel
!nelements=naxes(1)*naxes(2)
!call ftpprj(unit,group,fpixel,nelements,array,status)

! write another optional keyword to the header
call ftpkyj(unit,'EXPOSURE',1500,'Total Exposure Time',status)

! Extension

!ttypes = (/ 'TIME', 'String' /)
!tforms = (/ '1D', '20A' /)  ! Double
!tunits = (/ 'sec', '' /)
tunits(1) = 'sec'
tunits(2) = ''
!dvalues = (/ 12345.6, 7.89 /)
!svalues = (/ 'saisho', 'tsugi' /)

print *, 'test-svalues =', svalues

!call FTIBIN(unit,nrows,tfields,ttype,tform,tunit,extname,varidat > status) ! nrows should be 0
call FTIBIN(unit,0,2,ttypes,tforms,tunits,'TestBinExt',.true., status) ! Creates an extension with basic header and moves to it.
call FTGHDN(unit, nhdu)
call FTGERR(status, errtext)
print *,'test-new-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)
call ftpkys(unit,'MY_HEAD','Arbitrary','My comment 01',status)

! Write Table (double precision)
!FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
call FTPCLD(unit,1,1,1,2,dvalues, status)  ! colnum = 1
call FTGHDN(unit, nhdu)
call FTGERR(status, errtext)
print *,'test-dval-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)

call ftpkys(unit,'TDIM2','(20,1)','for Character in Binary table',status)
call FTPCLS(unit,2,1,1,2,svalues, status)  ! colnum = 2
!call FTPCLD(unit,2,1,1,2,d2values, status)
call FTGHDN(unit, nhdu)
call FTGERR(status, errtext)
print *,'test-char-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)  ! If wrong, 309  / not an ASCII (A) column

!! call FTMAHD(unit, 2, hdutype, status)           ! Move to the Absolute extention (1st extension if 2)
!call FTMRHD(unit, 1, hdutype,status) ! nmove==1 ! cMove to a new (existing) HDU forward or backwards relative to the CHDU
!call FTGHDN(unit, nhdu)
!call FTGERR(status, errtext)
!print *,'test-move-status=',status,' / HDU=',nhdu,' / ',trim(errtext)

!call ftphpr(unit,simple,bitpix,0,naxes,0,1,extend,status)
!call FTGERR(status, errtext)
!print *,'test-HD-phpr-status=',status,' / ',trim(errtext)

! close the file and free the unit number
call ftclos(unit, status)
call FTGERR(status, errtext)
print *,'test-close-status=',status,' / ',trim(errtext)
call ftfiou(unit, status)
!end

      ! ******** write data **********

  !call write_asm_fits(fname, fitshead, tables, status)

!      numsfs = 1  ! Number of sabu-frames
!      allocate(arnrows( numsfs), STAT=status)
!      allocate(fitshead(numsfs), STAT=status)
!      allocate(arout(   numsfs, nframes_per_sf), STAT=status)
!
!      isabu = 0
!      saved_sf4 = 0
!      i_fr64 = 0
!      itotrow = 0
!      print *, 'DEBUG842: nrows=', nrows
!      do while (itotrow <= nrows)
!        itotrow = itotrow + 1
!        i_fr64 = i_fr64 + 1
!
!        call get_sf4_fr64(itotrow, headers, sf4, cur_fr64)
!
!        if (saved_sf4 .ne. sf4) then  ! Next sabu-frame
!          arnrows(isabu) = i_fr64
!          isabu = isabu + 1
!          i_fr64 = 1
!          saved_sf4 = sf4
!          arout(isabu, itotrow)%cur_fr64 = cur_fr64
!        end if
!
!        ! itotrow: i-th row frame in FITS
!        ! i_fr64: i-th frame in the current SF(sabu-frame)
!        ! cur_fr64: i-th frame in the current SF(sabu-frame) written in the Telemetry
!        ! isabu:  i-th SF in FITS
!        ! saved_sf4: current i-th frame in (0..3) as in FI(=W3)
!        ! sf4: Temporary var (of sabu-frame)
!
!        !! function get_frame_word_bit(irow, headers, iframe, iword, ibit)
!        fitshead(isabu)%mode_dp  = get_frame_word_bit(itotrow, headers, 56, 66, 3)
!        fitshead(isabu)%existdat = .true.
!        if (fitshead(isabu)%mode_dp < 0) then ! No frame is found.
!          fitshead(isabu)%existdat = .false.
!          fitshead(isabu)%mode_dp  = 0
!          fitshead(isabu)%mode_asm = 0
!          exit
!          !!!!!!!!!!!! Output error message!
!        else
!          fitshead(isabu)%mode_asm = get_frame_word_bit(itotrow, headers, 56, 66, 4)
!        end if
!
!        arout(isabu, i_fr64)%i_frame = cur_fr64
!        call fill_asm_one(isabu, itotrow, i_fr64, telems, arout) ! Main ASM data (=arout)
!      end do
!
!      print *, 'DEBUG: outfil=', trim(outfil)
!      print *, 'DEBUG991:cur_fr64=', cur_fr64
!      print *, 'DEBUG992:size=', size(arnrows), ' arnrows=', arnrows
!      !call out_asm_fits(outfil, fitshead, arout, arnrows)

END PROGRAM ASMDUMP


