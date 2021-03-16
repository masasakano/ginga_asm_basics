! Common routine for ASM-related FITS files
!
module asm_fits_common
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  use fort_util
  use err_exit
  implicit none 

  integer, parameter, private :: dp  = kind(1.d0)
  integer, parameter, public ::  dp8 = 8 ! FITS file REAL8
  integer, parameter, public ::  ip4 = 4 ! FITS file INTEGER4
  integer, parameter :: n_all_fields = 10  ! Number of fields to read and output
  integer, parameter, public :: max_fits_char = 68
  integer, parameter, public :: MAX_LEN_FKEY  = 8  ! FITS key maximum length of characters
  integer, parameter, public :: nchar_extname = 8
  integer, parameter, public :: NFRAMES_PER_SF = 64  ! Number of Frames per SABU-Frame
  integer, parameter, public :: NUM_INSTR = 6, NCHANS_PHA = 16, NCHANS_TIME = 8  ! Number of channels in each mode 
  integer, parameter, public :: NBYTESPERCARD = 144
  integer, parameter, public :: NWORDS_MAIN = NUM_INSTR*NCHANS_PHA  ! Number of words (=bytes) of Main (mode-dependent) section in a frame: 96
  character(len=*), parameter, public :: tunit_main = 'count'
  integer, parameter, public :: ASM_STATUS_FN = 15; ! (First) Frame number; F15W65B1 ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
  integer, parameter, public :: DIM_ACS_C = 3, DIM_ASM_C = 4;  ! Number of words for ACS and (each of the two) ASM in the common area in each telemetry frame
  integer, parameter :: LEN_READABLE_KEY = 32  ! Char-length of maximum human-readable key name
  integer, parameter :: LEN_T_INVALID_FMT_KEY = 32
  integer, parameter :: LEN_PROC_STATS = 256
  integer, parameter :: LEN_T_ARGV = 1024
  real(dp8),    parameter :: UNDEF_REAL = -1024.0d0
  integer(ip4), parameter :: UNDEF_INT  = -999
  character(len=*), parameter :: OUTFTCOMMENT1 = 'Created by combining a Ginga telemetry file&
     & and corresponding FRF for the LAC.  However, the LAC FRFs usually lack the data in which&
     & the ASM-Mode is on.  Therefore, no meaningful Euler angles appear in the Table&
     & and those in the header are only the best guesses taken from a frame (if&
     & there was any) shortly before and after the period in which the ASM-Mode was on.'
  character(len=*), parameter :: OUTFTCOMMENT2 = 'See GETOAT() in the Ginga FRFREAD reference manual&
     & for the definitions of some of the header keyword parameters.'

  character(len=max_fits_char), dimension(n_all_fields) :: &
     tmtypes, tmcomms, tmforms, tmunits

  !character(len=max_fits_char), dimension(:, :), allocatable :: tmcolss  ! (i-th-column(1:n_all_fields), i-th-row)
  !integer(kind=1),              dimension(:, :), allocatable :: tmcolsi
  !integer,                      dimension(:, :), allocatable :: tmcolsj
  !real(kind=dp),                dimension(:, :), allocatable :: tmcolsd
  !character(len=max_fits_char), dimension(:),    allocatable :: outcolss ! (i-th-row)
  !integer(kind=1),              dimension(:),    allocatable :: outcolsi
  !integer,                      dimension(:),    allocatable :: outcolsj
  !real(kind=dp),                dimension(:),    allocatable :: outcolsd

  integer, private :: i

  DATA tmtypes(1:2) / 'SF_NO2B', 'FRAME_NO' /

  interface get_onoff_enadis
    module procedure get_onoff_enadis_from_key, get_onoff_enadis_from_tf
  end interface get_onoff_enadis

  interface get_index
    module procedure get_index_char, get_index_argv, get_index_colhead, get_index_form_unit
  end interface get_index

  interface get_element
    module procedure get_element_char, get_element_argv, get_element_colhead, get_element_form_unit
  end interface get_element

  interface get_val_from_key
    module procedure get_val_from_key_argv
  end interface get_val_from_key

  interface dump_type
    module procedure dump_asm_telem_row, dump_asm_frfrow, dump_asm_sfrow, dump_form_unit
    module procedure dump_asm_colhead, dump_fits_header
  end interface dump_type
 
  ! Number of the total columns (TTYPEs) of a type(t_asm_colhead)
  interface get_ncols_colheads
    module procedure get_ncols_colheads_char,       get_ncols_colheads_colheads
    module procedure get_ncols_colheads_frameunits, get_ncols_colheads_none
  end interface get_ncols_colheads

  ! For command-line arguments
  type t_argv
    character(len=LEN_READABLE_KEY) :: key;
    character(len=LEN_T_ARGV) :: val = '';
  end type t_argv

  ! Number of the total frames of a Colheads

  ! Ginga Telemetry file word structure
  !
  ! It starts from zero (0): W0-W127
  type t_telem_word_from0 ! "from0" means the word number starts from 0, as opposed to 1.
    integer :: fi     =  3; !  W3 (Frame-Info: SF(2-bits)+FrameNo(6-bits) == Telemetry-Header Counter_A2(Byte08))
    integer :: status = 65; ! W65
    integer :: dp     = 66; ! W66
    integer :: pi_mon = 67; ! W67
    integer :: acss   = 33; ! W33 (ACS: the fist word)
    integer :: acs1   = 33; ! W33 (ACS: W33-W35)
    integer :: acs2   = 34; ! W33 (ACS: W33-W35)
    integer :: acs3   = 35; ! W33 (ACS: W33-W35)
    integer :: asm1_commons =  48; ! W48  (ASM: W48-W51: the fist word in the common area)
    integer :: asm2_commons = 112; ! W112 (ASM: W112-W115: the fist word in the common area)
  end type t_telem_word_from0
  type(t_telem_word_from0), parameter :: TELEM_WORD_FROM0 = t_telem_word_from0()

  ! type for (maybe first) frame and word and bit
  type t_frame_word_bit
    integer :: frame, word, bit;
  end type t_frame_word_bit

  ! Location infor for Frame, Word, and Bit
  !
  ! If "F8n+4 W66 B3", f_multi=8, f_offset=4, word=66, bit=3
  ! If "F7 W0",        f_multi=0, f_offset=7, word=0,  bit=-999
  ! If "W65",          f_multi=1, f_offset=1, word=65, bit=-999
  !
  ! NOTE: frame starts from 1, whereas word(=byte) and bit start from 0.
  !     : Frame[1:64], Word[0:127], Bit[0:7] (The most significant bit is 0.)
  !     : In short, f_multi=[0:63], f_offset=[1:64], word=[0:127], Bit=[0:7] or negative(-999)
  ! "note" is for any comment as you like.
  type t_loc_fwb  ! Type-LOCation-Frame-Word-Bit
    integer :: f_multi=0, f_offset, word, bit=UNDEF_INT;  ! f_multi=0 means only 1 location. A negative bit is no bit info.
    character(len=64)   :: name = '';
    character(len=1024) :: note = '';
  end type t_loc_fwb

  ! Ginga Telemetry info location
  !
  ! It starts from zero (0): B0-B7
  type t_telem_loc
    type(t_loc_fwb) :: MODE_OBS  = t_loc_fwb(f_multi=8,  f_offset=4,  word=TELEM_WORD_FROM0%dp, note='(DP)'); ! W66
    type(t_loc_fwb) :: STAT_OBS  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, note='(Status)'); ! W65
    type(t_loc_fwb) :: DPID_OBS  = t_loc_fwb(f_multi=15, f_offset=16, word=TELEM_WORD_FROM0%dp, note='(DP)'); ! W66
    type(t_loc_fwb) :: MODE_ASM  = t_loc_fwb(f_multi=8,  f_offset=4,  word=TELEM_WORD_FROM0%dp,     bit=3, &
      note='(DP) (ON/OFF <=> 1/0)'); ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
    type(t_loc_fwb) :: MODE_SLEW = t_loc_fwb(f_multi=32, f_offset=10, word=TELEM_WORD_FROM0%status, bit=3, &
      note='(Status)'); ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
    type(t_loc_fwb) :: MODE_PHA  = t_loc_fwb(f_multi=8,  f_offset=4,  word=TELEM_WORD_FROM0%dp,     bit=4, &
      note='(DP) (TIME/PHA <=> 1/0)'); ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    type(t_loc_fwb) :: MODE_PHA_W56 = t_loc_fwb(f_multi=0, f_offset=56, word=TELEM_WORD_FROM0%dp,     bit=4, &
      note='(DP) (TIME/PHA <=> 1/0), duplicate'); ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    !integer(ip4) :: mode_real_stored = UNDEF_INT;
    type(t_loc_fwb) :: STAT_ASM  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=1, &
      note='(Status)'); ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
    type(t_loc_fwb) :: STAT_ASA  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=2, &
      note='(Status)'); ! ON/OFF for ASM-A    F15W65B2
    type(t_loc_fwb) :: STAT_AMC  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=3, &
      note='(Status)'); ! ON/OFF for ASM-AMC  F15W65B3
    type(t_loc_fwb) :: STAT_HV1  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=4, &
      note='(Status)'); ! ENA/DIS for ASM-HV1 F15W65B4
    type(t_loc_fwb) :: STAT_HV2  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=5, &
      note='(Status)'); ! ENA/DIS for ASM-HV2 F15W65B5
    type(t_loc_fwb) :: STAT_RBM  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=6, &
      note='(Status)'); ! ENA/DIS for ASM-RBM F15W65B6
    type(t_loc_fwb) :: STAT_BDR  = t_loc_fwb(f_multi=32, f_offset=15, word=TELEM_WORD_FROM0%status, bit=7, &
      note='(Status)'); ! ENA/DIS for ASM-BDR F15W65B7
    type(t_loc_fwb) :: BITRATE   = t_loc_fwb(f_multi=0,  f_offset=16, word=TELEM_WORD_FROM0%dp,     bit=-1, &
      note='(DP)'); ! [/s] Telemetry bit rate (F16W66) ! Taken from Telemetry as opposed to FRF
  end type t_telem_loc
  type(t_telem_loc), parameter :: TELEM_LOC = t_telem_loc()
  type(t_loc_fwb), dimension(7), parameter :: AR_TELEM_LOC = [ &
       TELEM_LOC%MODE_OBS &
     , TELEM_LOC%STAT_OBS &
     , TELEM_LOC%DPID_OBS &
     , TELEM_LOC%MODE_ASM &
     , TELEM_LOC%MODE_SLEW&
     , TELEM_LOC%MODE_PHA &
     , TELEM_LOC%MODE_PHA_W56 &
     ]

  !! Ginga Telemetry BITs
  !!
  !! It starts from zero (0): B0-B7
  !type t_telem_stat_bit_from0
  !  integer :: asm_dp =  3; ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
  !  integer :: pha_time = 4; ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
  !  integer :: slew   =  3; ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
  !  integer :: asm_stat = 1; ! ON/OFF for ASM  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
  !  integer :: asa      = 2; ! ON/OFF for ASM-A    F15W65B2
  !  integer :: amc      = 3; ! ON/OFF for ASM-AMC  F15W65B3
  !  integer :: hv1      = 4; ! ENA/DIS for ASM-HV1 F15W65B4
  !  integer :: hv2      = 5; ! ENA/DIS for ASM-HV2 F15W65B5
  !  integer :: rbm      = 6; ! ENA/DIS for ASM-RBM F15W65B6
  !  integer :: bdr      = 7; ! ENA/DIS for ASM-BDR F15W65B7
  !end type t_telem_stat_bit_from0
  !type(t_telem_stat_bit_from0), parameter :: TELEM_BIT_FROM0 = t_telem_stat_bit_from0()

  ! ASM Table as read from a Telemetry file; row-based, ie., each row constitutes 1 variable
  !
  ! NOTE: A byte must have been converted into Integer*4 with a proper filter
  type asm_telem_row
    !--- Header  (memo is taken from COMMENT in the header of the telemetry FITS 1st extension)
    integer(kind=ip4) :: month;     ! Byte00 
    integer(kind=ip4) :: day;       ! Byte01 
    integer(kind=ip4) :: hour;      ! Byte02 
    integer(kind=ip4) :: minute;    ! Byte03
    integer(kind=ip4) :: second;    ! Byte04
    integer(kind=ip4) :: millisec1; ! Byte05; Upper (more significant (larger) digits)
    integer(kind=ip4) :: millisec2; ! Byte06; Lower
    integer(kind=ip4) :: Counter_A1;! Byte07;
    integer(kind=ip4) :: Counter_A2;! Byte08; ! FRAME counter between [0:3](SF)+[0:63](Frame) as in FI in Telemetry
    integer(kind=ip4) :: Counter_B1;! Byte09;
    integer(kind=ip4) :: Counter_B2;! Byte10;
       ! A1, B1, B2 are the top, med, lower 8-bis of the 24-bit TI counter.
       !   WB: TI counter value is (I think): byte07*65536 + byte09*256 + byte10
    integer(kind=ip4) :: real_or_stored; ! Byte11; real(1) or stored(2)
    integer(kind=ip4) :: bitrate;   ! Byte12; bit-rate-low(0) or high(1);
       ! NOTE: Bytes 13-15 are unused.

    !--- Telemetry
    integer(kind=ip4) :: fi_w3;     ! W3(=FI) (Interim-Report Table 5.1.3, pp.201)
       ! B0: SF 2 (SF = Sabu-frame; 4-frame cyclic)
       ! B1: SF 1 (SF = Sabu-frame; 4-frame cyclic)
       ! B2: F 32 (F = Frame; 64-frames per 1 SF)
       ! B3: F 16
       ! B4: F 8
       ! B5: F 4
       ! B6: F 2
       ! B7: F 1
    integer(kind=ip4) :: STAT_OBS = UNDEF_INT; ! W65(=Status)
    character(len=8) :: STAT_OBS_B8 = ''; ! String Bit expression of W65(=Status)
       ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed))
       !   Ref1: Table 5.1.12, pp.209
    integer(kind=ip4) :: DPID_OBS = UNDEF_INT; ! W66(=DP)
    character(len=8) :: DPID_OBS_B8 = ''; ! String Bit expression of W66(=DP)
       ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
       !   Note: In short, F4 alone should be fine.  In practice, in some SFs,
       !         F4 may be missing.  In this code, such frames should be discarded.
       !   Ref1: Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
       !   Ref2: Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
       ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
       !   Note: The same is found at F56W66B4 (but it is ignored in this code).
       !   Ref2: Table 5.1.12, pp.214 “MODE” => B3(ASM ON/OFF), B4(ASM TIME/PHA)
    integer(kind=ip4) :: pi_mon = UNDEF_INT; ! W67(=PI_MON)
    integer(kind=ip4), dimension(4) :: asm_w48;  ! (W48-W51)   ignored
    integer(kind=ip4), dimension(4) :: asm_w112; ! (W112-W115) ignored
       !   Note: PI-MON (W67 in F8n+1,5) also contains a piece of ASM info, but is ignored in this code
       !   Refs: Interim-Report at Sec.5.1 Item-6b (pp.197), Table 5.1.14, pp.216 (for ASM-Y1/2 CAL-PH/FW[1-3]-PC)

    !--- Derived
    integer(kind=ip4) :: millisec_i4; ! (= byte05 * 256 + byte06) (cf. readfits_SF_WD.c);
    real(kind=dp8) :: second_real;  ! second(Byte04)+millisec(= byte05 * 256 + byte06) (cf. readfits_SF_WD.c);
    integer(kind=ip4) :: year;        ! Year in 4 digits (read from the telemetry FITS header)
    real(kind=dp8) :: mjd;          ! MJD ! call MJULIA(time, mjd) ! defined in mjd.f in ginga_tool
    integer(kind=ip4) :: TIcounter;   ! byte07*65536 + byte09*256 + byte10
    real(kind=dp8) :: tstart;
    integer(kind=ip4) :: w_fi;      ! FI (Frame-Info) (W3; 4th byte) in Telemetry
    integer(kind=ip4) :: sf_2bit;   ! 2-bit info of the SF in Telemetry
    integer(kind=ip4) :: fr_6bit;   ! Current frame number as recorded in Telemetry in 6 bits
    integer(kind=ip4) :: i_frame;   ! i-th frame in the current SF
    !real(kind=dp8), dimension(3) :: eulers;   ! Not in the telemetry frame?
    integer(kind=ip4), dimension(DIM_ACS_C) :: acss;        ! acss = 3 bytes (W33-35)
    integer(kind=ip4), dimension(DIM_ASM_C) :: asm1_commons; ! AMS in the common area (W48-51) = 4 bytes
    integer(kind=ip4), dimension(DIM_ASM_C) :: asm2_commons; ! AMS in the common area (W112-115) = 4 bytes
    integer(kind=ip4), dimension(NWORDS_MAIN) :: asmdats; ! Main ASM data; Y[1-2]FW[1-3](ch[0-15]) for PHA mode, Y[1-2]FW[1-3][LH](ch[0-7]) for Time mode
  end type asm_telem_row

  ! ASM FRF; row-based, ie., each row constitutes 1 variable, and SF (sabu-frame) based.
  !
  ! NOTE: A byte must have been converted into Integer*4 with a proper filter
  type asm_frfrow
    !------- with SFCHCK() -----------
    integer(kind=ip4), dimension(0:63) :: sync = UNDEF_INT;    ! (0:63), 0=OK, 1=NG
    integer(kind=ip4) :: lostf = UNDEF_INT;   ! Number of frames with SYNG=NG
    integer(kind=ip4) :: sfn = UNDEF_INT;     ! SF Number (by SIRIUS)
    integer(kind=ip4) :: bitrate = UNDEF_INT; ! 0: high, 1: medium, 2: low
      ! NOTE: bit-rate-low(0) or high(1) in Byte12 of Telemetry-Header-16-bits.
      ! NOTE: Name is "bitrate" (with trailing 'e') as opposed to 'BITRAT' in FRFREAD manual.
      ! NOTE: 128(L)/32(M)/4(H) sec/SF (cf. Table 4.1.2, pp.187 in Ginga specification)
      !     :   2(L)/0.5(M)/0.0625(H) sec/Frame
    integer(kind=ip4) :: relstr = UNDEF_INT;  ! 0: real-data, 1: stored data
      ! NOTE: The flag is different in Byte11 of Telemetry-Header-16-bits: (real(1) or stored(2))
    integer(kind=ip4), dimension(7) :: stime = UNDEF_INT; ! Start time of SF in (Y,M,D,h,m,s,ms) in INTEGER

    !------- with GETOAT() -----------
    ! NOTE: All but nsampl has an array of 1..4; it contains up to 4 data
    !   depending on nsampl (which is determined by `bitrate`).
    !   In short, only for Bitrate=Low, 1 SF contains 4 "OAT" data (presumably
    !   because Low-Bitrate spans so long (128s) that it is desirable to have
    !   more attitude data sampling. For any other Bitrates (High and Medium),
    !   nsampl=1 and the elements of n=2..4 are null (or undefined?).
    !
    !   Thus, ideally, the OAT data in the frames in a SF in the output FITS
    !   should reflect the 4-time sampling in the Low-Bitrate data, e.g.,
    !   the first quater has mjds(n=1) (n:[1-4]), the second, data(n=2), and so on.
    !   However, in the actual Ginga operations, Low-Bitrate data were used
    !   only once or so (for the ASM data, at least), majorly because the data
    !   quality was generally very poor.  Therefore, we agreed to ignore 
    !   the sampling frequency, in other words, only the n=1 data are used
    !   in the actual processing that uses this type of variable regardless
    !   of the bitrate and all the frames in a SF in the output FITS will have
    !   a common value with regard to any of the OAT.
    !
    real(dp8), dimension(4) :: mjds = UNDEF_REAL ! MJD OF THE ORBIT AND ATTITUDE
    real(dp8), dimension(17,4) :: rbuffs = UNDEF_REAL ! RBUFF(J,*)  *=1,NSAMPL ! for backup/debugging
    !---- From here, Contents of RBUFF([1-17],*)
    real(dp8), dimension(3, 4) :: eulers = UNDEF_REAL        ! [J=1-3] EURLER ANGLES  (Z-Y-Z) [radian]
    real(dp8), dimension(3, 4) :: d_eulers = UNDEF_REAL      ! [J=4-6] DOT EURLER ANGLES [radian]
    real(dp8), dimension(4) :: height = UNDEF_REAL     ! [J=7] HEIGHT [km]
    real(dp8), dimension(2, 4) :: lon_lat = UNDEF_REAL       ! [J=8-9] LONGITUDE, LATTITUDE [deg]
    real(dp8), dimension(4) :: dist_earth = UNDEF_REAL ! [J=10] DISTANCE FROM THE EARTH CENTER [km]
    real(dp8), dimension(2, 4) :: coords_earth = UNDEF_REAL  ! [J=11-12] ALPHA,DELTA OF THE EARTH CENTER (1950 EQUINOX) [deg]
    real(dp8), dimension(4) :: cor = UNDEF_REAL        ! [J=13] CUT OFF RIGIDITY [GeV/c]
    real(dp8), dimension(2, 4) :: coords_magnet = UNDEF_REAL ! [J=14-15] ALPHA,DELTA OF THE MAGNETIC FIELD [deg]
    real(dp8), dimension(2, 4) :: coords_sun = UNDEF_REAL    ! [J=16-17] ALPHA,DELTA OF THE SUN [deg]
    !---- Up To here, Contents of RBUFF 
    real(dp8), dimension(4) :: sunps = UNDEF_INT  ! PRESENCE OF SUNSHINE, 1/0=YES/NO
    real(dp8), dimension(4) :: elvys = UNDEF_REAL  ! ELEVATION OF YAXIS FROM THE EARTH EDGE [deg]
    real(dp8), dimension(4) :: eflags = UNDEF_INT ! CONDITION OF THE EARTH OCCULTATION
                   ! 0: NOT OCCULTED, 1: OCCULTED BY THE DARK EARTH
                   ! 2: OCCULTED BY SUN SHONE EARTH
    real(dp8) :: nsampl = UNDEF_INT ! NUMBER OF THE ORBIT AND ATTITUDE DATA
                       ! NSAMPL=1 FOR BITRATE H,M ,  =4 FOR BITRATE L
  end type asm_frfrow

  ! Invalid reason type
  type t_invalid_fmt
    character(len=LEN_T_INVALID_FMT_KEY)   :: key;
    character(len=1024) :: desc;
    character(len=1024) :: fmt;
    integer             :: nargs = 0;  ! Number of Integer arguments required with the "fmt"
  end type t_invalid_fmt
  type(t_invalid_fmt), dimension(5), parameter :: INVALID_FMT = [ &
       t_invalid_fmt(key='no_frf', desc='no matching with FRF found (not considered anymore)' &
                                , fmt='("no matching with FRF found (not considered anymore)")', nargs=0) &
     , t_invalid_fmt(key='tel64',  desc='less than 64 Telemetry nFrames' &  ! NFRAMES_PER_SF = 64
                                , fmt='("less than 64 Telemetry nFrames=",I2)', nargs=1) &
     , t_invalid_fmt(key='lostf',  desc='positive lostf(FRF)' &
                                , fmt='("positive lostf(FRF)=",I2)', nargs=1) &
     , t_invalid_fmt(key='asmswoff', desc='ASM is switched off' &
                                , fmt='("ASM is switched off")', nargs=0) &
     , t_invalid_fmt(key='asmmodeoff', desc='ASM-mode is off' &
                                , fmt='("ASM-mode is off")', nargs=0) &
     ]

  ! Invalid reason object to be used in asm_sfrow%reason_invalid
  type t_reason_invalid
    character(len=LEN_T_INVALID_FMT_KEY) :: key;
    character(len=max_fits_char) :: text = '';
  end type t_reason_invalid

  ! ASM SFs (Sabu-frames) row-based, ie., each row constitutes 1 variable, and SF (sabu-frame) based.
  !
  ! Used to match Telemetry and FRF.
  !
  ! Once itrow and ifrow have been determined, a number of SF info can be obtained
  ! from asm_frfrow, using ifrow, including bitrate and relstr; 
  type asm_sfrow
    logical  :: is_valid = .true.; ! if True, this SF is output.
    type(t_reason_invalid) :: reason_invalid = t_reason_invalid(key=''); ! Explanation why it is invalid (maybe output to STDERR).
    integer(ip4) :: irowt   = UNDEF_INT; ! i-th ROW of Telemetry that is the first one of this SF
    integer(ip4) :: sf2bits = UNDEF_INT; ! 2-bits SF in 16-byte Telemetry row header
    integer(ip4) :: nframes = UNDEF_INT; ! Number of FRAMES
    logical  :: with_frf = .false.; ! if True, %frf is valid.
    integer(ip4) :: irowf   = UNDEF_INT; ! i-th ROW of the original FRF-Array; -1 shall be set when with_frf is determined to be .false.
    type(asm_frfrow) :: frf = asm_frfrow(); ! -1 shall be (though not guaranteeed) set for %sfn and %lostf if with_frf is .false. 
       !integer(ip4) :: sfn     = UNDEF_INT; ! SF Number in FRF determined by SIRIUS
       !integer(ip4) :: lostf   = UNDEF_INT; ! number of LOST Frames (wrong "SYNC")
    integer(ip4) :: sfntelem  = UNDEF_INT; ! (guessed) SF Number based on the telemetry alone
    integer(ip4) :: mode_asm  = UNDEF_INT; ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
    integer(ip4) :: mode_slew = UNDEF_INT; ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
    integer(ip4) :: mode_PHA  = UNDEF_INT; ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    integer(ip4) :: mode_PHA_W56 = UNDEF_INT; ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    !integer(ip4) :: mode_real_stored = UNDEF_INT;
    integer(ip4) :: stat_asm_b = UNDEF_INT; ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213 ! "_b" for bit information (1 or 0)
    integer(ip4) :: stat_asa_b = UNDEF_INT; ! ON/OFF for ASM-A    F15W65B2
    integer(ip4) :: stat_amc_b = UNDEF_INT; ! ON/OFF for ASM-AMC  F15W65B3
    integer(ip4) :: stat_hv1_b = UNDEF_INT; ! ENA/DIS for ASM-HV1 F15W65B4
    integer(ip4) :: stat_hv2_b = UNDEF_INT; ! ENA/DIS for ASM-HV2 F15W65B5
    integer(ip4) :: stat_rbm_b = UNDEF_INT; ! ENA/DIS for ASM-RBM F15W65B6
    integer(ip4) :: stat_bdr_b = UNDEF_INT; ! ENA/DIS for ASM-BDR F15W65B7
    !character(len=max_fits_char) :: stat_asm = ''; ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
    !character(len=max_fits_char) :: stat_asa = ''; ! ON/OFF for ASM-A    F15W65B2
    !character(len=max_fits_char) :: stat_amc = ''; ! ON/OFF for ASM-AMC  F15W65B3
    !character(len=max_fits_char) :: stat_hv1 = ''; ! ENA/DIS for ASM-HV1 F15W65B4
    !character(len=max_fits_char) :: stat_hv2 = ''; ! ENA/DIS for ASM-HV2 F15W65B5
    !character(len=max_fits_char) :: stat_rbm = ''; ! ENA/DIS for ASM-RBM F15W65B6
    !character(len=max_fits_char) :: stat_bdr = ''; ! ENA/DIS for ASM-BDR F15W65B7
     integer(ip4) :: bitrate  = UNDEF_INT; ! [/s] Telemetry bit rate (F16W66) ! Taken from Telemetry as opposed to FRF
  end type asm_sfrow


  ! FITS output header
  type asm_header
    integer :: i_sframe, i_frame;
    character(len=18) :: TITLE    = 'ASM extracted data';
    character(len=5)  :: TELESCOP = 'Ginga';
    character(len=3)  :: INSTRUME = 'ASM';
    character(len=max_fits_char) :: FILENAME, date_obs;
    real    :: tsttasmm, tendasmm, tstts360, tends360;
    logical :: existdat = .true.;
    real, dimension(3) :: euler_s, euler_e;
    integer :: mode_dp;  ! 1/0 if ASM Mode is 'ON/OFF' (F4W66B3)
    integer :: mode_asm; ! 1/0 if ASM-PHA/Time Mode    (F4W66B4), ignoring F56W66B4
    character(len=max_fits_char) :: stat_asm, stat_asa, stat_amc;           ! ON/OFF
    character(len=max_fits_char) :: stat_hv1, stat_hv2, stat_rbm, stat_bdr; ! ENA/DIS
    integer :: bitrate;
  end type asm_header

  ! FITS output header comment
  type t_asm_header_comment
    character(len=max_fits_char) :: title = '', telescop = '', instrume = '';
    character(len=17) :: date_obs = '[day] Observation date';
    character(len=17) :: filename = 'Original filename';
    character(len=22) :: tsttasmm = 'Start time of ASM Mode';
    character(len=20) :: tendasmm = 'End time of ASM Mode';
    character(len=26) :: tstts360 = 'Start time of Slew360 Mode (F32n+10 W65B3)(=Status)';
    character(len=24) :: tends360 = 'End time of Slew360 Mode';
    character(len=30) :: existdat = 'True if the file contains data';
    character(len=37) :: fmt_euler_s  = '("[deg] Euler ", i1, " at the start")';
    character(len=35) :: fmt_euler_e  = '("[deg] Euler ", i1, " at the end")';
    character(len=35) :: mode_dp  = '1/0 if ASM Mode is ON/OFF (F4W66B3)';
    character(len=34) :: mode_asm = '1/0 if ASM-PHA/Time Mode (F4W66B4)';
    character(len=23) :: stat_asm = 'ON/OFF for ASM F15W65B1';  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
    character(len=25) :: stat_asa = 'ON/OFF for ASM-A F15W65B2';
    character(len=27) :: stat_amc = 'ON/OFF for ASM-AMC F15W65B3';
    character(len=28) :: stat_hv1 = 'ENA/DIS for ASM-HV1 F15W65B4';
    character(len=28) :: stat_hv2 = 'ENA/DIS for ASM-HV2 F15W65B5';
    character(len=28) :: stat_rbm = 'ENA/DIS for ASM-RBM F15W65B6';
    character(len=28) :: stat_bdr = 'ENA/DIS for ASM-BDR F15W65B7';
    character(len=32) :: bitrate  = '[/s] Telemetry bit rate (F16W66)';
  end type t_asm_header_comment

  type(t_asm_header_comment), parameter :: asm_comm = t_asm_header_comment()


  ! Specification of the ASM Table Types in FITS
  type t_form_unit
    character(len=LEN_READABLE_KEY) :: key; ! key for relating this to others
    character(len=max_fits_char) :: root; ! root for TTYPE, e.g., 'Euler' (=> Euler2 etc). Unless followed by a number, it should be identical with %key (for readability of the code)
    character(len=max_fits_char) :: form; ! Format like 1J
    character(len=max_fits_char) :: unit = '';
    character(len=max_fits_char) :: comm = ''; ! Comment (for TTYPE)
    integer :: dim = 1; ! dimension=3 for Euler (Euler1, 2, 3); for info purpose;
                        ! i.e., Euler1, Euler2, Euler3 must be in a different variable.
                        ! n.b., for ACS_C, though it is an array, the corresponding TTYPE is only 1, hence dim=1
  end type t_form_unit
  type(t_form_unit), dimension(19), parameter :: COL_FORM_UNITS = [ &
     ! note: I=Int*2, J=Int*4, D=Real*8
       t_form_unit(key='main',     root='',        form='1I', unit='count', comm='Main ASM data', dim=NWORDS_MAIN) & ! Special case: root should be explicitly specified when used. See function get_colheads()
     , t_form_unit(key='Tstart',   root='Tstart',  form='1D', unit='day', comm='Start datetime in MJD') &
     , t_form_unit(key='Euler',    root='Euler',   form='1D', unit='deg', comm='Euler angle', dim=3) &
     , t_form_unit(key='SFNum',    root='SFNum',   form='1J',  comm='As defined in FRF if defined') &
     , t_form_unit(key='SFNTelem', root='SFNTelem',form='1J',  comm='SF number based on Telemetry alone') & ! %sfntelem 
     , t_form_unit(key='SF2bits',  root='SF2bits', form='1I',  comm='2-bit SF from FI in Telemetry') &
     , t_form_unit(key='Fr6bits',  root='Fr6bits', form='1I',  comm='Frame number from FI in Telemetry') &
     , t_form_unit(key='i_frame',  root='i_frame', form='1J',  comm='i-th Frame in Telemetry from 1') &
     , t_form_unit(key='Mode_ASM', root='Mode_ASM', form='1I', comm='F4W66B3 ASM Mode (ON/OFF <=> 1/0)') & ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
     , t_form_unit(key='Mode_PHA', root='Mode_PHA', form='1I', comm='F4W66B4 ASM(TIME/PHA <=> 1/0)') & ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
       !   Note: The same is found at F56W66B4 (but it is ignored in this code).
     , t_form_unit(key='ModeSlew', root='ModeSlew', form='1I', comm='F10W65B3 ASM Slew360') & ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed))
     , t_form_unit(key='Status_C', root='Status_C',form='1I', comm='STATUS (W65) in every Frame') &
     , t_form_unit(key='Status_S', root='Status_S',form='8A', comm='STATUS (W65-8bit) in every Frame') &
       ! F32n+10 W65(=Status) B3:  Slew360 Mode (is ON "1"? (unconfirmed))
       !   Ref1: Table 5.1.12, pp.209
     , t_form_unit(key='DP_C',     root='DP_C',    form='1I', comm='DP (W66) in every Frame') &
     , t_form_unit(key='DP_S',     root='DP_S',    form='8A', comm='DP (W66-8bit) in every Frame') &
       ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
       !   Note: In short, F4 alone should be fine.  In practice, in some SFs,
       !         F4 may be missing.  In this code, such frames should be discarded.
       !   Ref1: Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
       !   Ref2: Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
       ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
       !   Note: The same is found at F56W66B4 (but it is ignored in this code).
       !   Ref2: Table 5.1.12, pp.214 “MODE” => B3(ASM ON/OFF), B4(ASM TIME/PHA)
     , t_form_unit(key='ACS_C',    root='ACS_C',   form='1I', comm='ACS (W33-35) in every frame', dim=DIM_ACS_C) &  ! dim=3 ! asm_telem_row%acss
     , t_form_unit(key='ASM1_C',   root='ASM1_C',  form='1I', comm='ASM W48-51 in every Frame', dim=DIM_ASM_C) &  ! dim=4 ! asm_telem_row%asm1_commons
     , t_form_unit(key='ASM2_C',   root='ASM2_C',  form='1I', comm='ASM W112-115 in every Frame', dim=DIM_ASM_C) &  ! dim=4 ! asm_telem_row%asm2_commons
     , t_form_unit(key='bitrate',  root='bitrate', form='1I', comm='Telemetry bitrate info (F16W66)') &
     ]

  ! Each column header info of the ASM Table to output as a FITS (i.e., column-based)
  !
  ! 1. An array of this type represents the info of the output FITS table data.
  ! 2. The index of the array represents the number of the talbe column, eg, TTYPE23
  ! 3. The array is created dynamically, rather than specified as a PARAMETER,
  !    because 'Y6CH15_Y6H07' etc is too awkward to be hard-coded.
  type t_asm_colhead
    character(len=LEN_READABLE_KEY) :: key; ! (internal) key for relating this to others
    character(len=max_fits_char) :: type; ! Column Name (=TTYPEn), set by deriving from %prm%root
    type(t_form_unit)           :: prm;  ! TFORM and TUNIT
  end type t_asm_colhead

  integer, parameter, private :: LEN_TTYPE = 16  ! maximum character length for TFORM (nb., 14 for 'Y23CH15_Y23H07')

  type fhead1i4
    integer(kind=ip4) :: val  = UNDEF_INT; ! eg., 40256
    character(len=8)  :: name = '' ; ! eg., 'NAXIS2'
    character(len=max_fits_char) :: comment = '' ; ! eg., 'number of rows in table'
    !character(len=1)  :: fmt = 'I';
  end type fhead1i4

  type fhead1r8
    real(kind=dp8)    :: val  = UNDEF_REAL; ! eg., 8.3e05
    character(len=8)  :: name = '' ; ! eg., 'DEC_2000'  
    character(len=max_fits_char) :: comment = '' ; ! eg., 'Declination in J2000'
    !character(len=1)  :: fmt = 'D';
  end type fhead1r8

  type fhead1ch
    character(len=max_fits_char) :: val  =  '' ; ! eg., 'ENA'
    character(len=8)  :: name = '' ; ! eg., 'STAT-RBM'
    character(len=max_fits_char) :: comment = '' ; ! eg., '"ENA" or "DIS"'
    !character(len=1)  :: fmt = 'S';
  end type fhead1ch

  type fhead1tf  ! (naming: tf = "true-false")
    logical           :: val       ; ! .true. OR .false.
    character(len=8)  :: name = '' ; ! eg., 'EXISTDAT'
    character(len=max_fits_char) :: comment = '' ; ! eg., '"ENA" or "DIS"'
    !character(len=1)  :: fmt = 'L';
  end type fhead1tf

  type fits_header
    ! If the name contains '__', it should be replaced with '-' as the FITS header name.
    ! -- Telemetry FITS ------------
    !! e.g., integer(kind=ip4) :: NAXIS =UNDEF_INT; ! 2 / 2-dimensional binary table
    type(fhead1i4) :: NAXIS   = fhead1i4(name='NAXIS',   comment='2-dimensional binary table');
    type(fhead1i4) :: NAXIS1  = fhead1i4(name='NAXIS1',  comment='width of table in bytes');       !   144
    type(fhead1i4) :: NAXIS2  = fhead1i4(name='NAXIS2',  comment='number of rows in table');       ! 40256
    type(fhead1i4) :: PCOUNT  = fhead1i4(name='PCOUNT',  comment='size of special data area');     !     0
    type(fhead1i4) :: GCOUNT  = fhead1i4(name='GCOUNT',  comment='one data group (required keyword)'); ! 1
    type(fhead1i4) :: TFIELDS = fhead1i4(name='TFIELDS', comment='number of fields in each row');  !     1
   !character(len=max_fits_char) :: TTYPE1  ='' ; ! 'Telemetry'          / label for field   1
   !character(len=max_fits_char) :: TFORM1  ='' ; ! '144B    '           / data format of field: BYTE
    type(fhead1ch) :: EXTNAME = fhead1ch(name='EXTNAME',  comment='name of this binary table extension'); ! 'SIRIUS binary'
    type(fhead1ch) :: ORIGIN  = fhead1ch(name='ORIGIN',   comment='Origin of the file.'); ! 'ISAS    '     
    type(fhead1ch) :: TELESCOP= fhead1ch(name='TELESCOP', comment='Telescope name');      ! 'GINGA   '     
    type(fhead1i4) :: SACD    = fhead1i4(name='SACD',     comment='Satelite_code'); !                   18
    type(fhead1i4) :: FRAMES  = fhead1i4(name='FRAMES',   comment='Number of frames in the file'); ! 40256
    type(fhead1ch) :: DATE__OBS=fhead1ch(name='DATE-OBS', comment='Start date and time of the file'); ! '1988-04-27T09:45:07.8320'
    type(fhead1i4) :: D_STA_C1= fhead1i4(name='D_STA_C1', comment='Data Start TICounter'); !  45496
    type(fhead1i4) :: D_STA_C2= fhead1i4(name='D_STA_C2', comment='Data Start FICounter'); !    128
    type(fhead1i4) :: D_STA_C3= fhead1i4(name='D_STA_C3', comment='Data Start Counter 3'); !      0
    type(fhead1ch) :: DATE__END=fhead1ch(name='DATE-END', comment='End date and time of the file');   ! '1988-04-28T02:45:23.2730'
    type(fhead1i4) :: D_END_C1= fhead1i4(name='D_END_C1', comment='Data End TICounter'); ! 256743
    type(fhead1i4) :: D_END_C2= fhead1i4(name='D_END_C2', comment='Data End FICounter'); !    191
    type(fhead1i4) :: D_END_C3= fhead1i4(name='D_END_C3', comment='Data End Counter 3'); !      0
    type(fhead1ch) :: DATE    = fhead1ch(name='DATE',     comment='file creation date (YYYY-MM-DDThh:mm:ss UT)'); ! '2020-02-04T10:18:42'
    ! -- Output FITS specific ------------
   !character(len=max_fits_char) :: EXTNAME ='' ; ! 'GINGA ASM'      / name of this binary table extension
    type(fhead1ch) :: TITLE   = fhead1ch(name='TITLE',    comment='ASM extracted data'); ! 
                                   ! DATE    ='' ; ! '2021-01-04' / [day] Creation date of this file
                                   ! TELESCOP='' ; ! 'Ginga'
    type(fhead1ch) :: INSTRUME= fhead1ch(name='INSTRUME', comment=''); ! 'ASM'
    type(fhead1ch) :: FILENAME= fhead1ch(name='FILENAME', comment='Original filename'); ! '/my/home/data/a123.fits'
                                  ! DATE-OBS='' ; ! '1990-01-23' / [day] Observation date (MJD)
    type(fhead1r8) :: TSTART  = fhead1r8(name='TSTART'  , comment='[day] Start time'); ! 8.12345678E05
    type(fhead1r8) :: TEND    = fhead1r8(name='TEND'    , comment='[day] End time');   ! 9.12345678E05
    type(fhead1r8) :: TSTARTMA= fhead1r8(name='TSTARTMA', comment='[day] Start time of ASM Mode');     ! 8.12345678E05
    type(fhead1r8) :: TENDMA  = fhead1r8(name='TENDMA'  , comment='[day] End time of ASM Mode');       ! 9.12345678E05
    type(fhead1r8) :: TSTARTMS= fhead1r8(name='TSTARTMS', comment='[day] Start time of Slew360 Mode'); ! 8.12345678E05
    type(fhead1r8) :: TENDMS  = fhead1r8(name='TENDMS'  , comment='[day] End time of Slew360 Mode');   ! 9.12345678E05
    type(fhead1i4) :: SSTARTMS= fhead1i4(name='SSTARTMS', comment='Start SF of Slew360 Mode'); ! 12345
    type(fhead1i4) :: SENDMS  = fhead1i4(name='SENDMS'  , comment='End SF of Slew360 Mode');   ! 66666
    type(fhead1tf) :: EXISTDAT= fhead1tf(val=.true., name='EXISTDAT', comment='True if the file contains data'); ! T
    type(fhead1r8) :: EQUINOX = fhead1r8(val=1950.0d0, name='EQUINOX', comment='Equinox of the coordinates'); ! 1950.0

    type(fhead1i4) :: SFRAMES = fhead1i4(name='SFRAMES',  comment='Number of SFs in the file'); ! 10000  ! Note: "TOTAL_SF" in the FRF file.
    type(fhead1i4) :: NROWSTEL= fhead1i4(name='NROWSTEL',  comment='Number of rows in orig Telemetry'); ! 50000  ! Note: number of rows = number of Frames (in the original Telemetry file)
    type(fhead1i4) :: MODE__OBS=fhead1i4(name='MODE-OBS', comment='F8N+4 W66');
    type(fhead1i4) :: STAT__OBS=fhead1i4(name='STAT-OBS', comment='F32N+15 W65');
    type(fhead1i4) :: DPID__OBS=fhead1i4(name='DPID-OBS', comment='F15N+16 W66');
    type(fhead1i4) :: MODE__B3 =fhead1i4(name='MODE-B3' , comment='1/0 for ASM Mode ''ON/OFF'''); !   1
    type(fhead1i4) :: MODE__B4 =fhead1i4(name='MODE-B4' , comment='1/0 for ASM-Time/PHA modes');  !   1
    type(fhead1i4) :: MODE__F56=fhead1i4(name='MODE-F56', comment='1/0 for ASM-Time/PHA modes in F56/W66 Bit 4'); ! 1
                                  ! COMMENT If negative, the frame does not exist. => the frame is discarded

    type(fhead1i4) :: ROW4STAT= fhead1i4(name='ROW4STAT', comment='Original Telemetry row number for STAT-*');
    type(fhead1ch) :: STAT__ASM=fhead1ch(name='STAT-ASM', comment='''ON'' or ''OFF'' at start');  ! 'ON' 
    type(fhead1ch) :: STAT__ASA=fhead1ch(name='STAT-ASA', comment='''ON'' or ''OFF'' for ASMA at start'); ! 'ON'
    type(fhead1ch) :: STAT__AMC=fhead1ch(name='STAT-AMC', comment='''ON'' or ''OFF'' at start');  ! 'OFF'
    type(fhead1ch) :: STAT__HV1=fhead1ch(name='STAT-HV1', comment='''ENA'' or ''DIS'' at start'); ! 'ENA'
    type(fhead1ch) :: STAT__HV2=fhead1ch(name='STAT-HV2', comment='''ENA'' or ''DIS'' at start'); ! 'ENA'
    type(fhead1ch) :: STAT__RBM=fhead1ch(name='STAT-RBM', comment='''ENA'' or ''DIS'' at start'); ! 'ENA'
    type(fhead1ch) :: STAT__BDR=fhead1ch(name='STAT-BDR', comment='''ENA'' or ''DIS'' at start'); ! 'DIS'
   !type(fhead1i4) :: BITRATE0= fhead1i4(name='BITRATE0', comment='[/s] Initial telemetry bit rate');   ! 32

    type(fhead1i4) :: FRFSFN_S= fhead1i4(name='FRFSFN_S', comment='FRF-SFNumber for Euler_S'); ! 156
    type(fhead1r8) :: FRFMJD_S= fhead1r8(name='FRFMJD_S', comment='[day] Central SF time for Euler_S'); ! -7.12345678E-2
    type(fhead1i4) :: FRFSFN_E= fhead1i4(name='FRFSFN_E', comment='FRF-SFNumber for Euler_E'); ! 234
    type(fhead1r8) :: FRFMJD_E= fhead1r8(name='FRFMJD_E', comment='[day] Central SF time for Euler_E'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_S1= fhead1r8(name='EULER_S1', comment='[deg] Euler 1 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_S2= fhead1r8(name='EULER_S2', comment='[deg] Euler 2 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_S3= fhead1r8(name='EULER_S3', comment='[deg] Euler 3 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E1= fhead1r8(name='EULER_E1', comment='[deg] Euler 1 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E2= fhead1r8(name='EULER_E2', comment='[deg] Euler 2 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E3= fhead1r8(name='EULER_E3', comment='[deg] Euler 3 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_S1= fhead1r8(name='D_EUL_S1', comment='[deg] D_Euler 1 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_S2= fhead1r8(name='D_EUL_S2', comment='[deg] D_Euler 2 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_S3= fhead1r8(name='D_EUL_S3', comment='[deg] D_Euler 3 before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_E1= fhead1r8(name='D_EUL_E1', comment='[deg] D_Euler 1 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_E2= fhead1r8(name='D_EUL_E2', comment='[deg] D_Euler 2 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: D_EUL_E3= fhead1r8(name='D_EUL_E3', comment='[deg] D_Euler 3 after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: ALTIT_S = fhead1r8(name='ALTIT_S', comment='[km] Altitude before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: ALTIT_E = fhead1r8(name='ALTIT_E', comment='[km] Altitude after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: LONLATS1= fhead1r8(name='LONLATS1', comment='[deg] Longitude before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: LONLATS2= fhead1r8(name='LONLATS2', comment='[deg] Latitude before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: LONLATE1= fhead1r8(name='LONLATE1', comment='[deg] Longitude after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: LONLATE2= fhead1r8(name='LONLATE2', comment='[deg] Latitude after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: DIST_E_S= fhead1r8(name='DIST_E_S', comment='[km] Distance-Earth before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: DIST_E_E= fhead1r8(name='DIST_E_E', comment='[km] Distance-Earth after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_ET_S1= fhead1r8(name='CO_ET_S1', comment='[deg] Alpha Earth center before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_ET_S2= fhead1r8(name='CO_ET_S2', comment='[deg] Delta Earth center before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_ET_E1= fhead1r8(name='CO_ET_E1', comment='[deg] Alpha Earth center after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_ET_E2= fhead1r8(name='CO_ET_E2', comment='[deg] Delta Earth center after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: COR_S   = fhead1r8(name='COR_S', comment='[GeV/c] Cutoff Rigidity before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: COR_E   = fhead1r8(name='COR_E', comment='[GeV/c] Cutoff Rigidity after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_EM_S1= fhead1r8(name='CO_EM_S1', comment='[deg] Alpha Earth B-field before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_EM_S2= fhead1r8(name='CO_EM_S2', comment='[deg] Delta Earth B-field before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_EM_E1= fhead1r8(name='CO_EM_E1', comment='[deg] Alpha Earth B-field after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_EM_E2= fhead1r8(name='CO_EM_E2', comment='[deg] Delta Earth B-field after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_SN_S1= fhead1r8(name='CO_SN_S1', comment='[deg] Alpha Sun before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_SN_S2= fhead1r8(name='CO_SN_S2', comment='[deg] Delta Sun before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_SN_E1= fhead1r8(name='CO_SN_E1', comment='[deg] Alpha Sun after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: CO_SN_E2= fhead1r8(name='CO_SN_E2', comment='[deg] Delta Sun after ASM Mode'); ! -7.12345678E-2
    type(fhead1i4) :: SUNPS_S = fhead1i4(name='SUNPS_S', comment='NSAS sees the sun before ASM Mode'); ! -7.12345678E-2
    type(fhead1i4) :: SUNPS_E = fhead1i4(name='SUNPS_E', comment='NSAS sees the sun after ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: ELVYS_S = fhead1r8(name='ELVYS_S', comment='[deg] Elevation in Y-axis before ASM Mode'); ! -7.12345678E-2
    type(fhead1r8) :: ELVYS_E = fhead1r8(name='ELVYS_E', comment='[deg] Elevation in Y-axis after ASM Mode'); ! -7.12345678E-2
    type(fhead1i4) :: EFLAGS_S= fhead1i4(name='EFLAGS_S', comment='EFLAGS(0-2) before ASM Mode'); ! -7.12345678E-2
    type(fhead1i4) :: EFLAGS_E= fhead1i4(name='EFLAGS_E', comment='EFLAGS(0-2) after ASM Mode'); ! -7.12345678E-2
    ! -- Misc ------------
    type(fhead1ch) :: FRFFILE = fhead1ch(name='FRFFILE',  comment='FRF Filename'); ! 'FR880428.S0220.fits'
    type(fhead1i4) :: TOTSFFRF= fhead1i4(name='TOTSFFRF', comment='Total number of SFs in the FRF'); ! 488
    ! -- Ginga FRF sample ------------
    type(fhead1ch) :: kPASS   = fhead1ch(name='PASS',     comment='Original GINGA pass number (yymmddpphh)'); ! '8804280220' ! "PASS" is a Fortran reserved word.
    type(fhead1i4) :: R_S_FLAG= fhead1i4(name='R_S_FLAG', comment='Real (0) or Stored (1) data'); ! 1
    type(fhead1ch) :: TARGET1 = fhead1ch(name='TARGET1',  comment='Target1 name in FRF'); ! 'W28'
    type(fhead1ch) :: TARGET2 = fhead1ch(name='TARGET2',  comment='Target2 name in FRF'); ! ''
    type(fhead1i4) :: TOTAL_SF= fhead1i4(name='TOTAL_SF', comment='Total number of Super-Frames in this file'); ! 488
                                  ! EXTNAME = 'GINGA_FRF'          / name of this binary table extension
                                  ! PASS    = '8804280220'         / Original GINGA pass number (yymmddpphh)
                                  ! R_S_FLAG=                    1 / Real (0) or Stored (1) data
                                  ! MESSAGE = 'NOVA ASM             880428' / Original message in the FRF
                                  ! BGN_DATE= '88 427 94539832'    / FRF start date (yymmddhhmmssmsc)
                                  ! END_DATE= '88 428 24451773'    / FRF end date (yymmddhhmmssmsc)
                                  ! TARGET1 = 'W28       '         / Target1 name in FRF
                                  ! TARG1RA =               269.45 / R.A. of Target1
                                  ! TARG1DEC=               -23.33 / DEC of Target1
                                  ! TARGET2 = '          '         / Target2 name in FRF
                                  ! TARG2RA =                 0.00 / R.A. of Target2
                                  ! TARG2DEC=                 0.00 / DEC of Target2
                                  ! LACTIME =                25888 / Total amount of LAC data time (sec)
                                  ! OSCHANGE=                   33 / Total number of OS changes
                                  ! TOTAL_SF=                  488 / Total number of Super-Frames in this file
                                  ! ORBEPOCH= '1988 4 23 8 0 0'    / Epoch of the orbital element (Y,M,D,h,m,s)
                                  ! ATTFILE = 'ASTROC.ATTITUDE.G08(M0067)          ' / Original attitude file ID
                                  ! ATT1TIME=                   32 / time for attitude 1 (sec)
                                  ! ATT1RA  =                90.00 / Y-axis RA for attitude 1 (deg, 1950)
                                  ! ATT1DEC =                 0.00 / Y-axis DEC for attitude 1 (deg, 1950)
                                  ! ATT2TIME=                25856 / time for attitude 2 (sec)
                                  ! ATT2RA  =               272.27 / Y-axis RA for attitude 2 (deg, 1950)
                                  ! ATT2DEC =               -18.41 / Y-axis DEC for attitude 2 (deg, 1950)
                                  ! ATT3TIME=                    0 / time for attitude 3 (sec)
                                  ! ATT3RA  =                 0.00 / Y-axis RA for attitude 3 (deg, 1950)
                                  ! ATT3DEC =                 0.00 / Y-axis DEC for attitude 3 (deg, 1950)
                                  ! FRFDATE = '0 9 11 15 24'       / Original FRF creation date and time (Y,M,D,h,m)
                                  ! ORIGIN  = 'ISAS    '           / Location where this file was created
                                  ! TELESCOP= 'GINGA   '           / Telescope name
                                  ! INSTRUME= 'LAC     '           / Instrument name
                                  ! CREATOR = 'GINGA_facom_FRF2FITS version 3.0 (2005-03-27)' / s/w task which wrote
                                  ! DATE    = '2005-03-27T21:33:25' / file creation date (YYYY-MM-DDThh:mm:ss UTC)
                                  ! CHECKSUM= 'ZGMDdDKCZDKCbDKC'   / HDU checksum updated 2005-03-27T21:33:25
                                  ! DATASUM = '3664369174'         / data unit checksum updated 2005-03-27T21:33:25
                                  ! CLKFILE = '/astroc/data/fits_frf_facom/clkfeduc.list' / clock correction file us
                                  ! COMMENT   The following entries of the "CLOCK-FIDUCIAL" files have been used
                                  ! COMMENT   to carry out the time correction on the present FITS FRF file.
                                  ! CLKFIDUC= ' Refer. PASS RawUT           CorrectUT       TI(hex)TI(decimal)'
                                  ! CLKFIDC1= ' R8804270619 880427092803833 880427092803823 B138    45368'
                                  ! CLKFIDC2= ' R8804280206 880428024611773 880428024611763 CFA2    53154'
                                  ! COMMENT   The "TI (Time Indicator)" is a 16-bit on-board time vernier whose
                                  ! COMMENT   interval is about 8 seconds.  The "CorrectUT" above gives the
                                  ! COMMENT   calibrated UT for the corresponding TI. Boundaries of TI and  Super-
                                  ! COMMENT   Frames coincide, that "CorrectUT" for each Super-Frame is calculated
                                  ! COMMENT   by interpolation of the CorrectUT-TI relations above.
                                  ! INTPITVL=           1.0381E+03 / Maximum interval for interpolation (minutes).
                                  ! COMMENT   The maximum interval used for the interpolation determines absolute
                                  ! COMMENT   timing accuracy.  When INTPITVL is ~100 minutes (contact pass), the
                                  ! COMMENT   accuracy is about 1 msec.  When INTPITVL is ~1000 minutes(remote
                                  ! COMMENT     pass), the accuracy is about 10 msec.
                                  ! MAXTDIFF=             -10.5757 / Maximum amount of time correction (msec)
  end type fits_header

contains

  type(t_invalid_fmt) function get_invalid_fmt(key)
    character(len=*), intent(in) :: key
    integer :: i
    
    do i=1, size(INVALID_FMT)
      if (trim(INVALID_FMT(i)%key) == trim(key)) then
        get_invalid_fmt = INVALID_FMT(i)
        return
      end if
    end do

    ! No matches
    write(stderr,'(A)') 'FATAL: Wrong key for get_invalid_fmt(): '//trim(key)
    call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
  end function get_invalid_fmt

  !-----------------------------------------
  ! interface get_index
  !   : to get the first index of the array which has the key
  !-----------------------------------------
  
  integer function get_index_char(key, ary, silent) result(iret)
    character(len=*), intent(in) :: key
    character(len=*), dimension(:), intent(in) :: ary
    logical, intent(in), optional :: silent

    logical :: is_silent
    integer :: i

    is_silent = .false.
    if (present(silent)) then
      if (silent) is_silent = .true.
    end if

    do i=1, size(ary)
      if (trim(ary(i)) == trim(key)) then
        iret = i
        return
      end if
    end do

    iret = UNDEF_INT
    if (.not. is_silent) write(stderr,'(A)') 'WARNING: (get_index_char) Index is not found for key="'//trim(key)//'"'
  end function get_index_char

  integer function get_index_argv(key, ary) result(iret)
    character(len=*), intent(in) :: key
    type(t_argv), dimension(:), intent(in) :: ary
    integer :: i

    do i=1, size(ary)
      if (trim(ary(i)%key) == trim(key)) then
        iret = i
        return
      end if
    end do
    write(stderr,'(A)') 'WARNING: (get_index_argv) Index is not found for key="'//trim(key)//'"'
    iret = UNDEF_INT
  end function get_index_argv


  ! Get the number associated with TTYPE in colhead, like 3 in TTYPEn='Euler3'
  integer function get_colhead_type_num(colhead, name) result(iret)
    type(t_asm_colhead), intent(in) :: colhead
    character(len=*), intent(in) :: name ! for the prefix, like 'Euler'. Mandatory
    integer :: status
    character(len=max_fits_char) :: kwd
    character(len=1024) :: usermsg

    read(colhead%type, '(A'//trim(ladjusted_int(len_trim(name)))//',I3)', iostat=status) kwd, iret

    if (trim(kwd) .ne. trim(name)) then
      print *, 'ERROR(get_colhead_type_num): colhead is:'
      call dump_type(colhead)
      write(usermsg, '("Key is not Expected=",A," <=> ",A)') trim(kwd), trim(name)
      call err_exit_play_safe(usermsg)
    end if

    if (status .ne. 0) then
      ! Read (format?) error
      print *, 'ERROR(get_colhead_type_num): colhead is:'
      call dump_type(colhead)
      write(usermsg, '("(get_colhead_type_num) Status is non-zero: ",I8)') status
      call err_exit_with_msg(usermsg)
    end if
  end function get_colhead_type_num


  integer function get_index_colhead(key, ary) result(iret)
    character(len=*), intent(in) :: key
    type(t_asm_colhead), dimension(:), intent(in) :: ary
    integer :: i

    do i=1, size(ary)
      if (trim(ary(i)%key) == trim(key)) then
        iret = i
        return
      end if
    end do
    write(stderr,'(A)') 'WARNING: (get_index_colhead) Index is not found for key="'//trim(key)//'"'
    iret = UNDEF_INT
  end function get_index_colhead

  integer function get_index_form_unit(key, ary, silent) result(iret)
    character(len=*), parameter :: subname = 'get_index_form_unit'
    character(len=*), intent(in) :: key
    type(t_form_unit), dimension(:), intent(in) :: ary
    logical, intent(in), optional :: silent

    logical :: is_silent
    integer :: i

    is_silent = .false.
    if (present(silent)) then
      if (silent) is_silent = .true.
    end if

    do i=1, size(ary)
      if (trim(ary(i)%key) == trim(key)) then
        iret = i
        return
      end if
    end do

    iret = UNDEF_INT
    if (.not. is_silent) write(stderr,'("WARNING: (",A,") Index is not found for key=''",A,"''")') &
       subname, trim(key)
  end function get_index_form_unit

  !-----------------------------------------
  ! interface get_element
  !   : to get the first element of the array which has the key
  !-----------------------------------------
  
  function get_element_char(key, ary) result(retobj)
    character(len=*), intent(in) :: key
    character(len=*), dimension(:), intent(in) :: ary
    character(len(ary)) :: retobj
    integer :: i

    retobj = ary(get_index(key, ary))  ! If not found, the result is uncertain.
  end function get_element_char

  function get_element_argv(key, ary) result(retobj)
    character(len=*), intent(in) :: key
    type(t_argv), dimension(:), intent(in) :: ary
    type(t_argv) :: retobj
    integer :: i

    retobj = ary(get_index(key, ary))  ! If not found, the result is uncertain.
  end function get_element_argv

  function get_element_colhead(key, ary) result(retobj)
    character(len=*), intent(in) :: key
    type(t_asm_colhead), dimension(:), intent(in) :: ary
    type(t_asm_colhead) :: retobj
    integer :: i

    retobj = ary(get_index(key, ary))  ! If not found, the result is uncertain.
  end function get_element_colhead

  function get_element_form_unit(key, ary) result(retobj)
    character(len=*), intent(in) :: key
    type(t_form_unit), dimension(:), intent(in) :: ary
    type(t_form_unit) :: retobj

    retobj = ary(get_index(key, ary))  ! If not found, the result is uncertain.
  end function get_element_form_unit

  !-----------------------------------------
  ! interface get_val_from_key
  !   : to get the (representative) value of the first element of the array which has the key
  !-----------------------------------------
  
  function get_val_from_key_argv(key, ary) result(retobj)
    character(len=*), intent(in) :: key
    type(t_argv), dimension(:), intent(in) :: ary
    character(len=LEN_T_ARGV) :: retobj
    type(t_argv) :: tmp

    tmp = get_element(key, ary)
    retobj = tmp%val  ! If not found, the result is uncertain.
  end function get_val_from_key_argv

  !-----------------------------------------
  
  ! Returns the object type(t_reason_invalid)
  !
  ! For the key, see the member 'key' of the parameter INVALID_FMT
  ! e.g., (no_frf|tel64|lostf|asmswoff|asmmodeoff)
  function get_reason_invalid(key, i1, i2) result(tret)
    character(len=*), intent(in) :: key
    integer, intent(in), optional :: i1, i2
    type(t_reason_invalid) :: tret

    type(t_invalid_fmt) :: invalid_fmt
    character(len=max_fits_char) :: rettext

    invalid_fmt = get_invalid_fmt(key)
    if      (invalid_fmt%nargs == 0) then
      write(rettext, invalid_fmt%fmt)
    else if (invalid_fmt%nargs == 1) then
      write(rettext, invalid_fmt%fmt) i1
    else if (invalid_fmt%nargs == 2) then
      write(rettext, invalid_fmt%fmt) i2
    else
      write(stderr,'(A)') 'Unsupported. Contact the code developer.'
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if
    tret = t_reason_invalid(key=key, text=rettext)
  end function get_reason_invalid

  ! Returns frame-row-index of type(asm_telem_row) that has the specified frame number like F32,
  ! with the search starting from a specified row number (istart; counted from 1).
  !
  ! If not found, a negative value is returned.
  ! NOTE: (Fortran Array index (from 1))
  !
  function get_telem_row_index_from_fr(frn, trows, istart, nrows) result(retrow)
    integer, intent(in) :: frn    ! Frame number to search for, eg., F32
    type(asm_telem_row), dimension(:), intent(in) :: trows
    integer, intent(in) :: istart ! Start row (in trows) for search
    integer, intent(in) :: nrows  ! Number of rows to search from istart
    integer :: retrow ! to return
    integer :: irow

    retrow = UNDEF_INT
    do irow=istart, istart+nrows-1
      if (frn == trows(irow)%fr_6bit) then
        retrow = irow
        return
      end if
    end do
  end function get_telem_row_index_from_fr

  ! (almost) Inverse of get_telem_row_index_from_fr
  !
  ! Find a SF-row object in sfrows from Telemetry-FITS row-number
  !
  ! If not found, the returned retsfrow%irowt is negative (or possibly zero, if specification changes).
  function get_sfrow_from_telem_row_index(in_rowt, sfrows) result(retsfrow)
    integer, intent(in) :: in_rowt    ! Row number of Telemetry
    type(asm_sfrow), dimension(:), allocatable, intent(in) :: sfrows
    type(asm_sfrow) :: retsfrow ! to return
    integer :: irow

    do irow=1, size(sfrows)
      if ((sfrows(irow)%irowt .le. in_rowt) .and. &
          (in_rowt .le. sfrows(irow)%irowt + sfrows(irow)%nframes)) then
        retsfrow = sfrows(irow)
        return
      end if
    end do
  end function get_sfrow_from_telem_row_index

  ! Returns the word number in Telemetry (128-words)
  !
  ! In default it counts from 0 (zero), namely W0 to W127,
  ! unless optional argument from1 is given and .true.,
  ! in which case it starts from 1 like a Fortran Array.
  !
  ! name is (fi|dp|status|pi_mon), so far, and in lower-cases ONLY.
  integer function w_no(name, from1) result(iret)
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: from1
    integer :: add1

    if (present(from1) .and. from1) then
      add1 = 1
    else
      add1 = 0
    end if

    select case(trim(name))
    case('fi')
      iret = TELEM_WORD_FROM0%fi
    case('dp')
      iret = TELEM_WORD_FROM0%dp
    case('status')
      iret = TELEM_WORD_FROM0%status
    case('pi_mon')
      iret = TELEM_WORD_FROM0%pi_mon
    case('acss')  ! The first word
      iret = TELEM_WORD_FROM0%acss
    case('asm1_commons')  ! The first word
      iret = TELEM_WORD_FROM0%asm1_commons
    case('asm2_commons')  ! The first word
      iret = TELEM_WORD_FROM0%asm2_commons
    case default
      iret = UNDEF_INT
    end select

    iret = iret + add1
  end function w_no

  ! Returns the frame, word, and bit numbers in Telemetry (128-words)
  ! for the given item
  !
  ! In default frame and word count from 0 (zero), namely F0 to F63 and W0 to W127,
  ! unless optional argument from1 is given and .true.,
  ! in which case it starts from 1 like a Fortran Array.
  ! Bit is always counted from 0 (like Fortran btest(i,pos)).
  !
  ! name is (fi|dp|status|pi_mon), so far, and in lower-cases ONLY.
  !
  ! If undefined, negative values for word and bit are returned (frame number is 0 or 1).
  function frame_word_first(name, from1) result(ret)
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: from1
    type(t_frame_word_bit) :: ret
    
    logical :: fr1
    integer :: iframe, iword, ibit
    integer :: add1

    if (present(from1) .and. from1) then
      add1 = 1
      fr1 = .true.
    else
      add1 = 0
      fr1 = .false.
    end if

    iframe = 0  ! Default (basically, any frame)
    iword = UNDEF_INT
    ibit  = UNDEF_INT
    if (     trim(name) == 'fi') then
      iword = w_no('fi', fr1)
    else if (trim(name) == 'dp') then
      iword = w_no('dp', fr1)
    else if (trim(name) == 'status') then
      iword = w_no('status', fr1)
    else if (trim(name) == 'pi_mon') then
      iword = w_no('pi_mon', fr1)
    !-----------
    else if (trim(name) == 'slew360') then ! F32n+10 W65(=Status) B3;  (is ON "1"? (unconfirmed))
      iframe = 10
      iword  = w_no('status', fr1)
      ibit   = 3
    else if (trim(name) == 'asm') then      ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
      iframe = 4
      iword  = w_no('dp', fr1)
      ibit   = 3
    else if (trim(name) == 'time_pha') then ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
      iframe = 4
      iword  = w_no('dp', fr1)
      ibit   = 4
    end if

    iframe = iframe + add1
    iword  = iword  + add1

    ret = t_frame_word_bit(frame=iframe, word=iword, bit=ibit) 
  end function frame_word_first

  ! Returns MJD of starting epoch of the first frame of a SF, based on sfrow/relrow
  real(kind=dp8) function sfrow2mjd(sfrow, trows) result(retmjd)
    type(asm_sfrow), intent(in) :: sfrow
    type(asm_telem_row), dimension(:), intent(in) :: trows

    retmjd = trows(sfrow%irowt)%mjd
  end function sfrow2mjd

  ! dump character array
  subroutine dump_chars(rows, prefix)
    character(len=*), dimension(:), intent(in) :: rows
    character(len=*), intent(in), optional :: prefix
    character(len=256) :: prefix_out
    integer :: i

    if (present(prefix)) then
      prefix_out = prefix
    else
      prefix_out = ''
    end if
    
    write(*,'(A,"[")',advance='no') trim(prefix_out)
    do i=1, size(rows)
      write(*,'("''",A,"''")',advance='no') trim(rows(i))
      if (i .ne. size(rows)) write(*,'(", ")',advance='no')
    end do
    write(*,'("]")')
  end subroutine dump_chars

  ! dump all_argv
  subroutine dump_all_argv(rows)
    type(t_argv), dimension(:), intent(in) :: rows
    integer :: i

    print *, '--------- all argv (size=', size(rows), ') ---------' 
    do i=1, size(rows)
      print *, trim(ladjusted_int(i))//': ('//trim(rows(i)%key)//') ', trim(rows(i)%val)
    end do
  end subroutine dump_all_argv

  ! dump internal FRF data
  subroutine dump_asm_telem_row(row)
    type(asm_telem_row), intent(in) :: row

    character(len=8) :: char_bin_stat, char_bin_dp
    integer :: i

    char_bin_stat = dump_int_lowest_8bits(row%STAT_OBS, ensure_range=.true.)
    char_bin_dp   = dump_int_lowest_8bits(row%DPID_OBS, ensure_range=.true.)

    print *, '--------- asm_telem_row (', row%i_frame, ') ---------' 
    write(*,'("TIME: ",I0.2,"-",I0.2," ",I0.2,":",I0.2,":",I0.2,".",I4, "(", F0.4, ") = MJD(",F19.12,")")') &
       row%month, row%day, row%hour, row%minute, row%second, row%millisec_i4, row%second_real, row%mjd
    write(*,'(" SF(2bit)=",I0.1," Frame(6bit)=",I0.2,' &
          //'" Status=",I3,"(=",A,") DP=",I3,"(=",A,")")') &
         row%sf_2bit, row%fr_6bit  &
       , row%STAT_OBS, trim(char_bin_stat), row%DPID_OBS, trim(char_bin_dp)
  end subroutine dump_asm_telem_row

  ! dump internal FRF data
  subroutine dump_asm_frfrow(row, isampl)
    type(asm_frfrow), intent(in) :: row
    integer, intent(in), optional :: isampl  ! i-th sample (out of up to 4)

    integer :: i

    if (present(isampl)) then
      i = isampl
    else
      i = 1
    end if

    print *, '--------- frfrow ---------' 
    print *, 'lostf   =', row%lostf
    print *, 'sfn     =', row%sfn
    print *, 'bitrate =', row%bitrate
    print *, 'relstr  =', row%relstr
    write(*,'("TIME: ",I4,"-",I0.2,"-",I0.2," ",I0.2,":",I0.2,":",I0.2,".",I4)') row%stime  ! '88' as opposed to '1988'
    print *, 'nsampl  =', row%nsampl
    print *, 'mjds    =', row%mjds(i)
    print *, 'eulers  =', row%eulers(:, i)
!print *, 'eulers  =', row%eulers(:, :)
    print *, 'd_eulers =', row%d_eulers(:, i)
    print *, 'height  =', row%height(i)
    print *, 'lon_lat =', row%lon_lat(:, i)
    ! real(dp), dimension(4) :: dist_earth ! [J=10] DISTANCE FROM THE EARTH CENTER                 
    ! real(dp), dimension(2, 4) :: coords_earth ! [J=11-12] ALPHA,DELTA OF THE EARTH CENTER (1950 EQUINOX)
    ! real(dp), dimension(4) :: cor ! [J=13] CUT OFF RIGIDITY
    ! real(dp), dimension(2, 4) :: coords_magnet ! [J=14-15] ALPHA,DELTA OF THE MAGNETIC FIELD
    ! real(dp), dimension(2, 4) :: coords_sun ! [J=16-17] ALPHA,DELTA OF THE SUN
    print *, 'sunps   =', row%sunps(i), '/PRESENCE OF SUNSHINE, 1/0=YES/NO'
    ! real(dp), dimension(4) :: elvys  ! ELEVATION OF YAXIS FROM THE EARTH EDGE                  
    ! real(dp), dimension(4) :: eflags ! CONDITION OF THE EARTH OCCULTATION                      
  end subroutine dump_asm_frfrow


  ! dump a relation Array
  subroutine dump_asm_sfrow(row, irow)
    type(asm_sfrow), intent(in) :: row
    integer(ip4), intent(in), optional :: irow  ! Row-No in the internal variable. Just for displaying purpose.

    character(len=16) :: s_lostf

    if (row%with_frf) then
      s_lostf = ladjusted_int(row%frf%lostf)
    else
      s_lostf = 'UNDEF('//trim(ladjusted_int(row%frf%lostf))//')'
    end if

    if (present(irow)) then
      write(*,'(" --------- asm_sfrow(row=", I4, ") ---------")') irow
    else
      print *, '--------- asm_sfrow ---------' 
    end if
    print *, 'is_valid   =', row%is_valid
    if (.not. row%is_valid) then
      write(*,'(" reason_invalid = ''", A, "''")') trim(row%reason_invalid%text)
    end if
    write(*,'(" irowt(irow_Telemetry)=",I6,", nFrames=",I5)') row%irowt, row%nframes
    write(*,'(" with_frf=",L1," irowf(irow_FRF)=",I6,", sfn(SF-No)=",I6,", lostf=",A6)') &
       row%with_frf, row%irowf, row%frf%sfn, trim(s_lostf)
    write(*,'(" mode_asm(1:ON)=",I4,", slew(1:ON)=",I4,", PHA(1:TIME)=",I4)') &
       row%mode_asm, row%mode_slew, row%mode_PHA
    write(*,'(" stat_asm(ON(1)/OFF)=''",A,"'', stat_hv1(ENA(1)/DIS)=''",A,"'', stat_rbm(ENA/DIS)=''",A,"''")') &
         trim(get_onoff_enadis(row%stat_asm_b, 'asm')), trim(get_onoff_enadis(row%stat_hv1_b, 'hv1')) &
       , trim(get_onoff_enadis(row%stat_rbm_b, 'rbm'))

    ! integer(ip4) :: bitrate  = UNDEF_INT; ! [/s] Telemetry bit rate (F16W66) ! Taken from Telemetry as opposed to FRF
  end subroutine dump_asm_sfrow

  subroutine dump_form_unit(fu, with_indent)
    type(t_form_unit), intent(in) :: fu
    logical, intent(in), optional :: with_indent
    logical :: w_indent
    character(len=1024) :: s

    w_indent = .false.
    if (present(with_indent)) then
      w_indent = with_indent
    end if

    s = '--------- asm_form_unit ---------' 
    if (w_indent) s = '  --------- %prm (=asm_form_unit) ---------'
    write(*,'(A)') trim(s)

    write(s,'("  k=''",A,"''(prm), TFORM=''",A,"'', TUNIT=''",A,"'', dim=",I2,", comm=''",A,"''")') &
       trim(fu%key), trim(fu%form), trim(fu%unit),fu%dim, trim(fu%comm)
    if (w_indent) s = '  '//s
    write(*,'(A)') trim(s)
  end subroutine dump_form_unit


  subroutine dump_asm_colhead(colhead)
    type(t_asm_colhead), intent(in) :: colhead

    if (.true.) then
      write(*,'("--------- asm_colhead ---------")')
      write(*,'("key=''",A,"'', TTYPE=",A,"''")') &
         trim(colhead%key), trim(colhead%type)
      call dump_form_unit(colhead%prm, with_indent=.true.)
      !print *, ' --------- %prm (=asm_form_unit) ---------' 
      !write(*,'("  k=''",A,"''(prm), TFORM=''",A,"'', TUNIT=''",A,"'', dim=",I2,", comm=''",A,"''")') &
      !   trim(colhead%prm%key), trim(colhead%prm%form), trim(colhead%prm%unit),colhead%prm%dim, trim(colhead%prm%comm)
    else  ! This is useful when memory trouble is rampant...
      print *, '--------- asm_colhead ---------' 
      print *,'key=',trim(colhead%key),' type=',trim(colhead%type)
      print *,'  k=',trim(colhead%prm%key),' form=',trim(colhead%prm%form),' unit=',trim(colhead%prm%unit)&
       ,' dim=',colhead%prm%dim, ' comm=',trim(colhead%prm%comm)
    end if
  end subroutine dump_asm_colhead

  subroutine dump_fits_header(fhead, iext)
    type(fits_header), intent(in) :: fhead
    integer, intent(in), optional :: iext  ! Extension number (to display)
    integer :: naxis

    naxis = fhead%NAXIS%val

    if (present(iext)) then
      write(*,'("--------- fits_header (Ext=",I2,") ---------")') iext
    else
      write(*,'("--------- fits_header ---------")')
    end if

    select case(naxis)
    case(0)
      write(*,'(" NAXIS=0")')
    case(1)
      write(*,'(" NAXIS=1, NAXIS1=",I7)') fhead%NAXIS1%val
    case(2)
      write(*,'(" NAXIS=2, (NAXIS1,2)=(",I7,",",I7,")")') fhead%NAXIS1%val, fhead%NAXIS2%val
    case default
      write(*,'(" NAXIS=",I7)') fhead%NAXIS%val
    end select
    write(*,'(" TITLE=''",A,"'', EXTNAME=''",A,"'', TFIELDS= ",I7)') &
       trim(fhead%TITLE%val), trim(fhead%EXTNAME%val), fhead%TFIELDS%val
    write(*,'(" TELESCOP=''",A,"'', INSTRUME=''",A,"'', SACD=",I3)') &
       trim(fhead%TELESCOP%val), trim(fhead%INSTRUME%val), fhead%SACD%val
    write(*,'(" FILENAME= ''",A,"'' / ",A)') trim(fhead%FILENAME%val), trim(fhead%FILENAME%comment)
    write(*,'(" FRFFILE= ''",A,"'' / ",A)') trim(fhead%FRFFILE%val), trim(fhead%FRFFILE%comment)
    write(*,'(" ",A,"=''",A,"'' / ",A)') &
       trim(fhead%DATE__OBS%name), trim(fhead%DATE__OBS%val), trim(fhead%DATE__OBS%comment)
    write(*,'(" PASS= ''",A,"'', TARGET1= ''",A,"''")') trim(fhead%kPASS%val), trim(fhead%TARGET1%comment)
    write(*,'("-------------------------------")')
  end subroutine dump_fits_header


  ! Calculates the asmdats and telemetry row numbers for the first ASM data in the given 16-byte row number
  !
  ! Basically, asm_telem_row%asmdats is similar to the telemetry rows, except the first 4 bytes (offset) in every 16 bytes are removed.
  !
  ! for examle, if (1,5) for irow16=1 (first in the 128-bytes frame), (13, 21) for irow16=2 (second)
  ! (85, 117) for irow16=8 (last).
  subroutine calc_rows_asmdats_telem(irow16, iasm, itel)
    implicit none
    integer, intent(in) :: irow16
    integer, intent(out) :: iasm, itel

    itel = (irow16-1)*16 + 4 + 1
    iasm = (irow16-1)*12 + 1
  end subroutine calc_rows_asmdats_telem


  ! Table5.5.5-6 (pp.233-234)
  function get_asmmain_row(acard) result(arret)
    implicit none
    integer(kind=1), dimension(NBYTESPERCARD), intent(in) :: acard ! == telems(1:128, i) ! Name: "a card"
    integer, dimension(NWORDS_MAIN) :: arret

    integer :: idet, ich, i_tele, i_out

    arret = 0
    do idet=1, NUM_INSTR  ! =6
      do ich=1, NCHANS_TIME  ! =8
        !i_tele = (idet-1)*2 + (ich-1)*16 + 5
        !i_out = (idet-1)*NCHANS_PHA + ich
        !arret(i_fr64) = telems(i_tele, itotrow)
        !    
        !i_tele = (idet-1)*2 + (ich-1)*16 + 6
        !i_out = (idet-1)*NCHANS_PHA + ich + 8
        !arret(i_fr64) = telems(i_tele, itotrow)
      end do
    end do
  end function get_asmmain_row


  ! Gets the corresponding row number of asm_telem_row%asmdats (see get_telem_raws2types() in asm_read_telemetry)
  ! for the output column (TTYPEn).
  !
  ! (Table5.5.5-6 (pp.233-234))
  ! For example,
  !   row=1  (Y1-FW1-CH00, W4 (Wn for n=0..127, CHnn for nn=0..15, Y1/2, FW1/2)) for TTYPE=1
  !   row=2  (Y1-FW1-CH08, W5)  for TTYPE=9
  !   row=13 (Y1-FW1-CH01, W20) for TTYPE=2
  !
  ! This routine gives the row number [1,2,13,...] for the column number [1,9,2,...].
  !
  integer function get_asmdats_row4col(index_col) result(i_asmdats)
    implicit none
    integer, intent(in)  :: index_col
    integer :: ifw, iy1, ichp, icht, idiv8

    integer :: idet, ich, i_tele, i_out

    call split_i_ttype_asmmain(index_col, ifw, iy1, ichp, icht, idiv8)

    ! ioff_vert = icht*12                        ! Vertial offset (every 12)
    ! ioff_hori = ((ifw-1)*2+(iy1-1))*2+idiv8+1  ! Horizontal offset (after some multiple of 12)
    i_asmdats =  icht*12 + ((ifw-1)*2+(iy1-1))*2+idiv8+1
  end function get_asmdats_row4col

  subroutine split_i_ttype_asmmain(index, ifw, iy1, ichp, icht, idiv8)
    implicit none
    integer, intent(in)  :: index
    integer, intent(out) :: ifw, iy1,  ichp,    icht,   idiv8 ! (I-DIVided-by-8)
                        ! FW1-3, Y1-2, CH00-15, CH00-07, 0-1(L-or-H in Time-mode)
                        !        Mode: PHA      TIME (I-CHannel-Time)
    integer :: ifwmod

    ifw    =     (index-1)/(NWORDS_MAIN/3)   + 1 ! FW1--FW3
    ifwmod = mod( index-1,  NWORDS_MAIN/3)   + 1 ! (1..32)
    iy1    =    (ifwmod-1)/(NWORDS_MAIN/3/2) + 1 ! Y1--Y2
    ichp   = mod(ifwmod-1,  NWORDS_MAIN/3/2)     ! CH0--CH15 for PHA-mode (0..15)
    idiv8  =     ichp/(NCHANS_PHA/2)                  ! 0 for Ch00-07, 1 for Ch08-15 
    icht   = mod(ichp, NCHANS_PHA/2)            ! CH0--CH07 for TIME-mode (0..7)
  end subroutine split_i_ttype_asmmain

  ! Return the TTYPE string of the Main ASM data part for the given index [1:96] (eg, TTYPE5).
  ! Table5.5.5-6 (pp.233-234)
  !
  ! Examples:
  !
  !   TFORM1= 'Y11CH00_Y11L00',
  !   TFORM17='Y21CH00_Y21L00',
  !   TFORM32='Y21CH15_Y21H07',
  !   TFORM33='Y12CH00_Y12L00',
  !   TFORM96='Y23CH15_Y23H07',
  !
  character(len=LEN_TTYPE) function get_ttype_main(index) result(ret)
    implicit none
    integer, intent(in) :: index

    integer :: iy1, ifw, ifwmod, ichp, icht  ! ichp for PHA-mode, icht=mod(ichp, 8) for TIME-mode
    character(len=1) :: low_high
    character(len=LEN_TTYPE) :: stmp
    character(len=1) :: ciy1, cifw
    character(len=2) :: cichp, cicht

    ret = ''
!print *,'DEBUG:632:start'
    ! Y11, Y21, Y12, Y22, Y13, Y23
    ifw    =     (index-1)/(NWORDS_MAIN/3)   + 1 ! FW1--FW3
    ifwmod = mod( index-1,  NWORDS_MAIN/3)   + 1 ! (1..32) !!!!!!!!!!! ifwmod = mod((index-1), NWORDS_MAIN/3)
    iy1    =    (ifwmod-1)/(NWORDS_MAIN/3/2) + 1 ! Y1--Y2
    ichp   = mod(ifwmod-1,  NWORDS_MAIN/3/2)     ! CH0--CH15 for PHA-mode (0..15)
    low_high = 'H'
!print *,'DEBUG:642:bef_lowh'
    if (ichp/(NCHANS_PHA/2) == 0) low_high = 'L'  ! else Default 'H'
    icht   = mod(ichp,  NCHANS_PHA/2)             ! CH0--CH07 for TIME-mode (0..7)

    if (.true.) then  ! set it .false. when memory trouble is rampant...
      write(ret, '("Y", I1, I1, "CH", I0.2, "_Y", I1, I1, A1, I0.2)') &
         iy1, ifw, ichp, iy1, ifw, low_high, icht
      return
    end if

    ! ------------- below: not executed in default (unless the above .true. is reset to .false.)
    !               This can be useful when memory trouble is rampant.
!print *,'DEBUG:642-1, iy1=', iy1
    select case(iy1)
    case (1)
      ciy1='1'
    case (2)
      ciy1='2'
    case default
      print *,'iy1 is NOT 1 or 2, but=',iy1
      call EXIT(1)
    end select
        !write(ciy1, '(I1)') iy1
!print *,'DEBUG:642-2, ifw=',ifw
    select case(ifw)
    case (1,2,3)
      cifw=char(ifw+48)
    case default
      print *,'ifw is NOT 1 or 2 or 3, but=',ifw
      call EXIT(1)
    end select
        !write(cifw, '(I1)') ifw
!print *,'DEBUG:642-3'
!print *,'DEBUG:642-3-cifw=',cifw,' ichp=',ichp
    cichp = char(ichp/10+48)
    cichp(2:2) = char(mod(ichp,10)+48)
        !write(cichp, '(I0.2)') ichp
!print *,'DEBUG:643:aft_lowh, cichp=',cichp
!print *,'DEBUG:643-0:icht=',icht
    cicht = '00'
    cicht(2:2) = char(icht+48)
!print *,'DEBUG:643-1, icht=', icht, ' cicht=',cicht
        !write(cicht, '(I0.2)') icht
!print *,'DEBUG:644:bef_wri,iy1=',iy1,' ifw=',ifw,' ichp=',ichp
    ret = 'Y'//ciy1//cifw//'CH'//cichp//'_Y'//ciy1//low_high//cicht
!print *,'DEBUG:644-1:bef_wri,ret=',ret
  end function get_ttype_main

  
  ! Modify the comment of a FITS header for a TTYPEn column
  !
  ! ckey is defined in COL_FORM_UNITS(i)%key (in asm_fits_common)
  subroutine modify_ttype_comment(unit, ittype, ckey, status)
    implicit none
    integer, intent(in) :: unit
    integer, intent(in) :: ittype  ! TTYPEn number
    character(len=*), intent(in) :: ckey
    integer, intent(out) :: status

    type(t_form_unit) :: colprm

    colprm = get_element(ckey, COL_FORM_UNITS) ! defined in asm_fits_common
    call FTMCOM(unit, trim(ladjusted_int(ittype, 'TTYPE')), colprm%comm, status) ! defined in fort_util
  end subroutine modify_ttype_comment
  

  ! Returns String like 'ASMSF002' for the num-th extension
  character(len=nchar_extname) function get_extname(num)
    integer, intent(in) :: num  ! starts from 1, i.e., num==1 for the 1st extension

    write(get_extname, '("ASMSF", i0.3)') num
    return
  end function get_extname

  !-----------------------------------------
  ! interface get_ncols_colheads
  !   : to calculate the summed number of (FITS) columns, aka TTYPEs
  !-----------------------------------------

  ! Number of the total columns (TTYPEs) of a Colheads corresponding to the given Character array
  !
  ! If the keyword is not found, a negative value is returned.
  function get_ncols_colheads_char(ckeys) result(nframes)
    character(len=*), dimension(:), intent(in) :: ckeys
    integer :: nframes ! return
    integer :: i, ind
    type(t_form_unit) :: tmp_fu

    nframes = 0
    do i=1, size(ckeys)
      ind = get_index(trim(ckeys(i)), COL_FORM_UNITS)
      if (ind < 0) then
        ! NOT found
        nframes = UNDEF_INT
        return
      end if
      nframes = nframes + COL_FORM_UNITS(ind)%dim
      !tmp_fu = get_element(trim(ckeys(i)), COL_FORM_UNITS)  ! With this, an error is hard to capture.
      !nframes = nframes + tmp_fu%dim
    end do
  end function get_ncols_colheads_char

  ! Number of the total columns (TTYPEs) of a Colheads
  function get_ncols_colheads_colheads(colheads) result(nframes)
    type(t_asm_colhead), dimension(:), intent(in) :: colheads
    integer :: nframes ! return
    integer :: i
    type(t_form_unit) :: tmp_fu

    nframes = sum(colheads%prm%dim)
  end function get_ncols_colheads_colheads

  ! Number of the total columns (TTYPEs) of an array of frame_unit
  function get_ncols_colheads_frameunits(form_units) result(nframes)
    type(t_form_unit), dimension(:), intent(in) :: form_units
    integer :: nframes ! return
    integer :: i

    nframes = sum(form_units%dim)
  end function get_ncols_colheads_frameunits

  ! Number of the total columns (TTYPEs) of COL_FORM_UNITS
  function get_ncols_colheads_none() result(nframes)
    integer :: nframes ! return

    nframes = sum(COL_FORM_UNITS%dim)
  end function get_ncols_colheads_none
  !-----------------------------------------

  
  ! Set 1 element of the given colheads (Array object of TTYPE)
  !
  ! Condition: TTYPE and key agree, except for the dimension
  !
  ! If member "dimension" in COL_FORM_UNITS is greater than 1, this sets multiple TTYPES
  ! for the "dimsion" number.  For example, key=Euler has dimsion=3; thus
  ! EULER1, EULER2, EULER3 are set.
  !
  ! increment is the number of indices (i.e., dimensions) this routine has filled.
  subroutine set_colheads_single(index_start, key, colheads, index_last)
    integer, intent(in) :: index_start ! colheads(index_start) and onwards will be set in this routine.
    character(len=*), intent(in) :: key
    type(t_asm_colhead), dimension(:), intent(inout) :: colheads
    integer, intent(out) :: index_last ! up to colheads(index_last) is set after this routine.  If negative, something has gone wrong.

    type(t_form_unit) :: colprm
    character(len=MAX_LEN_FKEY) :: sk
    character(len=max_fits_char) :: root, ttype ! TTYPE (for temporary use)
    character(len=10240) :: msg
    integer :: idim, i

!print *,'DEBUG:423:set_colhn'    
!call dump_type(ary(size(ary)))
    !colprm = get_element(key, COL_FORM_UNITS)  ! With this, error is difficult to catch.
    i = get_index(key, COL_FORM_UNITS)
!print *,'DEBUG:424:i=',i
    if (i < 0) then
      ! No column is found!
      index_last = UNDEF_INT
      write(stderr, '(A)') 'WARNING(set_colheads_single): Key='//trim(key)//' is not defined in COL_FORM_UNITS.'
!call err_exit_with_msg(msg)
      return
    end if
    colprm = COL_FORM_UNITS(i)

    index_last = index_start + colprm%dim - 1
    if (colprm%dim > 1) then
      do idim=1, colprm%dim
        ttype = trim(ladjusted_int(idim, trim(colprm%root))) ! defined in fort_util
        colheads(index_start+idim-1) = t_asm_colhead(key=key, type=ttype, prm=colprm)
      end do
    else if (colprm%dim < 0) then
      call err_exit_play_safe()
    else  ! ie. (dimension == 1)
      colheads(index_start) = t_asm_colhead(key=key, type=key, prm=colprm)
    end if
  end subroutine set_colheads_single

  ! Returns Array of type(t_asm_colhead) that is the FITS Column-head information
  ! for the given Array of Character.
  function get_colheads(ckeys) result(colheads)
    character(len=*), dimension(:), intent(in), optional :: ckeys
    type(t_asm_colhead), dimension(:), allocatable :: colheads

    ! Output columns (8 char max?)
    ! This may be defined as equal to COL_FORM_UNITS(:)%key
    ! Justification to redefine it here is you can control what to handle in this subroutine,
    ! independent of (module-wide constant parameter) COL_FORM_UNITS
    character(len=MAX_LEN_FKEY), dimension(:), allocatable :: colhead_keys
    integer, dimension(:), allocatable :: dims  ! Array of t_form_unit%dim corresponding to ckeys
    character(len=max_fits_char) :: ttype ! TTYPE (for temporary use)
    character(len=LEN_READABLE_KEY) :: sk
    integer :: i, irow, ikey, ittype, ilast, nsiz, increment
    type(t_form_unit) :: tmp_fu
    character(len=1024) :: msg
!character(len=LEN_TTYPE) :: tmp_cha  ! for DEBUG

    if (present(ckeys)) then
      colhead_keys = ckeys
      nsiz = get_ncols_colheads(colhead_keys)
      if (nsiz < 0) then
        call dump_chars(colhead_keys, 'ERROR: in get_colheads, given keys=')
        ! The keyword was not found!
        msg = 'One (or more) of the keys is not defined in COL_FORM_UNITS (Failed in get_colheads).'
        call err_exit_with_msg(msg)
      end if
    else
      colhead_keys = COL_FORM_UNITS(:)%key
      nsiz = get_ncols_colheads()
    end if

    allocate(dims(size(colhead_keys)))
    allocate(colheads(nsiz))

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  It may not start from NWORDS_MAIN. irow increases by %prm%dim
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Algorithm
    !  Make formunits%key%dim array corresponding to the input Character-array
    !  Loop over i of colhead_keys (=formunits%key%dim)
    !  Make TFORM etc for each i like 96 main ones.

    ittype = 0  ! 23 for TTYPE23 => colheads(ittype)
    do ikey=1, size(colhead_keys)
      select case(trim(colhead_keys(ikey)))
      case('main') ! Special case (ASM main data), where %prm%root is ignored.
        do irow=1, NWORDS_MAIN
          sk = 'main'
!print *,'DEBUG:524:irow=',irow
!tmp_cha = get_ttype_main(irow)
!print *,'DEBUG:52X:tmp_cha=',tmp_cha
!print *,'DEBUG:525:ttb'
!tmp_fu = get_element(sk, COL_FORM_UNITS)
!print *,'DEBUG:526:ok'
!print *,'DEBUG:527:tmp_fu',trim(tmp_fu%key)
          ttype = get_ttype_main(irow)
          ittype = ittype + 1
if (ittype > nsiz) call err_exit_play_safe()
          colheads(ittype) = t_asm_colhead(key=sk, type=trim(ttype), prm=get_element(sk, COL_FORM_UNITS))
        end do
      case default
        call set_colheads_single(ittype+1, colhead_keys(ikey), colheads, index_last=ilast)
        if (ilast < 0) then
          ! The keyword was not found!
          msg = 'Key='//trim(colhead_keys(ikey))//' is not defined in COL_FORM_UNITS (Failed in get_colheads).'
          call err_exit_with_msg(msg)
        end if
        ittype = ilast
if (ittype > nsiz) call err_exit_play_safe()
      end select
    end do

    if (allocated(colhead_keys)) deallocate(colhead_keys)
  end function get_colheads


  ! Print the result of calc_proc_stats() to STDOUT
  !
  subroutine print_proc_stats(trows, relrows, frfrows)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    type(asm_frfrow), dimension(:), intent(in) :: frfrows
    character(len=LEN_PROC_STATS), dimension(:), allocatable :: ar_strs_stats
    integer :: j

    ar_strs_stats = calc_proc_stats(trows, relrows, frfrows)  ! allocatable
    write(*,'("----------- Processing Statistics -----------")')
    do j=1, size(ar_strs_stats)
      write(*,'(A)') trim(ar_strs_stats(j))
    end do
    write(*,'("---------------------------------------------")')
    if (allocated(ar_strs_stats)) deallocate(ar_strs_stats)
  end subroutine print_proc_stats

  ! Returns String of process statistics
  !
  ! TODO: Discarded due to ASM at the beginning, end, middle
  !
  ! Note: Call print_proc_stats() to output to STDOUT
  function calc_proc_stats(trows, sfrows, frfrows) result(strs_stats)
    integer, parameter :: n_b4bd = 5, SIZE_FMT = size(INVALID_FMT)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), intent(in) :: sfrows
    type(asm_frfrow), dimension(:), intent(in) :: frfrows
    character(len=LEN_PROC_STATS), dimension(n_b4bd+SIZE_FMT+2) :: strs_stats
    character(len=LEN_PROC_STATS) :: s

    logical, dimension(size(trows)) :: mask_tr
    integer :: n_fr_output, n_tot, n_discarded
    integer :: i, j, n, ifmt

    n_fr_output = sum(sfrows%nframes, sfrows%is_valid)
    write(s,'("Output/Discarded/Telemetry numbers of frames: ", I6, " /", I6, " /", I6)') &
       n_fr_output, size(trows)-n_fr_output, size(trows)
    strs_stats(n_b4bd-4) = s  ! Index=1
    n = count(sfrows%is_valid)
    n_discarded = size(sfrows)-n
    write(s,'("Output/Discarded/Telemetry numbers of SFs: ", I5, " /", I5, " /", I5)') &
       n, n_discarded, size(sfrows)
    strs_stats(n_b4bd-3) = s  ! Index=2
    strs_stats(n_b4bd-2) = '  (Note: SF number of Telemetry may not be strictly accurate.)' ! Index=3
    write(s,'("FRF/matched(be4-discarded) numbers of SFs: ", I5, " /", I5)') &
       size(frfrows), count(sfrows%with_frf)
    strs_stats(n_b4bd-1) = s ! Index=4
    strs_stats(n_b4bd)   = '  Breakdown: discarded due to:' ! Index=5
    n_tot = 0
    do ifmt=1, SIZE_FMT             ! Index=5..X
      ! Counting the number of Frames for each "reason_invalid" (which is defined only when is_valid==.false.)
      n = 0
      do i=1, size(sfrows)
        if (.not. sfrows(i)%is_valid) then
          if (trim(sfrows(i)%reason_invalid%key) == trim(INVALID_FMT(ifmt)%key)) n = n + 1
        end if
      end do
      write(s,'("    ",I5,": ",A)') n, trim(INVALID_FMT(ifmt)%desc)
      strs_stats(n_b4bd+ifmt) = s
      n_tot = n_tot + n
    end do
    write(s,'("    ",I5,"=(Total) (for sanity-check <=> ",I5,")")') n_tot, n_discarded
    strs_stats(n_b4bd+SIZE_FMT+1) = s  ! Index: Last-1
    write(s,'("Telemetry SFs without FRF counterparts vs undefined sfn: ",I5," <=> ",I5," (for sanity-check)")') &
       count(.not. sfrows%with_frf), count(sfrows%frf%sfn < 0)
    strs_stats(n_b4bd+SIZE_FMT+2) = s  ! Index: Last
  end function calc_proc_stats

  ! Inverse of get_onoff_enadis(). Returns either 0 or 1.
  !
  ! If neither, return a negative value (-999)
  integer function get_0or1_from_onoff(val) result(iret)
    character(len=*), intent(in) :: val

    if (     (trim(val) == 'ON')  .or. (trim(val) == 'on')  .or. &
             (trim(val) == 'ENA') .or. (trim(val) == 'ena')) then
      iret = 1
    else if ((trim(val) == 'OFF') .or. (trim(val) == 'off')  .or. &
             (trim(val) == 'DIS') .or. (trim(val) == 'dis')) then
      iret = 0
    else
      iret = UNDEF_INT
      !write(stderr,'(A)') 'FATAL: Neither ON/OFF nor ENA/DIS: "'//trim(val)//'"'
      !call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if
  end function get_0or1_from_onoff

  !--------------------------------------------
  ! get_onoff_enadis()
  !--------------------------------------------

  ! Returns either 'ON' (OFF) or 'ENA' (DIS)
  !
  ! key=(asm|asa|amc|hv1|hv2|rbm|bdr)
  character(len=8) function get_onoff_enadis_from_key(ival, key) result(retstr)
    integer, intent(in) :: ival
    character(len=*), intent(in) :: key
    logical :: is_onoff

    select case(trim(key))
    case ('asm','asa','amc','ASM','ASA','AMC')
      is_onoff = .true.
    case ('hv1','hv2','rbm','bdr','HV1','HV2','RBM','BDR')
      is_onoff = .false.
    case default
      write(stderr,'(A)') 'FATAL: Wrong argument. Contact the code developer. key="'//trim(key)//'"'
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end select

    retstr = get_onoff_enadis(ival, is_onoff=is_onoff)
  end function get_onoff_enadis_from_key

  ! Returns 'ON|OFF' or 'ENA|DIS' if ival==(1|0) (len=3).
  !
  ! If neither 1 nor 0, 'UNK'
  function get_onoff_enadis_from_tf(ival, is_onoff) result(ret)
    integer, intent(in) :: ival
    logical, intent(in) :: is_onoff  ! if True, 'ON|OFF' is returned, else 'ENA|DIS'
    character(len=3) :: ret
    character(len=16), parameter :: subname = 'get_onoff_enadis'

    ret = 'UNK'
    if (ival == 1) then
      if (is_onoff) then
        ret = 'ON'
      else
        ret = 'ENA'
      end if
    else if (ival == 0) then
      if (is_onoff) then
        ret = 'OFF'
      else
        ret = 'DIS'
      end if
    else
      write(stderr,'("(",A,") WARNING: value is neither 0 nor 1 but (", I8, ")")') subname, ival
    end if
  end function get_onoff_enadis_from_tf

  ! Returns true if the environmental variable DEBUG is set and NOT 'false' or 'no'.
  logical function IS_DEBUG() result(ret)
    character(len=255) :: env_debug
    integer :: status
    integer, save :: prev_result = -99

    ! To avoid repeatedly accessing the system to get the environmental variable.
    if (prev_result < 0) then  ! This IF-statment is redundant (but is left for readability).
      select case(prev_result)
      case(0)
        ret = .false.
        return
      case(1)
        ret = .true.
        return
      end select
    end if
        
    call GET_ENVIRONMENT_VARIABLE('GINGA_DEBUG', env_debug, STATUS=status)
    if ((status == 1) .or. (trim(env_debug) == 'false') .or. (trim(env_debug) == 'no')) then  ! 1 for non-existent, 2 for environment var. not-supported by the system
      ret = .false.
      prev_result = 0
    else
      ret = .true.
      prev_result = 1
    end if
  end function IS_DEBUG
end module asm_fits_common

