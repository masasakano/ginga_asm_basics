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
  integer, parameter, public :: num_instr = 6, nchan_pha = 16, nchan_time = 8  ! Number of channels in each mode 
  integer, parameter, public :: NBYTESPERCARD = 144
  integer, parameter, public :: NWORDS_MAIN = num_instr*nchan_pha  ! Number of words (=bytes) of Main (mode-dependent) section in a frame: 96
  character(len=*), parameter, public :: tunit_main = 'count'
  integer, parameter, public :: ASM_STATUS_FN = 15; ! (First) Frame number; F15W65B1 ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
  integer, parameter :: LEN_READABLE_KEY = 32  ! Char-length of maximum human-readable key name
  integer, parameter :: LEN_T_INVALID_FMT_KEY = 32
  integer, parameter :: LEN_PROC_STATS = 256
  integer, parameter, public :: tfields_asm = 104  !!!!=========== Check!

  character(len=max_fits_char), dimension(n_all_fields) :: &
     tmtypes, tmcomms, tmforms, tmunits

  character(len=max_fits_char), dimension(:, :), allocatable :: tmcolss  ! (i-th-column(1:n_all_fields), i-th-row)
  integer(kind=1),              dimension(:, :), allocatable :: tmcolsi
  integer,                      dimension(:, :), allocatable :: tmcolsj
  real(kind=dp),                dimension(:, :), allocatable :: tmcolsd
  character(len=max_fits_char), dimension(:),    allocatable :: outcolss ! (i-th-row)
  integer(kind=1),              dimension(:),    allocatable :: outcolsi
  integer,                      dimension(:),    allocatable :: outcolsj
  real(kind=dp),                dimension(:),    allocatable :: outcolsd

  integer, private :: i

  DATA tmtypes(1:2) / 'SF_NO2B', 'FRAME_NO' /

  integer, parameter :: tfields_def = 9  ! TFIELDS in the output FITS   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface get_onoff_enadis
    module procedure get_onoff_enadis_from_key, get_onoff_enadis_from_tf
  end interface get_onoff_enadis

  interface get_index
    module procedure get_index_char, get_index_colhead, get_index_form_unit
  end interface get_index

  interface get_element
    module procedure get_element_char, get_element_colhead, get_element_form_unit
  end interface get_element

  interface dump_type
    module procedure dump_asm_telem_row, dump_asm_frfrow, dump_asm_sfrow, dump_form_unit
    module procedure dump_asm_colhead, dump_fits_header
  end interface dump_type
 
  ! Number of the total columns (TTYPEs) of a type(t_asm_colhead)
  interface get_ncols_colheads
    module procedure get_ncols_colheads_char,       get_ncols_colheads_colheads
    module procedure get_ncols_colheads_frameunits, get_ncols_colheads_none
  end interface get_ncols_colheads

  ! Number of the total frames of a Colheads

  ! Ginga Telemetry file word structure
  !
  ! It starts from zero (0): W0-W127
  type t_telem_word_from0 ! "from0" means the word number starts from 0, as opposed to 1.
    integer :: fi     =  3; !  W3 (Frame-Info: SF(2-bits)+FrameNo(6-bits) == Telemetry-Header Counter_A2(Byte08))
    integer :: status = 65; ! W65
    integer :: dp     = 66; ! W66
    integer :: pi_mon = 67; ! W67
    integer :: acs1   = 33; ! W33 (ACS: W33-W35)
    integer :: acs2   = 34; ! W33 (ACS: W33-W35)
    integer :: acs3   = 35; ! W33 (ACS: W33-W35)
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
    integer :: f_multi=0, f_offset, word, bit=-999;  ! f_multi=0 means only 1 location. A negative bit is no bit info.
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
      note='(Status)'); ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
    type(t_loc_fwb) :: MODE_PHA  = t_loc_fwb(f_multi=8,  f_offset=4,  word=TELEM_WORD_FROM0%dp,     bit=4, &
      note='(DP) (TIME/PHA <=> 1/0)'); ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    type(t_loc_fwb) :: MODE_PHA_W56 = t_loc_fwb(f_multi=0, f_offset=56, word=TELEM_WORD_FROM0%dp,     bit=4, &
      note='(DP) (TIME/PHA <=> 1/0), duplicate'); ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    !integer(ip4) :: mode_real_stored = -999;
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
  !  integer :: slew   =  3; ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
  !  integer :: asm_stat = 1; ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
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
    integer(kind=ip4) :: STAT_OBS = -999; ! W65(=Status)
       ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed))
       !   Ref1: Table 5.1.12, pp.209
    integer(kind=ip4) :: DPID_OBS = -999; ! W66(=DP)
       ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
       !   Note: In short, F4 alone should be fine.  In practice, in some SFs,
       !         F4 may be missing.  In this code, such frames should be discarded.
       !   Ref1: Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
       !   Ref2: Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
       ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
       !   Note: The same is found at F56W66B4 (but it is ignored).
       !   Ref2: Table 5.1.12, pp.214 “MODE” => B3(ASM ON/OFF), B4(ASM TIME/PHA)
    integer(kind=ip4) :: pi_mon = -999; ! W67(=PI_MON)
    integer(kind=ip4), dimension(4) :: asm_w48;  ! (W48-W51)   ignored
    integer(kind=ip4), dimension(4) :: asm_w112; ! (W112-W115) ignored
       !   Note: PI-MON (W67 in F8n+1,5) also contains a piece of ASM info, but is ignored
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
    real(kind=dp8), dimension(3) :: eulers, angles_intn;  ! Internal angle x,y,z
    integer(kind=ip4), dimension(3) :: acss;      ! acss = 3 bytes
    integer(kind=ip4), dimension(nwords_main) :: asmdats; ! Main ASM data; Y[1-2]FW[1-3](ch[0-15]) for PHA mode, Y[1-2]FW[1-3][LH](ch[0-7]) for Time mode
  end type asm_telem_row

  ! ASM FRF; row-based, ie., each row constitutes 1 variable, and SF (sabu-frame) based.
  !
  ! NOTE: A byte must have been converted into Integer*4 with a proper filter
  type asm_frfrow
    !------- with SFCHCK() -----------
    integer(kind=ip4), dimension(0:63) :: sync;    ! (0:63), 0=OK, 1=NG
    integer(kind=ip4) :: lostf;   ! Number of frames with SYNG=NG
    integer(kind=ip4) :: sfn;     ! SF Number (by SIRIUS)
    integer(kind=ip4) :: bitrate; ! 0: high, 1: medium, 2: low
      ! NOTE: bit-rate-low(0) or high(1) in Byte12 of Telemetry-Header-16-bits.
      ! NOTE: Name is "bitrate" (with trailing 'e') as opposed to 'BITRAT' in FRFREAD manual.
      ! NOTE: 128(L)/32(M)/4(H) sec/SF (cf. Table 4.1.2, pp.187 in Ginga specification)
      !     :   2(L)/0.5(M)/0.0625(H) sec/Frame
    integer(kind=ip4) :: relstr;  ! 0: real-data, 1: stored data
      ! NOTE: The flag is different in Byte11 of Telemetry-Header-16-bits: (real(1) or stored(2))
    integer(kind=ip4), dimension(7) :: stime; ! Start time of SF in (Y,M,D,h,m,s,ms) in INTEGER

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
    real(dp8), dimension(4) :: mjds ! MJD OF THE ORBIT AND ATTITUDE
    real(dp8), dimension(17,4) :: rbuffs ! RBUFF(J,*)  *=1,NSAMPL ! for backup/debugging
    !---- From here, Contents of RBUFF([1-17],*)
    real(dp8), dimension(3, 4) :: eulers        ! [J=1-3] EURLER ANGLES  (Z-Y-Z)
    real(dp8), dimension(3, 4) :: d_eulers      ! [J=4-6] DOT EURLER ANGLES
    real(dp8), dimension(4) :: height     ! [J=7] HEIGHT
    real(dp8), dimension(2, 4) :: lon_lat       ! [J=8-9] LONGITUDE, LATTITUDE
    real(dp8), dimension(4) :: dist_earth ! [J=10] DISTANCE FROM THE EARTH CENTER                 
    real(dp8), dimension(2, 4) :: coords_earth  ! [J=11-12] ALPHA,DELTA OF THE EARTH CENTER (1950 EQUINOX)
    real(dp8), dimension(4) :: cor        ! [J=13] CUT OFF RIGIDITY
    real(dp8), dimension(2, 4) :: coords_magnet ! [J=14-15] ALPHA,DELTA OF THE MAGNETIC FIELD
    real(dp8), dimension(2, 4) :: coords_sun    ! [J=16-17] ALPHA,DELTA OF THE SUN
    !---- Up To here, Contents of RBUFF 
    real(dp8), dimension(4) :: sunps  ! PRESENCE OF SUNSHINE, 1/0=YES/NO                        
    real(dp8), dimension(4) :: elvys  ! ELEVATION OF YAXIS FROM THE EARTH EDGE                  
    real(dp8), dimension(4) :: eflags ! CONDITION OF THE EARTH OCCULTATION                      
                   ! 0: NOT OCCULTED, 1: OCCULTED BY THE DARK EARTH         
                   ! 2: OCCULTED BY SUN SHONE EARTH                         
    real(dp8) :: nsampl ! NUMBER OF THE ORBIT AND ATTITUDE DATA                  
                       ! NSAMPL=1 FOR BITRATE H,M ,  =4 FOR BITRATE L           
  end type asm_frfrow

  ! Invalid reason type
  type t_invalid_fmt
    character(len=LEN_T_INVALID_FMT_KEY)   :: key;
    character(len=1024) :: desc;
    character(len=1024) :: fmt;
    integer             :: nargs = 0;  ! Number of Integer arguments required with the "fmt"
  end type t_invalid_fmt
  type(t_invalid_fmt), dimension(4), parameter :: INVALID_FMT = [ &
       t_invalid_fmt(key='no_frf', desc='no matching with FRF found' &
                                , fmt='("no matching with FRF found")', nargs=0) &
     , t_invalid_fmt(key='tel64',  desc='less than 64 Telemetry nFrames' &  ! NFRAMES_PER_SF = 64
                                , fmt='("less than 64 Telemetry nFrames=",I2)', nargs=1) &
     , t_invalid_fmt(key='lostf',  desc='positive lostf(FRF)' &
                                , fmt='("positive lostf(FRF)=",I2)', nargs=1) &
     , t_invalid_fmt(key='asmoff', desc='ASM is off' &
                                , fmt='("ASM is off")', nargs=0) &
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
    integer(ip4) :: irowt   = -999; ! i-th ROW of Telemetry that is the first one of this SF
    integer(ip4) :: sf2bits = -999; ! 2-bits SF in 16-byte Telemetry row header
    integer(ip4) :: nframes = -999; ! Number of FRAMES
    integer(ip4) :: irowf   = -999; ! i-th ROW of FRF
    integer(ip4) :: sfn     = -999; ! SF Number in FRF determined by SIRIUS
    integer(ip4) :: lostf   = -999; ! number of LOST Frames (wrong "SYNC")
    integer(ip4) :: mode_asm  = -999; ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
    integer(ip4) :: mode_slew = -999; ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
    integer(ip4) :: mode_PHA  = -999; ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    integer(ip4) :: mode_PHA_W56 = -999; ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    !integer(ip4) :: mode_real_stored = -999;
    integer(ip4) :: stat_asm_b = -999; ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213 ! "_b" for bit information (1 or 0)
    integer(ip4) :: stat_asa_b = -999; ! ON/OFF for ASM-A    F15W65B2
    integer(ip4) :: stat_amc_b = -999; ! ON/OFF for ASM-AMC  F15W65B3
    integer(ip4) :: stat_hv1_b = -999; ! ENA/DIS for ASM-HV1 F15W65B4
    integer(ip4) :: stat_hv2_b = -999; ! ENA/DIS for ASM-HV2 F15W65B5
    integer(ip4) :: stat_rbm_b = -999; ! ENA/DIS for ASM-RBM F15W65B6
    integer(ip4) :: stat_bdr_b = -999; ! ENA/DIS for ASM-BDR F15W65B7
    !character(len=max_fits_char) :: stat_asm = ''; ! ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213
    !character(len=max_fits_char) :: stat_asa = ''; ! ON/OFF for ASM-A    F15W65B2
    !character(len=max_fits_char) :: stat_amc = ''; ! ON/OFF for ASM-AMC  F15W65B3
    !character(len=max_fits_char) :: stat_hv1 = ''; ! ENA/DIS for ASM-HV1 F15W65B4
    !character(len=max_fits_char) :: stat_hv2 = ''; ! ENA/DIS for ASM-HV2 F15W65B5
    !character(len=max_fits_char) :: stat_rbm = ''; ! ENA/DIS for ASM-RBM F15W65B6
    !character(len=max_fits_char) :: stat_bdr = ''; ! ENA/DIS for ASM-BDR F15W65B7
     integer(ip4) :: bitrate  = -999; ! [/s] Telemetry bit rate (F16W66) ! Taken from Telemetry as opposed to FRF
  end type asm_sfrow

!* tmtypes / (TeleMetry-TYPES) Field names derived from Telemetry
!* tmcomms / (TeleMetry-COMMentS) Comments for outputs for Field names that is derived from Telemetry 
!* tmforms / (TeleMetry-FORMS) FORMAT to output (and input)
!* tmunits / (TeleMetry-UNITS) Unit to output
!* tmnframes / (TeleMetry-I-th-FRAMES) Which n-th frame in n+offset (1 may mean all), eg., 16 for 24n+3 (like 3, 27, 51)
!* tmoframes / (TeleMetry-I-th-FRAMES) Which offset frame in n+offset (0 may mean all), eg., 3 for 24n+3 (like 3, 27, 51)
!* tmiiwords / (TeleMetry-I-th-Initial-WORDS) Starting at which word (1..128)
!* tmifwords / (TeleMetry-I-th-Final-WORDS) End at which word (1..128)
!* tmiibits / (TeleMetry-I-th-Initial-BITS) Starting at which bit (1..8) (0 means all)
!* tmifbits / (TeleMetry-I-th-Initial-BITS) End at which bit (1..8) (0 means all)
!* tmcols / (TeleMetry-COLumnS) Type

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
  type(t_form_unit), dimension(11), parameter :: COL_FORM_UNITS = [ &
     ! note: I=Int*2, J=Int*4, D=Real*8
       t_form_unit(key='main',     root='',        form='1I', unit='count', comm='Main ASM data', dim=NWORDS_MAIN) & ! Special case: root should be explicitly specified when used. See function get_colheads()
     , t_form_unit(key='Tstart',   root='Tstart',  form='1D', unit='day', comm='Start datetime in MJD') &
     , t_form_unit(key='Euler',    root='Euler',   form='1D', unit='rad', comm='Euler angle', dim=3) &
     , t_form_unit(key='SFNum',    root='SFNum',   form='1J',  comm='As defined in FRF if defined') &
     , t_form_unit(key='SF2bits',  root='SF2bits', form='1I',  comm='2-bit SF from FI in Telemetry') &
     , t_form_unit(key='Fr6bits',  root='Fr6bits', form='1I',  comm='Frame number from FI in Telemetry') &
     , t_form_unit(key='i_frame',  root='i_frame', form='1I',  comm='i-th Frame in Telemetry') &
    !   ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed))
    !   ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
    !   ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    !   !   Note: The same is found at F56W66B4 (but it is ignored).
     , t_form_unit(key='Status_C', root='Status_C',form='1I', comm='STATUS (W65) in every Frame') &
     , t_form_unit(key='DP_C',     root='DP_C',    form='1I', comm='DP (W66) in every Frame') &
     , t_form_unit(key='ACS_C',    root='ACS_C',   form='3I', comm='ACS (W33-35) in every frame', dim=1) &  ! 3I but dim=1
     , t_form_unit(key='AMS_C',    root='AMS_C',   form='8I', comm='W48-51, W112-115 in every Frame', dim=1) &  ! 8I but dim=1
     !, t_form_unit(key='bitrate', root='bitrate', form='1I', comm='Bitrate') &
     ]
    !integer(kind=ip4) :: STAT_OBS = -999; ! W65(=Status)
    !   ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed))
    !   !   Ref1: Table 5.1.12, pp.209
    !integer(kind=ip4) :: DPID_OBS = -999; ! W66(=DP)
    !   ! F8n+4 W66(=DP) B3:  ASM Mode (ON/OFF <=> 1/0)
    !   !   Note: In short, F4 alone should be fine.  In practice, in some SFs,
    !   !         F4 may be missing.  In this code, such frames should be discarded.
    !   !   Ref1: Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
    !   !   Ref2: Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
    !   ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    !   !   Note: The same is found at F56W66B4 (but it is ignored).
    !   !   Ref2: Table 5.1.12, pp.214 “MODE” => B3(ASM ON/OFF), B4(ASM TIME/PHA)

  ! Each column header info of the ASM Table to output as a FITS (i.e., column-based)
  !
  ! 1. An array of this type represents the info of the output FITS table data.
  ! 2. The index of the array represents the number of the talbe column, eg, TTYPE23
  ! 3. The array is created dynamically, rather than specified as a PARAMETER,
  !    because 'Y6CH15/Y6H07' etc is too awkward to be hard-coded.
  type t_asm_colhead
    character(len=LEN_READABLE_KEY) :: key; ! (internal) key for relating this to others
    character(len=max_fits_char) :: type; ! Column Name (=TTYPEn), set by deriving from %prm%root
    type(t_form_unit)           :: prm;  ! TFORM and TUNIT
  end type t_asm_colhead

  !! ASM Table to output as a FITS (i.e., column-based)
  !!
  !! (x,y) is (internal-number, row-number); e.g., eulerss(2, 9) is Euler_2 in the 9th row, eulerss(2, :) is the column array of Euler_2
  !! For asmdatss, dimension(:, nwords_main)
  !! NOTE: it is faster for Fortran to access eulerss(:, 2) than eulerss(1, :)
  !type asm_table_col
  !  real,            dimension(:, :), allocatable :: tstart;
  !  integer,         dimension(:, :), allocatable :: sfn;  ! i-th SF in the FRF
  !  integer(kind=1), dimension(:, :), allocatable :: sf_2bit;  ! 2-bit info of the SF in Telemetry
  !  integer(kind=1), dimension(:, :), allocatable :: i_frame;  ! i-th frame in the current SF
  !  integer(kind=1), dimension(:, :), allocatable :: fr_6bit; ! Current frame number as recorded in Telemetry
  !  real(dp),        dimension(:, :), allocatable :: eulers; &
  !                                                   !angles_intns;  ! Internal angle x,y,z
  !  integer(kind=1), dimension(:, :), allocatable :: acsss;     ! acss = 3 bytes
  !  integer(kind=1), dimension(:, :), allocatable :: asmdatss;    ! Main ASM data; Y[1-6](ch[0-15]) for PHA mode, Y[1-6][LH][0-7] for Time mode
  !end type asm_table_col

  !! Specification of the ASM Table Types in FITS
  !type t_index_low_high
  !  integer :: low;
  !  integer :: high;
  !end type t_index_low_high

  !! Specification of the ASM Table Types in FITS
  !type asm_index
  !  type(t_index_low_high) :: asmdats  = t_index_low_high(1, nwords_main);  ! main ASM data (96)
  !  type(t_index_low_high) :: tstart   = t_index_low_high(nwords_main+1, nwords_main+1);
  !  type(t_index_low_high) :: sf_siri  = t_index_low_high(nwords_main+2, nwords_main+2);
  !  type(t_index_low_high) :: sf_2bit  = t_index_low_high(nwords_main+2, nwords_main+2);
  !  type(t_index_low_high) :: i_frame  = t_index_low_high(nwords_main+3, nwords_main+3);
  !  type(t_index_low_high) :: fr_6bit = t_index_low_high(nwords_main+4, nwords_main+4);
  !  type(t_index_low_high) :: eulers   = t_index_low_high(nwords_main+5, nwords_main+7); ! 3
  !  type(t_index_low_high) :: angles_intn = t_index_low_high(nwords_main+8, nwords_main+10); ! 3
  !  type(t_index_low_high) :: acss     = t_index_low_high(nwords_main+11, nwords_main+14); ! acss = 3 bytes
  !end type asm_index

  !! Specification of the ASM Table in FITS
  !type asm_table_spec
  !  type(asm_index) :: i = asm_index();
  !  type(form_unit) :: tstart   = form_unit('1D', 'MJD');
  !  type(form_unit) :: sf_siri  = form_unit('1L', '');
  !  type(form_unit) :: sf_2bit  = form_unit('1J', '');
  !  type(form_unit) :: i_frame  = form_unit('1J', '');
  !  type(form_unit) :: fr_6bit  = form_unit('1J', '');
  !  type(form_unit), dimension(3) :: eulers = form_unit(form='1E', unit='deg');
  !  !type(form_unit), dimension(3) :: eulers = form_unit('1E', 'deg');
  !  type(form_unit), dimension(3) :: angles_intn = form_unit('1B', 'deg');  ! Internal angle x,y,z
  !  type(form_unit), dimension(3) :: acss = form_unit('1B', 'binary');      ! acss = 3 bytes
  !  type(form_unit), dimension(nwords_main) :: asms = form_unit('1J', 'count');	! Array
  !end type asm_table_spec

  integer, parameter, private :: LEN_TTYPE = 16  ! maximum character length for TFORM (nb., 14 for 'Y23CH15/Y23H07')

  type fhead1i4
    integer(kind=ip4) :: val  = -999; ! eg., 40256
    character(len=8)  :: name = '' ; ! eg., 'NAXIS2'
    character(len=max_fits_char) :: comment = '' ; ! eg., 'number of rows in table'
    !character(len=1)  :: fmt = 'I';
  end type fhead1i4

  type fhead1r8
    real(kind=dp8)    :: val  = -999.0d0; ! 8.3e05
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
    !! e.g., integer(kind=ip4) :: NAXIS =-999; ! 2 / 2-dimensional binary table
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
    type(fhead1r8) :: TSTART  = fhead1r8(name='TSTART'  , comment='[s] Start time'); ! 8.12345678E05
    type(fhead1r8) :: TEND    = fhead1r8(name='TEND'    , comment='[s] End time');   ! 9.12345678E05
    type(fhead1r8) :: TSTARTMA= fhead1r8(name='TSTARTMA', comment='[s] Start time of ASM Mode');     ! 8.12345678E05
    type(fhead1r8) :: TENDMA  = fhead1r8(name='TENDMA'  , comment='[s] End time of ASM Mode');       ! 9.12345678E05
    type(fhead1r8) :: TSTARTMS= fhead1r8(name='TSTARTMS', comment='[s] Start time of Slew360 Mode'); ! 8.12345678E05
    type(fhead1r8) :: TENDMS  = fhead1r8(name='TENDMS'  , comment='[s] End time of Slew360 Mode');   ! 9.12345678E05
    type(fhead1i4) :: SSTARTMS= fhead1i4(name='SSTARTMS', comment='Start SF of Slew360 Mode'); ! 12345
    type(fhead1i4) :: SENDMS  = fhead1i4(name='SENDMS'  , comment='End SF of Slew360 Mode');   ! 66666
    type(fhead1tf) :: EXISTDAT= fhead1tf(val=.true., name='EXISTDAT', comment='True if the file contains data'); ! T
    type(fhead1r8) :: EULER_S1= fhead1r8(name='EULER_S1', comment='[deg] Euler 1 at the start'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_S2= fhead1r8(name='EULER_S2', comment='[deg] Euler 2 at the start'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_S3= fhead1r8(name='EULER_S3', comment='[deg] Euler 3 at the start'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E1= fhead1r8(name='EULER_E1', comment='[deg] Euler 1 at the end'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E2= fhead1r8(name='EULER_E2', comment='[deg] Euler 2 at the end'); ! -7.12345678E-2
    type(fhead1r8) :: EULER_E3= fhead1r8(name='EULER_E3', comment='[deg] Euler 3 at the end'); ! -7.12345678E-2
    type(fhead1i4) :: MODE__OBS=fhead1i4(name='MODE-OBS', comment='F8N+4 W66');
    type(fhead1i4) :: STAT__OBS=fhead1i4(name='STAT-OBS', comment='F32N+15 W65');
    type(fhead1i4) :: DPID__OBS=fhead1i4(name='DPID-OBS', comment='F15N+16 W66');
    type(fhead1i4) :: MODE__B3 =fhead1i4(name='MODE-B3' , comment='1/0 for ASM Mode ''ON/OFF'''); !   1
    type(fhead1i4) :: MODE__B4 =fhead1i4(name='MODE-B4' , comment='1/0 for ASM-Time/PHA modes');  !   1
    type(fhead1i4) :: MODE__F56=fhead1i4(name='MODE-F56', comment='1/0 for ASM-Time/PHA modes in F56/W66 Bit 4'); ! 1
                                  ! COMMENT If negative, the frame does not exist. => the frame is discarded
    type(fhead1ch) :: STAT__ASM=fhead1ch(name='STAT-ASM', comment='''ON'' or ''OFF''');  ! 'ON' 
    type(fhead1ch) :: STAT__ASA=fhead1ch(name='STAT-ASA', comment='''ON'' or ''OFF'' for ASMA'); ! 'ON'
    type(fhead1ch) :: STAT__AMC=fhead1ch(name='STAT-AMC', comment='''ON'' or ''OFF''');  ! 'OFF'
    type(fhead1ch) :: STAT__HV1=fhead1ch(name='STAT-HV1', comment='''ENA'' or ''DIS'''); ! 'ENA'
    type(fhead1ch) :: STAT__HV2=fhead1ch(name='STAT-HV2', comment='''ENA'' or ''DIS'''); ! 'ENA'
    type(fhead1ch) :: STAT__RBM=fhead1ch(name='STAT-RBM', comment='''ENA'' or ''DIS'''); ! 'ENA'
    type(fhead1ch) :: STAT__BDR=fhead1ch(name='STAT-BDR', comment='''ENA'' or ''DIS'''); ! 'DIS'
   !type(fhead1i4) :: BITRATE0= fhead1i4(name='BITRATE0', comment='[/s] Initial telemetry bit rate');   ! 32
    type(fhead1i4) :: SFRAMES = fhead1i4(name='SFRAMES',  comment='Number of SFs in the file'); ! 10000
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

    iret = -999
    if (.not. is_silent) write(stderr,'(A)') 'WARNING: (get_index_char) Index is not found for key="'//trim(key)//'"'
  end function get_index_char

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
    iret = -999
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

    iret = -999
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
    integer :: i

    retobj = ary(get_index(key, ary))  ! If not found, the result is uncertain.
  end function get_element_form_unit

  !-----------------------------------------
  
  ! Returns the object type(t_reason_invalid)
  !
  ! For the key, see the member 'key' of the parameter INVALID_FMT
  ! e.g., (no_frf|tel64|lostf|asmoff)
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

  ! Returns frame-row-index of type(asm_telem_row) that has the specified frame number
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

    retrow = -999
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

    if (     trim(name) == 'fi') then
      iret = TELEM_WORD_FROM0%fi
    else if (trim(name) == 'dp') then
      iret = TELEM_WORD_FROM0%dp
    else if (trim(name) == 'status') then
      iret = TELEM_WORD_FROM0%status
    else if (trim(name) == 'pi_mon') then
      iret = TELEM_WORD_FROM0%pi_mon
    else
      iret = -999
    end if

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
    iword = -999
    ibit  = -999
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
         row%sf_2bit, row%fr_6bit &
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
    write(*,'(" irowf(irow_FRF)=",I6,", sfn(SF-No)=",I6,", lostf=",I4)') row%irowf, row%sfn, row%lostf
    write(*,'(" mode_asm(1:ON)=",I4,", slew(1:ON)=",I4,", PHA(1:TIME)=",I4)') &
       row%mode_asm, row%mode_slew, row%mode_PHA
    write(*,'(" stat_asm(ON(1)/OFF)=''",A,"'', stat_hv1(ENA(1)/DIS)=''",A,"'', stat_rbm(ENA/DIS)=''",A,"''")') &
         trim(get_onoff_enadis(row%stat_asm_b, 'asm')), trim(get_onoff_enadis(row%stat_hv1_b, 'hv1')) &
       , trim(get_onoff_enadis(row%stat_rbm_b, 'rbm'))

    ! integer(ip4) :: bitrate  = -999; ! [/s] Telemetry bit rate (F16W66) ! Taken from Telemetry as opposed to FRF
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
    write(*,'(" TELESCOP=''",A,"'', INSTRUME=''",A,"''")') trim(fhead%TELESCOP%val), trim(fhead%INSTRUME%val)
    write(*,'(" FILENAME= ",A," / ",A)') trim(fhead%FILENAME%val), trim(fhead%FILENAME%comment)
    write(*,'(" ",A,"=''",A,"'' / ",A)') &
       trim(fhead%DATE__OBS%name), trim(fhead%DATE__OBS%val), trim(fhead%DATE__OBS%comment)
    write(*,'("-------------------------------")')
  end subroutine dump_fits_header


  ! Table5.5.5-6 (pp.233-234)
  function get_asmmain_row(acard) result(arret)
    implicit none
    integer(kind=1), dimension(NBYTESPERCARD), intent(in) :: acard ! == telems(1:128, i) ! Name: "a card"
    integer, dimension(nwords_main) :: arret

    integer :: idet, ich, i_tele, i_out

    arret = 0
    do idet=1, num_instr  ! =6
      do ich=1, nchan_time  ! =8
        !i_tele = (idet-1)*2 + (ich-1)*16 + 5
        !i_out = (idet-1)*nchan_pha + ich
        !arret(i_fr64) = telems(i_tele, itotrow)
        !    
        !i_tele = (idet-1)*2 + (ich-1)*16 + 6
        !i_out = (idet-1)*nchan_pha + ich + 8
        !arret(i_fr64) = telems(i_tele, itotrow)
      end do
    end do
  end function get_asmmain_row

  ! Return the TTYPE string of the Main ASM data part for the given index [1:96] (eg, TTYPE5).
  !
  ! Examples:
  !
  !   TFORM1= 'Y11CH00/Y11L00',
  !   TFORM17='Y21CH00/Y21L00',
  !   TFORM32='Y21CH15/Y21H07',
  !   TFORM33='Y12CH00/Y12L00',
  !   TFORM96='Y23CH15/Y23H07',
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
    ifw    =     (index-1)/(nwords_main/3)   + 1 ! FW1--FW3
    ifwmod = mod( index-1,  nwords_main/3)   + 1 ! (1..32) !!!!!!!!!!! ifwmod = mod((index-1), nwords_main/3)
    iy1    =    (ifwmod-1)/(nwords_main/3/2) + 1 ! Y1--Y2
    ichp   = mod(ifwmod-1,  nwords_main/3/2)     ! CH0--CH15 for PHA-mode (0..15)
    low_high = 'H'
!print *,'DEBUG:642:bef_lowh'
    if (ichp/(nchan_pha/2) == 0) low_high = 'L'  ! else Default 'H'
    icht   = mod(ichp,  nchan_pha/2)             ! CH0--CH07 for TIME-mode (0..7)

    if (.true.) then  ! set it .false. when memory trouble is rampant...
      write(ret, '("Y", I1, I1, "CH", I0.2, "/Y", I1, I1, A1, I0.2)') &
         iy1, ifw, ichp, iy1, ifw, low_high, icht
      return
    end if

    ! ------------- below: not executed in default (unless the above .true. is reset to .false.)
    !               This can be useful when memory trouble is rampant.
    print *,'DEBUG:642-1, iy1=', iy1
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
    print *,'DEBUG:642-2, ifw=',ifw
    select case(ifw)
    case (1,2,3)
      cifw=char(ifw+48)
    case default
      print *,'ifw is NOT 1 or 2 or 3, but=',ifw
      call EXIT(1)
    end select
        !write(cifw, '(I1)') ifw
    print *,'DEBUG:642-3'
    print *,'DEBUG:642-3-cifw=',cifw,' ichp=',ichp
    cichp = char(ichp/10+48)
    cichp(2:2) = char(mod(ichp,10)+48)
        !write(cichp, '(I0.2)') ichp
    print *,'DEBUG:643:aft_lowh, cichp=',cichp
    print *,'DEBUG:643-0:icht=',icht
    cicht = '00'
    cicht(2:2) = char(icht+48)
    print *,'DEBUG:643-1, icht=', icht, ' cicht=',cicht
        !write(cicht, '(I0.2)') icht
    print *,'DEBUG:644:bef_wri,iy1=',iy1,' ifw=',ifw,' ichp=',ichp
    ret = 'Y'//ciy1//cifw//'CH'//cichp//'/Y'//ciy1//low_high//cicht
    print *,'DEBUG:644-1:bef_wri,ret=',ret
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

  !! Get ttype character Array
  !subroutine get_ttypes(tret)
  !  character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret
  !  integer :: i, k, ini = 1

  !  !tret(:) = ''
  !  !tret(ini) = 'Tstart'

  !  !do i=ini+1, ini+3
  !  !  write(tret(i), '("Euler", i1)') i
  !  !  write(tret(i+3), '("F16W", i1)') (i-1)*2+1 ! Internal angle x,y,z
  !  !end do

  !  !do i=ini+3*2+1, tfields_asm ! to the last one
  !  !  tret(i) = trim(get_ttype(i-6-ini))
  !  !end do
  !end subroutine get_ttypes

  !! Get tform character Array
  !subroutine get_tforms(tret)
  !  character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret

  !  tret(:) = ''
  !  tret(1:7) = 'E15.8E2'  ! TSTART, Euler x 2
  !  tret(8:) = 'I1'
  !end subroutine get_tforms

  !! Get tunit character Array
  !subroutine get_tunits(tret)
  !  character(len=max_fits_char), dimension(tfields_asm), intent(out) :: tret

  !  tret(:) = ''
  !  tret(1) = 's'      ! TSTART
  !  tret(2:7) = 'deg'  ! Euler x 2
  !  tret(8:) = 'count'
  !end subroutine get_tunits

  !-----------------------------------------
  ! interface get_ncols_colheads
  !   : to calculate the summed number of (FITS) columns, aka TTYPEs
  !-----------------------------------------

  ! Number of the total columns (TTYPEs) of a Colheads corresponding to the given Character array
  function get_ncols_colheads_char(ckeys) result(nframes)
    character(len=*), dimension(:), intent(in) :: ckeys
    integer :: nframes ! return
    integer :: i
    type(t_form_unit) :: tmp_fu

    nframes = 0
    do i=1, size(ckeys)
      tmp_fu = get_element(trim(ckeys(i)), COL_FORM_UNITS)
      nframes = nframes + tmp_fu%dim
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
  ! If member "dimsion" in COL_FORM_UNITS is greater than 1, this sets multiple TTYPES
  ! for the "dimsion" number.  For example, key=Euler has dimsion=3; thus
  ! EULER1, EULER2, EULER3 are set.
  !
  ! increment is the number of indices (i.e., dimensions) this routine has filled.
  subroutine set_colheads_single(index_start, key, colheads, index_last)
    integer, intent(in) :: index_start ! colheads(index_start) and onwards will be set in this routine.
    character(len=*), intent(in) :: key
    type(t_asm_colhead), dimension(:), intent(inout) :: colheads
    integer, intent(out) :: index_last ! up to colheads(index_last) is set after this routine.

    type(t_form_unit) :: colprm
    character(len=MAX_LEN_FKEY) :: sk
    character(len=max_fits_char) :: root, ttype ! TTYPE (for temporary use)
    integer :: idim

    colprm = get_element(key, COL_FORM_UNITS)

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
    !character(len=MAX_LEN_FKEY), dimension(9), parameter :: Colhead_keys = [ &
    !     'main    '&
    !   , 'Tstart  '&
    !   , 'Euler   '&
    !   , 'SFNum   '&
    !   , 'SF2bits '&
    !   , 'Fr6bits '&
    !   , 'i_frame '&
    !   , 'Status_C'&
    !   , 'DP_C    '&
    !   !, 'ACS_C   '&
    !   !, 'AMS_C   '&
    !   !, 'bitrate ' &
    !   ]
    integer, dimension(:), allocatable :: dims  ! Array of t_form_unit%dim corresponding to ckeys
    character(len=max_fits_char) :: ttype ! TTYPE (for temporary use)
    character(len=LEN_READABLE_KEY) :: sk
    integer :: i, irow, ikey, ittype, ilast, nsiz, increment
    type(t_form_unit) :: tmp_fu
!character(len=LEN_TTYPE) :: tmp_cha  ! for DEBUG

    if (present(ckeys)) then
      colhead_keys = ckeys
      nsiz = get_ncols_colheads(colhead_keys)
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
        ittype = ilast
if (ittype > nsiz) call err_exit_play_safe()
      end select
    end do

!!print *,'DEBUG:523:here023,size=',size(colheads)
!    ! First 96
!    do ikey=1, NWORDS_MAIN
!      sk = 'main'
!!print *,'DEBUG:524:ikey=',ikey
!!tmp_cha = get_ttype_main(ikey)
!!print *,'DEBUG:52X:tmp_cha=',tmp_cha
!!print *,'DEBUG:525:ttb'
!!tmp_fu = get_element(sk, COL_FORM_UNITS)
!!print *,'DEBUG:526:ok'
!!print *,'DEBUG:527:tmp_fu',trim(tmp_fu%key)
!      colheads(ikey) = t_asm_colhead(key=sk, type=trim(get_ttype_main(ikey)), prm=get_element(sk, COL_FORM_UNITS))
!    end do
!
!    increment = 0
!    irow=NWORDS_MAIN+1
!    do i=2, size(Colhead_keys)
!      irow = irow + increment
!      call set_colheads_single(irow, Colhead_keys(i), colheads, increment)
!    end do
!!!print *,'DEBUG:533:here033'
!!    call set_colheads_single(NWORDS_MAIN+1, 'Tstart',   colheads)
!!!print *,'DEBUG:533:here034'
!!    call set_colheads_single(NWORDS_MAIN+2, 'SFNum',    colheads)
!!    call set_colheads_single(NWORDS_MAIN+3, 'SF2bits',  colheads)
!!    call set_colheads_single(NWORDS_MAIN+4, 'Fr6bits',  colheads)
!!    call set_colheads_single(NWORDS_MAIN+5, 'i_frame',  colheads)
!!    call set_colheads_single(NWORDS_MAIN+6, 'Status_C', colheads)
!!    call set_colheads_single(NWORDS_MAIN+7, 'DP_C',     colheads)
!!    !call set_colheads_single(NWORDS_MAIN+8, 'ACS_C',    colheads)
!!    !call set_colheads_single(NWORDS_MAIN+9, 'AMS_C',    colheads)
!    !call set_colheads_single(NWORDS_MAIN+10, 'bitrate',  colheads)
!!print *,'DEBUG:633:here133'
!!call dump_asm_colhead(colheads(1))
!!call dump_asm_colhead(colheads(8))
!!print *,'DEBUG:634:here134'

    if (allocated(colhead_keys)) deallocate(colhead_keys)
  end function get_colheads


  !! Convert an Array of asm_telem_row into asm_out_col
  !subroutine convert_row2col(rows, frfrows, colout)
  !  type(asm_telem_row), dimension(:), allocatable, intent(in) :: trows
  !  type(asm_frfrow), dimension(:), allocatable, intent(in) :: frfrows
  !  type(asm_table_col),                            intent(out) :: colout

  !  integer :: i, nrows

  !  nrows = size(trows)

  !  allocate( colout%tstarts( 1, nrows) )
  !  allocate( colout%sfs(     1, nrows) )
  !  allocate( colout%sf_2bits(1, nrows) )
  !  allocate( colout%i_frames(1, nrows) )
  !  allocate( colout%fr_6bits(1, nrows) )
  !  allocate( colout%eulerss( 3, nrows) )
  !  allocate( colout%acsss(   3, nrows) )
  !  allocate( colout%asmdatss(num_instr*nchan_pha, nrows) )

  !  colout%asmdatss( i, :) = rows(i)%asmdats
  !  colout%tstarts(  i, 1) = rows(i)%tstart
  !  !colout%sfs(     i, 1) = rows(i)%sf
  !  !colout%sf_2bits(i, 1) = rows(i)%sf_2bit
  !  !colout%i_frames(i, 1) = rows(i)%
  !  colout%fr_6bits( i, 1) = rows(i)%fr_6bit
  !  !colout%eulerss( i, :) = rows(i)%eulers
  !  !colout%acsss(   i, :) = rows(i)%acss

  !  !do i = 1, nrows
  !  !  colout%asmdatss(   i, :) = rows(i)%asmdats
  !  !  colout%tstarts(  i, 1) = rows(i)%tstart
  !  !  !colout%sfs(      i, 1) = rows(i)%sf
  !  !  !colout%sf_2bits( i, 1) = rows(i)%sf_2bit
  !  !  !colout%i_frames( i, 1) = rows(i)%
  !  !  colout%fr_6bits(i, 1) = rows(i)%fr_6bit
  !  !  colout%eulerss(  i, :) = rows(i)%eulers
  !  !  !colout%acsss(    i, :) = rows(i)%acss
  !  !end do
  !end subroutine convert_row2col

  ! Get (ttype, tform, tunit) character Array
      !  character(len=64), dimension(:), parameter :: ttype
      !, tform, tunit, extname, varidat, status)

  ! Returns String of process statistics
  !
  ! TODO: Discarded due to ASM at the beginning, end, middle
  function calc_proc_stats(trows, sfrows) result(strs_stats)
    integer, parameter :: n_b4bd = 4, SIZE_FMT = size(INVALID_FMT)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), intent(in) :: sfrows
    character(len=LEN_PROC_STATS), dimension(n_b4bd+SIZE_FMT+2) :: strs_stats
    character(len=LEN_PROC_STATS) :: s

    logical, dimension(size(trows)) :: mask_tr
    integer :: n_fr_output, n_tot, n_discarded
    integer :: i, j, n, ifmt

    n_fr_output = sum(sfrows%nframes, sfrows%is_valid)
    write(s,'("Output/Discarded/Telemetry numbers of frames: ", I6, " /", I6, " /", I6)') &
       n_fr_output, size(trows)-n_fr_output, size(trows)
    strs_stats(n_b4bd-3) = s  ! Index=1
    n = count(sfrows%is_valid)
    n_discarded = size(sfrows)-n
    write(s,'("Output/Discarded/Telemetry numbers of SFs: ", I5, " /", I5, " /", I5)') &
       n, n_discarded, size(sfrows)
    strs_stats(n_b4bd-2) = s  ! Index=2
    strs_stats(n_b4bd-1) = '  (Note: SF number of Telemetry may not be strictly accurate.)' ! Index=3
    strs_stats(n_b4bd)   = '  Breakdown: discarded due to:' ! Index=4
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
    write(s,'("Telemetry SFs without FRF counterparts (undefined sfn): ",I5," (for sanity-check)")') &
       count(sfrows%sfn < 0)
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
      iret = -999
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

    retstr = get_onoff_enadis(ival, is_onoff=.false.)
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

end module asm_fits_common

