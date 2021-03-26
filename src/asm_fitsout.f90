! Routines to output a FITS file
!
! Matching Telemetry and FRF, and outputting the result to a file.
!
module asm_fitsout
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  use err_exit
  use fort_util
  use asm_consts
  use asm_fits_common
  implicit none 
contains

  !------------------------------------------------------------
  ! Gets the initialized type(asm_sfrow) purely based on an asm_telem_row object
  !
  ! Basically, type(asm_sfrow) is an array with the index being a row
  ! of the SF, pointing to the original Telemetry initial row and number
  ! of the frames in the SF.  For example, 
  !   sfrow(2): %irowt=65, %nframes=64
  !
  ! SF is judged on the basis of its sequence (and also the 2-bit SF info
  ! in the 16-byte telemetry header, though it does not mean much, because
  ! consective two SFs may have the same 2-bit SF number!  In short,
  !
  ! (1) If the next Frame number is smaller than the previous one,
  !     the next one belongs to a new SF.
  ! (2) If the next Frame number is larger than the previous one,
  !     the next one belongs to the same SF.
  !
  ! In the worst case senario, the following is possible:
  !
  !  SF5 (2bit=3): Frame28
  !  SF6 (2bit=3): Frame29
  !
  ! Only the way to classify them is on the basis of their time stamps.
  ! However, the interval depends on the bitrate, and the bitrate
  ! can only be correctly derived once the SF has been accurately defined.
  ! In this case, our goal is to DETERMINE which frames belong to a common SF.
  ! Therefore, it is tricky to guess on the basis of bitrate.
  !
  ! In this case, we do not do that. The Frames 28&29 was regarded as
  ! belonging to the same SF (SF5). However, they should be discarded later
  ! anyway because their "lostf" is non-zero (all the frames from Frame29
  ! onwards are missing in SF5).
  !
  !------------------------------------------------------------
  function get_ams_sfrow_init(trows) result(rets)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), allocatable :: rets
    type(asm_sfrow), dimension(size(trows)) :: rows2init
    ! Comment:
    ! The maximum possible number of rows for the returned array is that of the given trows.
    ! The returned array from this function eliminate undefined rows (newly allocated).

    integer :: i, irowt, iret, siz_trows ! I-th-ROW-Telemetry, I-th-row-RETurned-array
    integer :: sf2bit_prev, fr6bit_prev, status
    integer :: nframes ! Number of FRAMES in the current SF.

    if (size(trows) == 0) then
      allocate(rets(0), STAT=status)
      return
    end if

    iret = 0
    rows2init(1) = asm_sfrow(irowt=1, sf2bits=trows(1)%sf_2bit)

    siz_trows = size(trows)
    nframes = 0
    fr6bit_prev = -99
    sf2bit_prev = -99
    do irowt=1, siz_trows
      if ((trows(irowt)%sf_2bit == sf2bit_prev) .and. (trows(irowt)%fr_6bit > fr6bit_prev)) then
        ! Note: if the Frame-numbers are the same, the latter one belongs to a new SF (as we interpret).
        nframes = nframes + 1
        fr6bit_prev = trows(irowt)%fr_6bit
        cycle
      end if

      ! Now this frame belongs to a new SF.
      if (iret > 0) rows2init(iret)%nframes = nframes  ! Set nFrames for the previous one.
      iret = iret + 1
      nframes = 1
      sf2bit_prev = trows(irowt)%sf_2bit
      fr6bit_prev = trows(irowt)%fr_6bit
      rows2init(iret) = asm_sfrow(irowt=irowt, sf2bits=trows(irowt)%sf_2bit)
    end do

    if (iret > 0) rows2init(iret)%nframes = nframes  ! Set nFrames for the last one

    allocate(rets(iret), STAT=status)

    rets(:) = rows2init(1:iret)
    rets(:)%sfntelem = (/ (i, i=1, iret) /)

    if ((mod(siz_trows, NFRAMES_PER_SF) == 0) .and. (iret == siz_trows/NFRAMES_PER_SF)) then
      ! do nothing
    else
      write(stderr,'(A)') 'WARNING: Some Frames are missing from the telemetry.'
    end if
  end function get_ams_sfrow_init

  !------------------------------------------------------------
  ! Returns TRUE if the MJD difference between FRF and Telemetry is small enough.
  !------------------------------------------------------------
  logical function is_frf_mjd_similar(exp, act, irowf, msgt, match, factor)
    real(dp8), intent(in) :: exp, act
    integer(ip4), intent(in) :: irowf ! Row-number of FRF
    character(len=*), intent(in) :: msgt  ! Word to express Telemetry info like 'Telemetry(row=40224, Frame=12)'
    logical,   intent(in), optional :: match  ! if .false.(Default), NOT print message "matches it" to STDERR
    real(dp8), intent(in), optional :: factor ! if 1 (Default), the allowance is 1[msec]; if 1000, it is 1[sec]

    integer,   parameter :: DAY2MSEC = 86400000 ! a scaling factor
    real(dp8), parameter :: ALLOWANCE = 1.0d0/DAY2MSEC ! 1 [msec] ~ 1e-8 [day]
    real(dp8) :: fac
    real(dp8) :: diff
    logical   :: pr_match
    character(len=255) :: env_ginga_chatter
    integer :: chatter = -99  ! read only once

    fac = 1.0d0
    if (present(factor)) fac = factor
    pr_match = .false.
    if (present(match)) pr_match = match

    is_frf_mjd_similar = .false.

    if (chatter < 0) then
      call GET_ENVIRONMENT_VARIABLE('GINGA_CHATTER', env_ginga_chatter)
      read(env_ginga_chatter, '(I2)') chatter
      if (chatter == 0) chatter = 10
    end if

    diff = abs(exp - act)
    if (diff < ALLOWANCE*fac) then
      if (4 < chatter) then
        write(stderr, '("WARNING: FRF(row=",I5,") differs from ", A &
           ," by ",ES10.3," [msec]")', advance='no') &
           irowf, trim(msgt), diff*DAY2MSEC 
        if (pr_match) then
          write(stderr, '(", but matches anyway.")')
        else
          write(stderr, '(".")')
        end if
      end if

      is_frf_mjd_similar = .true.
      return
    end if
  end function is_frf_mjd_similar

  !------------------------------------------------------------
  ! Gets the row-number of the FRF that matches the given Telemetry row
  !
  ! If not found, a negative value is returned.
  !
  ! Only the first element of frows%mjds is considered.
  ! (n.b., the elements 2-4 are valid only for Low-Bitrate, which is very rare.)
  !------------------------------------------------------------
  integer(ip4) function get_matched_frfrow(irowt, trows, frows) result(retrow)
    integer(ip4), intent(in) :: irowt
    type(asm_telem_row), dimension(:), intent(in) :: trows
    real(dp8) :: mjd
    type(asm_frfrow), dimension(:), intent(in) :: frows

    real(dp8) :: diff
    integer :: irow
    character(len=256) :: msg
    logical :: tf

    mjd = trows(irowt)%mjd
    retrow = -99
    do irow=1, size(frows)
      if (mjd == frows(irow)%mjds(1)) then
        retrow = irow
        return
      else if (mjd < frows(irow)%mjds(1)) then
        write(msg, '("Telemetry(row=",I5,", Frame(0-63)=", I0.2, ")")') irowt, trows(irow)%fr_6bit;
        ! MJD in the current FRF-Row is larger than the MJD compared with.
        ! If the current one is just infinitesimally different (due to
        ! the floating-point calculation issue), match it.
        if (is_frf_mjd_similar(mjd, frows(irow)%mjds(1), irow, msg, match=.true.)) then
          retrow = irow
          return
        end if

        if (irow < 2) return

        ! Now, check out the previous index "irow - 1". Whether it matches or not, it returns.

        ! MJD in the last FRF-Row may be extremely close to the MJD compared with.
        ! If that is the case, match it.
        if (is_frf_mjd_similar(mjd, frows(irow-1)%mjds(1), irow-1, msg, match=.true.)) then
          retrow = irow - 1
          return
        end if

        ! See if the last one is close by 0.001 sec (=1 ms) to the Telemetry one. (cf. 62.5 ms/frame for H-bit)
        if (is_frf_mjd_similar(mjd, frows(irow-1)%mjds(1), irow-1, msg, match=.false., factor=1.d0)) then
          write(stderr, '("WARNING: This time difference is too large for a floating error!")')  ! Extra warning (in addition to that in is_frf_mjd_similar()).
          return
        end if

        ! Finally,
        ! see if the current one is close by 0.001 sec (=1 ms) to the Telemetry one. (62.5 ms/frame for H-bit)
        ! Note this is skipped when irow==1
        if (is_frf_mjd_similar(mjd, frows(irow)%mjds(1), irow, msg, match=.false., factor=1.d0)) then
          write(stderr, '("WARNING: This time difference is too large for a floating error!")')  ! Extra warning (in addition to that in is_frf_mjd_similar()).
          return
        end if

        return
      end if
    end do
  end function get_matched_frfrow

  !------------------------------------------------------------
  ! Gets Array of type(asm_sfrow), matching Telemetry with FRF
  !
  ! The number of the rows (aka SFs) in the returned array is based on
  ! that of the SFs in Telemetry (NOT on that of the FRF).
  !
  ! NOTE: mjds obtained from FRF are for the time in the middle of the SF.
  !   In other words, the start time of the 32nd frame (64 frames/SF).
  !
  !------------------------------------------------------------
  function get_asm_sfrow(trows, frows) result(retrows)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_frfrow), dimension(:), intent(in) :: frows
    type(asm_sfrow), dimension(:), allocatable :: retrows
    ! Comment:
    ! The returned rets may have more rows than the rows of SFs taken from the FRF
    ! because some SFs that exist in the Telemetry may not exist in the FRF
    ! whereas the info of "SFs" in the returned array is based on those
    ! in the telemetry.  Therefore its size is allocated.

    integer :: irowr, irowt, irowf, irowf_prev ! I-th-ROW-(Retrow|Telemetry|Frf)
    character(len=1024) :: msg

    retrows = get_ams_sfrow_init(trows) ! allocated and initialized

    do irowr=1, size(retrows)
      ! Full 64 frames do not exist in Telemetry (This SHOULD have been already detected with "lostf")
      if (retrows(irowr)%nframes < NFRAMES_PER_SF) then ! defined in asm_fits_common
        retrows(irowr)%is_valid = .false.
        retrows(irowr)%reason_invalid = get_reason_invalid('tel64', retrows(irowr)%nframes) ! defined in asm_fits_common
        !write(retrows(irowr)%reason_invalid, '("nFrames in Telemetry ", I2, " < ", I2)') retrows(irowr)%nframes, NFRAMES_PER_SF
        cycle
      else if (retrows(irowr)%nframes > NFRAMES_PER_SF) then ! should never happen.
        write(msg, '("nFrames in Telemetry (row=", I6, "): " I2, " > ", I2, ".")') &
           irowr, retrows(irowr)%nframes, NFRAMES_PER_SF
        call err_exit_play_safe(msg)
        ! stop 1      ! redundant. Anyway not yet standard? (works for gfortran?)
      end if

      ! Gets the 32nd frame (=64/2, where 64 frames/SF), as its time is the mjds in FRF.
      irowt = get_telem_row_index_from_fr(NFRAMES_PER_SF/2, trows, retrows(irowr)%irowt, retrows(irowr)%nframes) ! defined in asm_fits_common

      irowf = get_matched_frfrow(irowt, trows, frows)
      if (irowf < 0) then  ! No matching with FRF is found
        !!! Comment: FRF may (or actually does) not match for the telemetry rows for the ASM-mode data
        !retrows(irowr)%is_valid = .false.
        !retrows(irowr)%reason_invalid = get_reason_invalid('no_frf') ! defined in asm_fits_common
        retrows(irowr)%with_frf = .false. ! Same as Default
        retrows(irowr)%irowf = -1
        retrows(irowr)%frf%sfn   = -1
        retrows(irowr)%frf%lostf = -1
        cycle
      end if

      retrows(irowr)%with_frf = .true.
      retrows(irowr)%irowf = irowf
      retrows(irowr)%frf = frows(irowf)
      !retrows(irowr)%sfn   = frows(irowf)%sfn
      !retrows(irowr)%lostf = frows(irowf)%lostf

      if (retrows(irowr)%frf%lostf > 0) then  ! Some frames are lost due to SYNC
        retrows(irowr)%is_valid = .false.
        retrows(irowr)%reason_invalid = get_reason_invalid('lostf', retrows(irowr)%frf%lostf) ! defined in asm_fits_common
        !write(retrows(irowr)%reason_invalid, '("lostf=", I2, " (FRF) > 0")') retrows(irowr)%lostf
        cycle
      end if
    end do
  end function get_asm_sfrow


  !------------------------------------------------------------
  ! Update asm_sfrow, checking various modes.
  !
  ! invalid flag in the sfrow may be set.
  !------------------------------------------------------------
  subroutine update_asm_sfrow_modes(trows, sfrows, skip_validate)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), allocatable, intent(inout) :: sfrows
    logical, intent(in), optional :: skip_validate  ! if specified and .true., no validation.

    integer :: irows ! I-th-ROW-(SFrow)
    integer(kind=1) :: ibyte
    logical :: does_validate

    does_validate = .true.
    if (present(skip_validate)) then
      if (skip_validate) does_validate = .false.
    end if

    do irows=1, size(sfrows)
      call update_asm_sfrow_mode_one(trows, sfrows(irows))
      if (does_validate) call validate_asm_sfrow_mode_one(trows, sfrows(irows))
    end do
  end subroutine update_asm_sfrow_modes

  !------------------------------------------------------------
  ! Update asm_sfrow, checking various modes.
  !------------------------------------------------------------
  subroutine update_asm_sfrow_mode_one(trows, sfrow)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), intent(inout) :: sfrow

    integer :: iget

    !!! This line is commented out; it continues even when there is no matching SF in FRF.
    ! if (.not. sfrow%is_valid) return  ! SF does not exist in FRF

    ! MODE_ASM
    sfrow%mode_asm  = get_val_frb(TELEM_LOC%MODE_ASM,  trows, sfrow) ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
    
    ! MODE_SLEW
    sfrow%mode_slew = get_val_frb(TELEM_LOC%MODE_SLEW, trows, sfrow) ! F32n+10 W65(=Status) B4:  Slew360 Mode (ON/OFF <=> 1/0) ! Ref: Unofficial final specification
    
    ! MODE_SLEW_MINUS
    sfrow%mode_slem = get_val_frb(TELEM_LOC%MODE_SLEW_MINUS, trows, sfrow) ! F32n+10 W65(=Status) B5:  SlewMinus Mode (ON/OFF <=> 1/0) ! Ref: Unofficial final specification
    
    ! MODE_SLEW_PLUS
    sfrow%mode_slep = get_val_frb(TELEM_LOC%MODE_SLEW_PLUS,  trows, sfrow) ! F32n+10 W65(=Status) B6:  SlewPlus Mode (ON/OFF <=> 1/0) ! Ref: Unofficial final specification
    
    ! MODE_PHA
    sfrow%mode_PHA  = get_val_frb(TELEM_LOC%MODE_PHA,  trows, sfrow) ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    
    ! MODE_PHA_W56
    sfrow%mode_PHA_W56  = get_val_frb(TELEM_LOC%MODE_PHA_W56, trows, sfrow) ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    
    ! STAT_ASM
    sfrow%stat_asm_b = get_val_frb(TELEM_LOC%STAT_ASM,  trows, sfrow) ! (int) ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    ! STAT_ASA
    sfrow%stat_asa_b = get_val_frb(TELEM_LOC%STAT_ASA,  trows, sfrow) ! (int) ON/OFF for ASM-A F15W65B2  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    ! STAT_AMC
    sfrow%stat_amc_b = get_val_frb(TELEM_LOC%STAT_AMC,  trows, sfrow) ! (int) ON/OFF for ASM-AMC F15W65B3  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    ! STAT_HV1
    sfrow%stat_hv1_b = get_val_frb(TELEM_LOC%STAT_HV1,  trows, sfrow) ! (int) ENA/DIS for ASM-HV1 F15W65B4  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    ! STAT_HV2
    sfrow%stat_hv2_b = get_val_frb(TELEM_LOC%STAT_HV2,  trows, sfrow) ! (int) ENA/DIS for ASM-HV2 F15W65B5  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    ! STAT_RBM
    sfrow%stat_rbm_b = get_val_frb(TELEM_LOC%STAT_RBM,  trows, sfrow) ! (int) ENA/DIS for ASM-RBM F15W65B6

    ! STAT_BDR
    sfrow%stat_bdr_b = get_val_frb(TELEM_LOC%STAT_BDR,  trows, sfrow) ! (int) ENA/DIS for ASM-BDR F15W65B7

    ! BITRATE
    sfrow%bitrate    = get_val_frb(TELEM_LOC%BITRATE,   trows, sfrow) ! (int) Telemetry bit rate (F16W66)
  end subroutine update_asm_sfrow_mode_one

  !------------------------------------------------------------
  ! Validate asm_sfrow, checking ASM mode etc.
  !
  ! sfrow%invalid flag in the sfrow may be set.
  !------------------------------------------------------------
  subroutine validate_asm_sfrow_mode_one(trows, sfrow)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), intent(inout) :: sfrow

    if (.not. sfrow%is_valid) return  ! Likely (already) 'no matching with FRF found'

    ! lostf  ! (should be already flagged... just in case)
    if ((sfrow%with_frf) .and. (sfrow%frf%lostf > 0)) then
      sfrow%is_valid = .false.
      sfrow%reason_invalid = get_reason_invalid('lostf', sfrow%frf%lostf) ! defined in asm_fits_common
      return
    end if

    ! MODE_ASM & STAT_ASM
    if (sfrow%stat_asm_b < 0) then  ! STAT_ASM is neither 'ON' nor 'OFF'
      write(stderr,'("FATAL: strange. Contact the code developer.")')
      write(stderr,'(A,I4,A)') 'FATAL: Negative (uninitialized?) sfrow%stat_asm_b=', sfrow%stat_asm_b, '"'
      call dump_asm_sfrow(sfrow)
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
      return
    end if

    if (sfrow%stat_asm_b .ne. 1) then
      ! ASM is off.
      sfrow%is_valid = .false.
      sfrow%reason_invalid = get_reason_invalid('asmswoff') ! defined in asm_fits_common
    end if

    if (sfrow%mode_asm .ne. 1) then
      ! NOT in the ASM Mode.
      sfrow%is_valid = .false.
      sfrow%reason_invalid = get_reason_invalid('asmmodeoff') ! defined in asm_fits_common
    end if
  end subroutine validate_asm_sfrow_mode_one

  !------------------------------------------------------------
  ! Get the first value (n=0 in FXn+Y) of the specified Frame, Word, and maybe Byte
  !
  ! sfrow%invalid flag in the sfrow may be set.
  !
  ! Example: 
  !   For "F8n+1, W23", returns the value (0..255) of F1W23.
  !   For "F8n+4, W0, B0", returns 1 or 0 of F4W0B0.
  !
  !------------------------------------------------------------
  integer function get_val_frb(loc_fwb, trows, sfrow) result(kval)  ! fwb: frame, word, bit
    type(t_loc_fwb), intent(in) :: loc_fwb
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), intent(in) :: sfrow

    integer :: irowr, irowt, irowf, irowf_prev ! I-th-ROW-(Retrow|Telemetry|Frf)
    integer(kind=1) :: ibyte
    integer :: irow
    character(len=32) :: title

    kval = -998

    irow = get_telem_row_index_from_fr( frn=loc_fwb%f_offset &  ! defined in asm_fits_common
       , trows=trows, istart=sfrow%irowt, nrows=sfrow%nframes)
    if (irow .le. 0) then
      if (sfrow%with_frf) then
        write(stderr,'(A, I2, A, I5, A, I5, A, I2, A)') 'WARNING: Frame (', loc_fwb%f_offset &
         , ') does not exist in SF=(', sfrow%frf%sfn &
         , ') with the starting Row_Telemetry=(', sfrow%irowt &
         , '), which should not happen.  lostf=(', sfrow%frf%lostf, ').'
      else
        write(stderr,'(A, I2, A, I5, A, I5, A, I2, A)') 'WARNING: Frame (', loc_fwb%f_offset &
           , ') does not exist in a SF with the starting Row_Telemetry=(', sfrow%irowt &
           , '), which should not happen.  nframes=(', sfrow%nframes, ').'
      end if
      return
    else if (size(trows) < irow) then
      call err_exit_play_safe() ! defined in err_exit
      return ! redundant
    end if

    if (       TELEM_LOC%STAT_OBS%word == loc_fwb%word) then ! Status
      kval = trows(irow)%STAT_OBS  ! Word (ie, Byte)
      title = 'Word "Status"'
    else if (  TELEM_LOC%DPID_OBS%word == loc_fwb%word) then ! DP
      kval = trows(irow)%DPID_OBS  ! Word (ie, Byte)
      title = 'Word "DP"'
    else
      write(stderr,'(A, I3)') 'FATAL: Unknown word number: ', loc_fwb%word
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if

    if (kval < 0) then
      write(stderr,'(A, I2, A, I5, A, I5, A)') 'WARNING: '//trim(title)//' is not defined in Frame (' &
         , loc_fwb%f_offset, ') in SF=(', sfrow%frf%sfn, ') in Row_Telemetry=(', irow, ').'
      return
    end if

    if (loc_fwb%bit < 0) return
    ! Evaluate the Bit
    kval = btest2int_int4_as_1byte(kval, loc_fwb%bit) ! defined in fort_util
  end function get_val_frb

  !------------------------------------------------------------
  ! Returns merged FITS header from Telemetry and FRF
  !------------------------------------------------------------
  function get_merged_head(tfhead, frfhead) result(rethead)
    type(fits_header), intent(in) :: tfhead, frfhead
    type(fits_header) :: rethead

    rethead = tfhead
    rethead%FRFFILE%val = trim(frfhead%FRFFILE%val)
    rethead%kPASS%val = trim(frfhead%kPASS%val)
    rethead%TARGET1%val = trim(frfhead%TARGET1%val)
    rethead%TARGET2%val = trim(frfhead%TARGET2%val)
  end function get_merged_head

  !------------------------------------------------------------
  ! Returns merged FITS header from Telemetry and FRF
  !------------------------------------------------------------
  function get_asm_fits_header(tfhead, frfhead, trows, relrows, status, creator) result(rethead)
    type(fits_header), intent(in) :: tfhead, frfhead
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !type(asm_frfrow), dimension(:), intent(in) :: frfrows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    integer, intent(out) :: status
    character(len=*), intent(in), optional :: creator  ! e.g., asmmkevt ver.2021-03-31
    type(fits_header) :: rethead

    integer, dimension(1) :: ilocs
    real(kind=dp8) :: mjdtmp, mjd_interval
    integer :: irowt, loc_asm_s, loc_asm_e  ! Location_ASM_Start/End
    integer :: irow,  loc_frf_s, loc_frf_e  ! Location_FRF_for_asm_Start/End (immediately before/after ASM-mdoe)
    logical, dimension(size(relrows)) :: tfs  ! for MASK for findloc()
    integer, dimension(8) :: vals
    !character(8)  :: date
    !character(10) :: time
    character(5)  :: zone

    ! Initialization
    rethead = get_merged_head(tfhead, frfhead)

    if (present(creator)) then
      rethead%CREATOR%val = trim(creator)
    end if

    rethead%SFRAMES%val = count(relrows%is_valid)
    rethead%NROWSTEL%val = size(trows)

    rethead%TSTART%val   = UNDEF_REAL
    rethead%TEND%val     = UNDEF_REAL
    rethead%TSTARTMA%val = UNDEF_REAL
    rethead%TENDMA%val   = UNDEF_REAL
    rethead%TSTARTMS%val = UNDEF_REAL
    rethead%TENDMS%val   = UNDEF_REAL

    ! Whether the file contains the data.
    rethead%EXISTDAT%val = .false.  ! any(relrows%is_valid)

    ! Creation date
    call date_and_time(ZONE=zone, VALUES=vals)
    write(rethead%DATE%val, '(I4,"-",I0.2,"-",I0.2,"T",I0.2,":",I0.2,":",I0.2,".",I0.3,A)') &
       vals(1), vals(2), vals(3), vals(5), vals(6), vals(7), vals(8), trim(zone)
    
    ! Start/End times
    ilocs = findloc(relrows%is_valid, .true.)
    loc_asm_s = ilocs(1)
    if (loc_asm_s == 0) return  ! No valid data

    rethead%EXISTDAT%val = .true.
    rethead%TSTART%val = sfrow2mjd(relrows(loc_asm_s), trows)  ! defined in asm_fits_common
    rethead%TSTARTMA%val = rethead%TSTART%val

    ilocs = findloc(relrows%is_valid, .true., BACK=.true.)
    loc_asm_e = ilocs(1)
    mjdtmp = sfrow2mjd(relrows(loc_asm_e), trows)  ! The start time of the last valid Frame
    mjd_interval = mjdtmp - sfrow2mjd(relrows(loc_asm_e-1), trows)
    rethead%TEND%val   = mjdtmp + mjd_interval    ! The end time of the last valid Frame
    rethead%TENDMA%val = rethead%TEND%val

    ! First/Last Eulers and other values from FRF
    !..................................................
    tfs(:) = .false.
    tfs(1:loc_asm_s) = .true.
    ilocs = findloc(relrows%with_frf, .true., MASK=tfs, BACK=.true.)
    irow = ilocs(1) ! Last valid relrow(i) before the first ASM-mode
    if (irow > 0) then
      rethead%FRFSFN_S%val = relrows(irow)%frf%sfn
      rethead%FRFMJD_S%val = relrows(irow)%frf%mjds(1)
      rethead%EULER_S1%val = rad2deg(relrows(irow)%frf%eulers(1, 1))   ! radian to degree
      rethead%EULER_S2%val = rad2deg(relrows(irow)%frf%eulers(2, 1))
      rethead%EULER_S3%val = rad2deg(relrows(irow)%frf%eulers(3, 1))
      rethead%D_EUL_S1%val = rad2deg(relrows(irow)%frf%d_eulers(1, 1)) ! radian to degree
      rethead%D_EUL_S2%val = rad2deg(relrows(irow)%frf%d_eulers(2, 1))
      rethead%D_EUL_S3%val = rad2deg(relrows(irow)%frf%d_eulers(3, 1))
      rethead%ALTIT_S%val  = relrows(irow)%frf%height(1)
      rethead%LONLATS1%val = relrows(irow)%frf%lon_lat(1, 1)
      rethead%LONLATS2%val = relrows(irow)%frf%lon_lat(2, 1)
      rethead%DIST_E_S%val = relrows(irow)%frf%dist_earth(1)
      rethead%CO_ET_S1%val = relrows(irow)%frf%coords_earth(1, 1)
      rethead%CO_ET_S2%val = relrows(irow)%frf%coords_earth(2, 1)
      rethead%COR_S%val    = relrows(irow)%frf%cor(1)
      rethead%CO_EM_S1%val = relrows(irow)%frf%coords_magnet(1, 1)
      rethead%CO_EM_S2%val = relrows(irow)%frf%coords_magnet(2, 1)
      rethead%CO_SN_S1%val = relrows(irow)%frf%coords_sun(1, 1)
      rethead%CO_SN_S2%val = relrows(irow)%frf%coords_sun(2, 1)
      rethead%SUNPS_S%val  = relrows(irow)%frf%sunps(1)
      rethead%ELVYS_S%val  = relrows(irow)%frf%elvys(1)
      rethead%EFLAGS_S%val = relrows(irow)%frf%eflags(1)
    end if

    tfs(:) = .false.
    tfs(loc_asm_e:size(tfs)) = .true.
    ilocs = findloc(relrows%with_frf, .true., MASK=tfs)
    irow = ilocs(1) ! First valid relrow(i) after the last ASM-mode
    if (irow > 0) then
      rethead%FRFSFN_E%val = relrows(irow)%frf%sfn
      rethead%FRFMJD_E%val = relrows(irow)%frf%mjds(1)
      rethead%EULER_E1%val = rad2deg(relrows(irow)%frf%eulers(1, 1))
      rethead%EULER_E2%val = rad2deg(relrows(irow)%frf%eulers(2, 1))
      rethead%EULER_E3%val = rad2deg(relrows(irow)%frf%eulers(3, 1))
      rethead%D_EUL_E1%val = rad2deg(relrows(irow)%frf%d_eulers(1, 1))
      rethead%D_EUL_E2%val = rad2deg(relrows(irow)%frf%d_eulers(2, 1))
      rethead%D_EUL_E3%val = rad2deg(relrows(irow)%frf%d_eulers(3, 1))
      rethead%ALTIT_E%val  = relrows(irow)%frf%height(1)
      rethead%LONLATE1%val = relrows(irow)%frf%lon_lat(1, 1)
      rethead%LONLATE2%val = relrows(irow)%frf%lon_lat(2, 1)
      rethead%DIST_E_E%val = relrows(irow)%frf%dist_earth(1)
      rethead%CO_ET_E1%val = relrows(irow)%frf%coords_earth(1, 1)
      rethead%CO_ET_E2%val = relrows(irow)%frf%coords_earth(2, 1)
      rethead%COR_E%val    = relrows(irow)%frf%cor(1)
      rethead%CO_EM_E1%val = relrows(irow)%frf%coords_magnet(1, 1)
      rethead%CO_EM_E2%val = relrows(irow)%frf%coords_magnet(2, 1)
      rethead%CO_SN_E1%val = relrows(irow)%frf%coords_sun(1, 1)
      rethead%CO_SN_E2%val = relrows(irow)%frf%coords_sun(2, 1)
      rethead%SUNPS_E%val  = relrows(irow)%frf%sunps(1)
      rethead%ELVYS_E%val  = relrows(irow)%frf%elvys(1)
      rethead%EFLAGS_E%val = relrows(irow)%frf%eflags(1)
    end if

    ! Status values from the SF when the ASM-Mode became on
    !..................................................
    rethead%ROW4STAT%val = relrows(loc_asm_s)%irowt
    rethead%STAT__ASM%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_asm_b, 'asm'))
    rethead%STAT__ASA%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_asa_b, 'asa'))
    rethead%STAT__AMC%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_amc_b, 'amc'))
    rethead%STAT__HV1%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_hv1_b, 'hv1'))
    rethead%STAT__HV2%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_hv2_b, 'hv2'))
    rethead%STAT__RBM%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_rbm_b, 'rbm'))
    rethead%STAT__BDR%val = trim(get_onoff_enadis(relrows(loc_asm_s)%stat_bdr_b, 'bdr'))

    ! Start/End times of Slew360 mode
    ilocs = findloc(relrows%mode_slew, 1, MASK=relrows%is_valid)
    if (ilocs(1) == 0) return  ! No ASM-Slew360 data

    rethead%TSTARTMS%val = sfrow2mjd(relrows(ilocs(1)), trows)  ! defined in asm_fits_common
    ilocs = findloc(relrows%mode_slew, 1, MASK=relrows%is_valid, BACK=.true.)
    rethead%TENDMS%val   = sfrow2mjd(relrows(ilocs(1)), trows) + mjd_interval ! The end time of the last valid Slew360-Mode Frame
  end function get_asm_fits_header


  !------------------------------------------------------------
  ! Output FITS header in a HDU of the ASM data FITS file
  !------------------------------------------------------------
  subroutine write_asm_fits_header(unit, fhd, status, comname, args, primary)
    implicit none
    character(len=*), parameter :: Subname = 'write_asm_fits_header'
    integer, intent(in) :: unit
    type(fits_header), intent(in) :: fhd
    integer, intent(out) :: status
    character(len=*), intent(in), optional :: comname  ! Command name
    character(len=*), dimension(:), intent(in), optional :: args  ! command-line Arguments
    logical, intent(in), optional :: primary  ! if .true., it is for Primary header.
    logical :: is_primary
    integer :: i
    character(len=2048) :: strarg

    is_primary = .false.
    if (present(primary)) then
      is_primary = primary
    end if

    call FTPKYS(unit, fhd%TELESCOP%name, fhd%TELESCOP%val, fhd%TELESCOP%comment, status)
    call warn_ftpcl_status(status, 'ftpkys', trim(Subname)//':TELESCOP')
    call FTPKYJ(unit, fhd%SACD%name, fhd%SACD%val, fhd%SACD%comment, status)
    call warn_ftpcl_status(status, 'ftpkyj', trim(Subname)//':SACD')
    call FTPKYS(unit, fhd%INSTRUME%name, fhd%INSTRUME%val, fhd%INSTRUME%comment, status)
    call FTPKYS(unit, fhd%ORIGIN%name,   fhd%ORIGIN%val,   fhd%ORIGIN%comment,   status)
    call FTPKYS(unit, fhd%CREATOR%name,   fhd%CREATOR%val, fhd%CREATOR%comment,  status)
    call FTPKYS(unit, fhd%FILENAME%name, fhd%FILENAME%val, fhd%FILENAME%comment, status)
    call FTPKYS(unit, fhd%FRFFILE%name, fhd%FRFFILE%val, fhd%FRFFILE%comment, status)
    call FTPKYS(unit, fhd%kPASS%name, fhd%kPASS%val, fhd%kPASS%comment, status)

    if (is_primary) return

    call FTPKYL(unit, fhd%EXISTDAT%name, fhd%EXISTDAT%val, fhd%EXISTDAT%comment, status)
    call warn_ftpcl_status(status, 'ftpkyl', trim(Subname)//':EXISTDAT')

    call FTPKYJ(unit, fhd%SFRAMES%name, fhd%SFRAMES%val, fhd%SFRAMES%comment, status)
    call FTPKYJ(unit, fhd%NROWSTEL%name, fhd%NROWSTEL%val, fhd%NROWSTEL%comment, status)
    call FTPKYD(unit, fhd%TSTART%name,   fhd%TSTART%val,   14, fhd%TSTART%comment, status)
    call FTPKYD(unit, fhd%TEND%name,     fhd%TEND%val,     14, fhd%TEND%comment, status)
    call FTPKYD(unit, fhd%TSTARTMA%name, fhd%TSTARTMA%val, 14, fhd%TSTARTMA%comment, status)
    call FTPKYD(unit, fhd%TENDMA%name,   fhd%TENDMA%val,   14, fhd%TENDMA%comment, status)
    call FTPKYD(unit, fhd%TSTARTMS%name, fhd%TSTARTMS%val, 14, fhd%TSTARTMS%comment, status)
    call FTPKYD(unit, fhd%TENDMS%name,   fhd%TENDMS%val,   14, fhd%TENDMS%comment, status)

    call FTPKYJ(unit, fhd%ROW4STAT%name, fhd%ROW4STAT%val, fhd%ROW4STAT%comment, status)
    call FTPKYS(unit, fhd%STAT__ASM%name, fhd%STAT__ASM%val, fhd%STAT__ASM%comment, status)
    call FTPKYS(unit, fhd%STAT__ASA%name, fhd%STAT__ASA%val, fhd%STAT__ASA%comment, status)
    call FTPKYS(unit, fhd%STAT__AMC%name, fhd%STAT__AMC%val, fhd%STAT__AMC%comment, status)
    call FTPKYS(unit, fhd%STAT__HV1%name, fhd%STAT__HV1%val, fhd%STAT__HV1%comment, status)
    call FTPKYS(unit, fhd%STAT__HV2%name, fhd%STAT__HV2%val, fhd%STAT__HV2%comment, status)
    call FTPKYS(unit, fhd%STAT__RBM%name, fhd%STAT__RBM%val, fhd%STAT__RBM%comment, status)
    call FTPKYS(unit, fhd%STAT__BDR%name, fhd%STAT__BDR%val, fhd%STAT__BDR%comment, status)

    call FTPKYD(unit, fhd%EQUINOX%name, fhd%EQUINOX%val,   -7, fhd%EQUINOX%comment, status)
    call FTPKYJ(unit, fhd%FRFSFN_S%name, fhd%FRFSFN_S%val, fhd%FRFSFN_S%comment, status)
    call FTPKYD(unit, fhd%FRFMJD_S%name, fhd%FRFMJD_S%val, 14, fhd%FRFMJD_S%comment, status)
    call FTPKYJ(unit, fhd%FRFSFN_E%name, fhd%FRFSFN_E%val, fhd%FRFSFN_E%comment, status)
    call FTPKYD(unit, fhd%FRFMJD_E%name, fhd%FRFMJD_E%val, 14, fhd%FRFMJD_E%comment, status)
    call FTPKYD(unit, fhd%EULER_S1%name, fhd%EULER_S1%val, 10, fhd%EULER_S1%comment, status)
    call FTPKYD(unit, fhd%EULER_S2%name, fhd%EULER_S2%val, 10, fhd%EULER_S2%comment, status)
    call FTPKYD(unit, fhd%EULER_S3%name, fhd%EULER_S3%val, 10, fhd%EULER_S3%comment, status)
    call FTPKYD(unit, fhd%EULER_E1%name, fhd%EULER_E1%val, 10, fhd%EULER_E1%comment, status)
    call FTPKYD(unit, fhd%EULER_E2%name, fhd%EULER_E2%val, 10, fhd%EULER_E2%comment, status)
    call FTPKYD(unit, fhd%EULER_E3%name, fhd%EULER_E3%val, 10, fhd%EULER_E3%comment, status)
    call FTPKYD(unit, fhd%D_EUL_S1%name, fhd%D_EUL_S1%val, 10, fhd%D_EUL_S1%comment, status)
    call FTPKYD(unit, fhd%D_EUL_S2%name, fhd%D_EUL_S2%val, 10, fhd%D_EUL_S2%comment, status)
    call FTPKYD(unit, fhd%D_EUL_S3%name, fhd%D_EUL_S3%val, 10, fhd%D_EUL_S3%comment, status)
    call FTPKYD(unit, fhd%D_EUL_E1%name, fhd%D_EUL_E1%val, 10, fhd%D_EUL_E1%comment, status)
    call FTPKYD(unit, fhd%D_EUL_E2%name, fhd%D_EUL_E2%val, 10, fhd%D_EUL_E2%comment, status)
    call FTPKYD(unit, fhd%D_EUL_E3%name, fhd%D_EUL_E3%val, 10, fhd%D_EUL_E3%comment, status)
    call FTPKYD(unit, fhd%ALTIT_S%name , fhd%ALTIT_S%val , 10, fhd%ALTIT_S%comment, status)
    call FTPKYD(unit, fhd%ALTIT_E%name , fhd%ALTIT_E%val , 10, fhd%ALTIT_E%comment, status)
    call FTPKYD(unit, fhd%LONLATS1%name, fhd%LONLATS1%val, 10, fhd%LONLATS1%comment, status)
    call FTPKYD(unit, fhd%LONLATS2%name, fhd%LONLATS2%val, 10, fhd%LONLATS2%comment, status)
    call FTPKYD(unit, fhd%LONLATE1%name, fhd%LONLATE1%val, 10, fhd%LONLATE1%comment, status)
    call FTPKYD(unit, fhd%LONLATE2%name, fhd%LONLATE2%val, 10, fhd%LONLATE2%comment, status)
    call FTPKYD(unit, fhd%DIST_E_S%name, fhd%DIST_E_S%val, 10, fhd%DIST_E_S%comment, status)
    call FTPKYD(unit, fhd%DIST_E_E%name, fhd%DIST_E_E%val, 10, fhd%DIST_E_E%comment, status)
    call FTPKYD(unit, fhd%CO_ET_S1%name, fhd%CO_ET_S1%val, 10, fhd%CO_ET_S1%comment, status)
    call FTPKYD(unit, fhd%CO_ET_S2%name, fhd%CO_ET_S2%val, 10, fhd%CO_ET_S2%comment, status)
    call FTPKYD(unit, fhd%CO_ET_E1%name, fhd%CO_ET_E1%val, 10, fhd%CO_ET_E1%comment, status)
    call FTPKYD(unit, fhd%CO_ET_E2%name, fhd%CO_ET_E2%val, 10, fhd%CO_ET_E2%comment, status)
    call FTPKYD(unit, fhd%COR_S%name,    fhd%COR_S%val,    10, fhd%COR_S%comment, status)
    call FTPKYD(unit, fhd%COR_E%name,    fhd%COR_E%val,    10, fhd%COR_E%comment, status)
    call FTPKYD(unit, fhd%CO_EM_S1%name, fhd%CO_EM_S1%val, 10, fhd%CO_EM_S1%comment, status)
    call FTPKYD(unit, fhd%CO_EM_S2%name, fhd%CO_EM_S2%val, 10, fhd%CO_EM_S2%comment, status)
    call FTPKYD(unit, fhd%CO_EM_E1%name, fhd%CO_EM_E1%val, 10, fhd%CO_EM_E1%comment, status)
    call FTPKYD(unit, fhd%CO_EM_E2%name, fhd%CO_EM_E2%val, 10, fhd%CO_EM_E2%comment, status)
    call FTPKYD(unit, fhd%CO_SN_S1%name, fhd%CO_SN_S1%val, 10, fhd%CO_SN_S1%comment, status)
    call FTPKYD(unit, fhd%CO_SN_S2%name, fhd%CO_SN_S2%val, 10, fhd%CO_SN_S2%comment, status)
    call FTPKYD(unit, fhd%CO_SN_E1%name, fhd%CO_SN_E1%val, 10, fhd%CO_SN_E1%comment, status)
    call FTPKYD(unit, fhd%CO_SN_E2%name, fhd%CO_SN_E2%val, 10, fhd%CO_SN_E2%comment, status)
    call FTPKYJ(unit, fhd%SUNPS_S%name , fhd%SUNPS_S%val , fhd%SUNPS_S%comment, status)
    call FTPKYJ(unit, fhd%SUNPS_E%name , fhd%SUNPS_E%val , fhd%SUNPS_E%comment, status)
    call FTPKYD(unit, fhd%ELVYS_S%name , fhd%ELVYS_S%val , 10, fhd%ELVYS_S%comment, status)
    call FTPKYD(unit, fhd%ELVYS_E%name , fhd%ELVYS_E%val , 10, fhd%ELVYS_E%comment, status)
    call FTPKYJ(unit, fhd%EFLAGS_S%name, fhd%EFLAGS_S%val, fhd%EFLAGS_S%comment, status)
    call FTPKYJ(unit, fhd%EFLAGS_E%name, fhd%EFLAGS_E%val, fhd%EFLAGS_E%comment, status)
    call FTPKYS(unit, fhd%DATE%name, fhd%DATE%val, fhd%DATE%comment, status)

    call FTPCOM(unit, OUTFTCOMMENT1, status)  ! defined in asm_fits_common
    call FTPCOM(unit, OUTFTCOMMENT2, status)  ! defined in asm_fits_common

    if (present(comname) .and. present(args)) then
      strarg = 'Command: '//trim(basename(comname))
      do i=1, size(args)
        strarg = trim(strarg) // ' ' // trim(args(i))
      end do
      call FTPHIS(unit, strarg, status)
    end if
  end subroutine write_asm_fits_header

  !------------------------------------------------------------
  ! Warn if failing in writing a column
  !------------------------------------------------------------
  subroutine warn_ftpcl_status(status, funcname, kwd)
    integer, intent(in) :: status
    character(len=*), intent(in) :: funcname, kwd
    character(len=30) :: errtext ! for FITS error to receive

    if (status .ne. 0) then
      call FTGERR(status, errtext)
      write(stderr,'("ERROR: (",A,") Failed in ",A,"() with status=", I12,": ",A)') &
         trim(kwd), trim(funcname), status, trim(errtext)
    end if
  end subroutine warn_ftpcl_status

  !------------------------------------------------------------
  ! Output FITS file of the ASM data
  !------------------------------------------------------------
  subroutine write_cols(unit, trows, relrows, colheads, status)
    implicit none
    character(len=*), parameter :: Subname = 'write_cols'
    integer, intent(in) :: unit
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !type(asm_frfrow), dimension(:), intent(in) :: frfrows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    type(t_asm_colhead), dimension(:), allocatable, intent(in) :: colheads
    integer, intent(out) :: status
    integer :: i, ikind, ittype, iprm, itrow, iframe, irelrow, iout, iend, iasm, naxis2, ntrows, nrelrows, kval
    character(len=30) :: errtext ! for FITS error to receive
    character(len=1024) :: usermsg  ! for humans
    character(len=LEN_READABLE_KEY) :: ckey
    real(dp8) :: rval
    real(dp8), dimension(:), allocatable :: cold
    integer, dimension(:), allocatable :: colj
    integer(kind=2), dimension(:), allocatable :: coli
    character(len=8), dimension(:), allocatable :: cols8

    naxis2 = sum(relrows%nframes, relrows%is_valid)  ! Number of valid frames.
    if (naxis2 == 0) return ! i.e., no table data. Header keyword EXISTDAT should be FALSE, too.

    ntrows   = size(trows)
    nrelrows = size(relrows)

    allocate(coli(naxis2))
    allocate(colj(naxis2))
    allocate(cold(naxis2))
    allocate(cols8(naxis2))

    !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
    status = 0 
    iframe = 0 
    ittype = 0  ! TTYPEn
    iasm = 0    ! The i-th number of the main ASM data (should be repeated NWORDS_MAIN=96 times)
    ! do ikind=1, size(COL_FORM_UNITS)
    do ikind=1, size(colheads)
      iout = 0  ! Row number of each output Column
      iend = 0
      ckey = colheads(ikind)%key 
      select case(trim(ckey))
      case('main')  ! Main ASM data (96 cells)
        iasm = iasm + 1
        if (iasm > NWORDS_MAIN) call err_exit_play_safe("ASM more than 96 times repeated.")  ! sanity check
        iout = 0  ! Row number of each output Column
        coli(:) = 0
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            kval = trows(itrow)%asmdats(get_asmdats_row4col(iasm))
            if ((kval < 0) .or. (255 < kval)) then  ! sanity check; this should never happen.
              write(usermsg, '("ASM-data out of range =",I12," iasm=",I2," asmdats_row=",I2," ttype=",A)') &
                 kval, iasm, get_asmdats_row4col(iasm), trim(get_ttype_main(iasm))
              call err_exit_with_msg(usermsg)
            end if
            coli(iout) = int(kval, kind=2)
          end do
        end do
        ittype = ittype + 1
        call FTPCLI(unit, ittype, 1, 1, naxis2, coli, status)  ! colnum = 1
        call warn_ftpcl_status(status, 'FTPCLI', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case('Tstart')
        cold(:) = 0.0d0
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            cold(iout) = trows(itrow)%mjd 
          end do
        end do
        ittype = ittype + 1  ! FITS Table column number
        call FTPCLD(unit, ittype, 1, 1, naxis2, cold, status)  ! colnum = 1
        call warn_ftpcl_status(status, 'FTPCLD', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case('Euler')
        ! All frames in the same SF have a common value.
        ! In practice, Euler can never be determined because no FRFs match where ASM-Mode is ON.
        iprm = get_colhead_type_num(colheads(ikind), 'Euler')
        !do iprm=1, 3
          cold(:) = 0.0d0
          iout = 0
          iend = 0
          do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
            if (.not. relrows(irelrow)%is_valid) cycle
            iout = iend + 1
            iend = iout+relrows(irelrow)%nframes-1
            if (iend > naxis2) then  ! sanity check
              write(stderr, '("ERROR: strange (in Euler) with iout=",I6," <=> ",I6,"(max) when SFrow=",I5,"/",I5)') &
                 iend, naxis2, irelrow, nrelrows
              call EXIT(1)
            end if
            rval = relrows(irelrow)%frf%eulers(iprm,1)
            if (rval .ge. -360) then
              rval = rad2deg(rval)  ! Convert radian to degree
            end if
            cold(iout:iend) = rval  ! Same value for the entire SF
          end do
          ittype = ittype + 1  ! FITS Table column number
          call FTPCLD(unit, ittype, 1, 1, naxis2, cold, status)  ! colnum = 1
          call warn_ftpcl_status(status, 'FTPCLD', ckey)
          call modify_ttype_comment(unit, ittype, ckey, status)

      case('SFNum', 'SFNTelem')
        ! Integer*4 columns based on the SF
        do iprm=1, 1
          colj(:) = 0
          iout = 0
          iend = 0
          do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
            if (.not. relrows(irelrow)%is_valid) cycle
            iout = iend + 1
            iend = iout+relrows(irelrow)%nframes-1
            if (iend > naxis2) then  ! sanity check
              write(stderr, '("ERROR: strange (",A,") with iout=",I6," <=> ",I6,"(max) when SFrow=",I5,"/",I5)') &
                 trim(ckey), iend, naxis2, irelrow, nrelrows
              call err_exit_play_safe()
            end if
            select case(trim(ckey))
            case('SFNum')
              colj(iout:iend) = relrows(irelrow)%frf%sfn
            case('SFNTelem')
              colj(iout:iend) = relrows(irelrow)%SFNTelem
            case default
              call err_exit_play_safe(trim(ckey)) ! never happens
            end select
          end do
          ittype = ittype + 1  ! FITS Table column number
          call FTPCLJ(unit, ittype, 1, 1, naxis2, colj, status)  ! colnum = 1
          call warn_ftpcl_status(status, 'FTPCLJ', ckey)
          call modify_ttype_comment(unit, ittype, ckey, status)
          ! call FTSNUL(unit,colnum,snull > status) ! Define string representation for NULL column
          ! call FTTNUL(unit,colnum,tnull > status) ! Define the integer(!) value to be treated as NULL
        end do

      case('SF2bits', 'Mode_ASM', 'Mode_PHA', 'ModeSlew', 'ModeSleM', 'ModeSleP', 'bitrate')  ! 'Fr6bits', 'i_frame', are Frame-based 
        ! Integer*2 columns
        do iprm=1, 1
          coli(:) = 0
          iout = 0
          iend = 0
          do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
            if (.not. relrows(irelrow)%is_valid) cycle
            iout = iend + 1
            iend = iout+relrows(irelrow)%nframes-1
            if (iend > naxis2) then  ! sanity check
              write(stderr, '("ERROR: strange (in mode) with iout=",I6," <=> ",I6,"(max) when SFrow=",I5,"/",I5)') &
                 iend, naxis2, irelrow, nrelrows
              call err_exit_play_safe()
            end if

            select case(trim(ckey))
            case('SF2bits') 
              coli(iout:iend) = relrows(irelrow)%sf2bits 
            case('Mode_ASM')
              coli(iout:iend) = relrows(irelrow)%mode_asm
            case('Mode_PHA')
              coli(iout:iend) = relrows(irelrow)%mode_PHA 
            case('ModeSlew')
              coli(iout:iend) = relrows(irelrow)%mode_slew
            case('ModeSleM')
              coli(iout:iend) = relrows(irelrow)%mode_slem
            case('ModeSleP')
              coli(iout:iend) = relrows(irelrow)%mode_slep
            case('bitrate')
              coli(iout:iend) = relrows(irelrow)%bitrate
            case default
              call err_exit_play_safe() ! never happens
            end select
          end do
          ittype = ittype + 1  ! FITS Table column number
          call FTPCLI(unit, ittype, 1, 1, naxis2, coli, status)  ! colnum = 1
          call warn_ftpcl_status(status, 'FTPCLI', ckey)
          call modify_ttype_comment(unit, ittype, ckey, status)
          ! call FTSNUL(unit,colnum,snull > status) ! Define string representation for NULL column
          ! call FTTNUL(unit,colnum,tnull > status) ! Define the integer(!) value to be treated as NULL
        end do

      case('Fr6bits', 'Status_C', 'DP_C')    
        ! Integer*2, frame-based
        coli(:) = 0
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            select case(trim(ckey))
            case('Fr6bits')
              coli(iout) = int(trows(itrow)%fr_6bit, kind=2)
            case('Status_C')    
              coli(iout) = int(trows(itrow)%STAT_OBS, kind=2)
            case('DP_C')    
              coli(iout) = int(trows(itrow)%DPID_OBS, kind=2)
            case default
              call err_exit_play_safe('Integer*2 part') ! should never happen
            end select
          end do
        end do
        ittype = ittype + 1  ! FITS Table column number
        call FTPCLI(unit, ittype, 1, 1, naxis2, coli, status)  ! colnum = 1
        call warn_ftpcl_status(status, 'FTPCLI', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case('Status_S', 'DP_S')    
        ! Character*8, frame-based
        cols8(:) = ''
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            select case(trim(ckey))
            case('Status_S')
              cols8(iout) = trows(itrow)%STAT_OBS_B8
            case('DP_S')
              cols8(iout) = trows(itrow)%DPID_OBS_B8
            case default
              call err_exit_play_safe('Chracter*8 part') ! should never happen
            end select
          end do
        end do
        ittype = ittype + 1  ! FITS Table column number
        call FTPCLS(unit, ittype, 1, 1, naxis2, cols8, status)
        call warn_ftpcl_status(status, 'FTPCLS', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case('i_frame')    
        ! Integer*4, frame-based
        colj(:) = 0
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            select case(trim(ckey))
            case('i_frame')    
              colj(iout) = trows(itrow)%i_frame
            case default
              call err_exit_play_safe('Integer*4 part') ! should never happen
            end select
          end do
        end do
        ittype = ittype + 1  ! FITS Table column number
        call FTPCLJ(unit, ittype, 1, 1, naxis2, colj, status)  ! colnum = 1
        call warn_ftpcl_status(status, 'FTPCLJ', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case('ACS_C', 'ASM1_C', 'ASM2_C')   
        ! Integer*2 columns (multiple)
        iprm = get_colhead_type_num(colheads(ikind), trim(ckey))
        do irelrow=1, nrelrows  ! Each relrow (=asm_sfrow) number
          if (.not. relrows(irelrow)%is_valid) cycle
          do itrow=relrows(irelrow)%irowt, relrows(irelrow)%irowt + relrows(irelrow)%nframes - 1  ! Telemetry Row number
            iout = iout + 1
            if (iout > naxis2) then  ! sanity check
              write(usermsg, '("strange with iout=",I6," <=> ",I6,"(max)")') iout, naxis2
              call err_exit_with_msg(usermsg)
            end if
            select case(trim(ckey))
            case('ACS_C')   
              coli(iout) = int(trows(itrow)%acss(iprm), kind=2)
            case('ASM1_C')   
              coli(iout) = int(trows(itrow)%asm1_commons(iprm), kind=2)
            case('ASM2_C')   
              coli(iout) = int(trows(itrow)%asm2_commons(iprm), kind=2)
            case default
              call err_exit_play_safe('Integer*2 part') ! should never happen
            end select
          end do
        end do
        ittype = ittype + 1  ! FITS Table column number
        call FTPCLI(unit, ittype, 1, 1, naxis2, coli, status)
        call warn_ftpcl_status(status, 'FTPCLI', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

      case default
        call err_exit_with_msg('('//Subname//') Parameter '//trim(ckey)//' is not yet taken into account.')
      end select
    end do

    if (allocated(coli)) deallocate(coli)
    if (allocated(colj)) deallocate(colj)
    if (allocated(cold)) deallocate(cold)
    if (allocated(cols8)) deallocate(cols8)
  end subroutine write_cols

  !------------------------------------------------------------
  ! Output FITS file of the ASM data
  !------------------------------------------------------------
  subroutine write_asm_evt_fits(outfil, fhead, trows, relrows, status, comname, args, outcolkeys)
    implicit none
    integer, parameter :: MY_FUNIT = 59  ! arbitrary
    character(len=*), parameter :: Subname = 'write_asm_evt_fits'
    character(len=*), parameter :: Extname = 'ASM table'

    character(len=*), intent(in) :: outfil
    type(fits_header), intent(inout) :: fhead  ! Mainly for 1st-Extension header. EXISTDAT is written.
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    integer, intent(out) :: status
    character(len=*), intent(in) :: comname
    character(len=*), dimension(:), intent(in) :: args
    character(len=*), dimension(:), intent(in), optional :: outcolkeys  ! e.g., ['Tstart', 'Euler', SFNum]

    integer :: unit, bitpix, blocksize, hdutype !, naxis, nframes, naxis1
    character(len=30) :: errtext
    integer :: nhdu
    logical simple, extend
    integer, dimension(0) :: naxes
    type(t_asm_colhead), dimension(:), allocatable :: colheads
    integer :: i, naxis2

    !character(len=80), dimension(2) :: ttypes = (/ 'TIME', 'Char' /), tunits, &
    !     tforms = (/ '1D ', '20A' /)  ! 20A20?
    character(len=80), dimension(:), allocatable :: ttypes, tunits, tforms
    real(kind=dp8), dimension(2) :: dvalues = (/ 12345.6, 7.89 /), d2values = (/ 0.023, 0.0045 /)
    character(len=20), dimension(2) :: svalues = (/ 'saisho', 'tsugi1' /)

    status=0

    if (present(outcolkeys)) then
      colheads = get_colheads(outcolkeys)
    else
      colheads = get_colheads()  ! Column Header info
                                 ! %(key, type, prm%(form, unit, comm, dim))
    end if

    ! Get an unused Logical Unit Number to use to create the FITS file
    call ftgiou(unit,status)
    if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) then
      write(stderr,'("WARNING: Failed in ftgiou(): unit = ",I12,". Manually reset to ",I3)') unit, MY_FUNIT
      unit = MY_FUNIT
    end if

    ! create the new empty FITS file blocksize=1
    call ftinit(unit,'!'//outfil,blocksize,status)
    call warn_ftpcl_status(status, 'FTINIT', Subname)
    if (status .ne. 0) call err_exit_with_msg('Is the output directory writable?')
    !! Note: Without this, Segmentation Fault may occur.

    call FTGERR(status, errtext)  ! for Debugging
    call FTGHDN(unit, nhdu)  ! CHDU: Current HDU

    ! initialize parameters about the FITS image (300 x 200 16-bit integers)
    simple=.true.
    bitpix=16  ! signed 2-byte, 8: unsigned 1-byte, -32: real, -64: double
    extend=.true.

    ! write the required (Primary) header keywords
    call ftphpr(unit, simple, bitpix, 0, naxes, 0, 1, extend, status) ! Because naxis=0, naxes is ignored.
    ! write other optional keywords to the header
    call write_asm_fits_header(unit, fhead, status, primary=.true.)
    
    ! ------------ Write Extension ------------
    
    ttypes = colheads%type
    tforms = colheads%prm%form
    tunits = colheads%prm%unit

    ! FTIBIN(unit,nrows,tfields,ttypes,tforms,tunits,Extname,varidat > status) ! nrows should be 0 ! FiTs-Insert-BINary-table
    !! Creates an extension with basic header and moves to it.
    call FTIBIN(unit, 0, size(tforms) &
              , ttypes, tforms, tunits, Extname, .true., status) ! Creates an extension with basic header and moves to it.
    call warn_ftpcl_status(status, 'FTIBIN', Subname)

    !call FTGHDN(unit, nhdu)  ! current HDU (for DEBUG)
    !call FTGERR(status, errtext)
    !if (IS_DEBUG()) print *,'test-new-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)

    call write_asm_fits_header(unit, fhead, status, comname, args, primary=.false.)

    call write_cols(unit, trows, relrows, colheads, status)  !!!!!!!!!!!!!!!

    ! close the file and free the unit number
    call ftclos(unit, status)
    call err_exit_if_status(status, 'in FTCLOS()')
    call ftfiou(unit, status)

    if (allocated(ttypes)) deallocate(ttypes)
    if (allocated(tforms)) deallocate(tforms)
    if (allocated(tunits)) deallocate(tunits)
    if (allocated(colheads)) deallocate(colheads)

    ! A terrible "emergency" work-around practice for an empty-table FITS...
    if (.not. any(relrows%is_valid)) then
      call force_delete_trailing_bytes_fits(outfil)
    end if
  end subroutine write_asm_evt_fits


  !------------------------------------------------------------
  ! Delete FORCIBLY the last bytes in the FITS file with direct access.
  !
  ! *** CAUTION ***
  !
  ! This is an extremely dirty hack for work-around of FITSIO mal-behaviour!!
  ! Basically this read and write the FITS as a binary, deleting
  ! a certain number of the bytes at the tail, when the FITS file
  ! contains no table data.
  !
  ! The number should depend on the numbers of the FITS header keywords
  ! and table columns; however this routine takens account of none!
  ! Therefore, this routine would fail immediately as soon as a FITS header
  ! keyword or table column is deleted or a new one is added in the output FITS
  ! by this package.
  !
  !------------------------------------------------------------
  subroutine force_delete_trailing_bytes_fits(outfil)
    implicit none
    integer, parameter :: MY_FUNIT = 123  ! arbitrary
    character(len=*), parameter :: Subname = 'force_delete_trailing_bytes_fits'
    character(len=*), intent(in) :: outfil

    integer, parameter :: Fitssize = 40320
    character(len=SIZE_EMPTY_FITS_BYTES) :: chall ! defined in asm_fits_common
    integer :: ios

    open(unit=MY_FUNIT, iostat=ios, status='old', file=outfil, form='unformatted', access='direct', recl=SIZE_EMPTY_FITS_BYTES)
    call err_exit_if_status(ios, '('//trim(subname)//')Failed in reopening '//trim(outfil))
    read(MY_FUNIT,rec=1) chall
    close(unit=MY_FUNIT)
    open(unit=MY_FUNIT, iostat=ios, status='replace', file=outfil, form='unformatted' &
       , access='direct', recl=SIZE_EMPTY_FITS_BYTES, action='write')
    call err_exit_if_status(ios, '('//trim(subname)//')Failed in reopening to write '//trim(outfil))
    write(MY_FUNIT,rec=1) chall
    close(unit=MY_FUNIT)
  end subroutine force_delete_trailing_bytes_fits


  !------------------------------------------------------------
  ! Test output for debugging.
  !------------------------------------------------------------
  subroutine write_tmp_fits(fname, status)
    implicit none
    integer, parameter :: MY_FUNIT = 60  ! arbitrary
    character(len=*), intent(in) :: fname
    integer, intent(out) :: status
    integer :: unit, bitpix, naxis = 0
    integer :: blocksize, naxes(0)
    integer :: group,fpixel,nelements,array(300,200) ! i,j,
    logical :: simple =.true., extend = .true.
    integer :: nhdu
    character(len=30) :: errtext
    call ftgiou(unit, status)
!if (IS_DEBUG()) print *,'DEBUG:041:giou, unit=', unit, ' status=',status
if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) unit = MY_FUNIT
call ftinit(unit, '/tmp/out.fits', blocksize, status)
!if (IS_DEBUG()) print *,'DEBUG:042:b3ftart-out, status=',status
    call ftclos(unit, status)
    call ftfiou(unit, status)
  end subroutine write_tmp_fits
end module asm_fitsout

