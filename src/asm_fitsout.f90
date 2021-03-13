! Routines to output a FITS file
!
! Matching Telemetry and FRF, and outputting the result to a file.
!
module asm_fitsout
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  use err_exit
  use fort_util
  use asm_fits_common
  implicit none 
contains

  ! Table5.5.5-6 (pp.233-234)
  subroutine fill_asm_one(isabu, itotrow, i_fr64, telems, arout)
    implicit none
    integer, intent(in) :: isabu, itotrow, i_fr64
    integer(kind=1), dimension(:, :),  intent(in) :: telems
    type(asm_telem_row), dimension(:, :), intent(inout) :: arout  ! (iSabuFrame, Row)

    integer :: idet, ich, i_tele, i_out

    do idet=1, NUM_INSTR  ! =6
      do ich=1, NCHANS_TIME  ! =8
        i_tele = (idet-1)*2 + (ich-1)*16 + 5
        i_out = (idet-1)*NCHANS_PHA + ich
        arout(isabu, i_fr64)%asmdats(i_out) = telems(itotrow, i_tele)
            
        i_tele = (idet-1)*2 + (ich-1)*16 + 6
        i_out = (idet-1)*NCHANS_PHA + ich + 8
        arout(isabu, i_fr64)%asmdats(i_out) = telems(itotrow, i_tele)
      end do
    end do
  end subroutine fill_asm_one


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
!if (irowt < 300) print *, 'DEBUG:692: (row=', irowt,') iret=',iret,' prev2bit=',sf2bit_prev, ' 2bit=', trows(irowt)%sf_2bit

      ! Now this frame belongs to a new SF.
      if (iret > 0) rows2init(iret)%nframes = nframes  ! Set nFrames for the previous one.
      iret = iret + 1
      nframes = 1
      sf2bit_prev = trows(irowt)%sf_2bit
      fr6bit_prev = trows(irowt)%fr_6bit
      rows2init(iret) = asm_sfrow(irowt=irowt, sf2bits=trows(irowt)%sf_2bit)
    end do
!print *, 'DEBUG:795: iret=',iret

    if (iret > 0) rows2init(iret)%nframes = nframes  ! Set nFrames for the last one

    allocate(rets(iret), STAT=status)
!print *, 'DEBUG:796: iret=',iret, ' size(rets)=', size(rets)
    rets(:) = rows2init(1:iret)
    rets(:)%sfntelem = (/ (i, i=1, iret) /)

    if ((mod(siz_trows, NFRAMES_PER_SF) == 0) .and. (iret == siz_trows/NFRAMES_PER_SF)) then
      ! do nothing
    else
      write(stderr,'(A)') 'WARNING: Some Frames are missing from the telemetry.'
    end if
  end function get_ams_sfrow_init

  ! Returns TRUE if the MJD difference between FRF and Telemetry is small enough.
  !
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
!print *, 'DEBUG:537: irowf=',irowf, ' ret=', is_frf_mjd_similar
      return
    end if
  end function is_frf_mjd_similar

  ! Gets the row-number of the FRF that matches the given Telemetry row
  !
  ! If not found, a negative value is returned.
  !
  ! Only the first element of frows%mjds is considered.
  ! (n.b., the elements 2-4 are valid only for Low-Bitrate, which is very rare.)
  !
  integer(ip4) function get_matched_frfrow(irowt, trows, frows) result(retrow)
  !integer(ip4) function get_matched_frfrow(mjd, frows, irowt) result(retrow)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !real(dp8), intent(in) :: mjd
    real(dp8) :: mjd
    type(asm_frfrow), dimension(:), intent(in) :: frows
    integer(ip4), intent(in), optional :: irowt ! Row-number of Telemetry (for warning info purpose only)

    real(dp8) :: diff
    integer :: irow
    character(len=256) :: msg
    logical :: tf

    mjd = trows(irowt)%mjd
    retrow = -99
!if (irowt > 95) print *,'DEBUG:429: irowt=',irowt,' mjd=',mjd
!if (irowt > 95) print *,'DEBUG:430: irowt=',irowt,' size(frows)=',size(frows),' fr(1)mjd=',frows(1)%mjds(1)
    do irow=1, size(frows)
!if (irowt > 95) print *,'DEBUG:431: irowt=',irowt,' irow=',irow,' fr(irow)mjd=',frows(irow)%mjds(1)
      if (mjd == frows(irow)%mjds(1)) then
!if (irowt > 95) print *,'DEBUG:432: FOUND: irowt=',irowt,' irow=',irow
        retrow = irow
        return
      else if (mjd < frows(irow)%mjds(1)) then
!if ((irow > 1).and.(irowt>95)) print *,'DEBUG:434: to return unfound; irow=',irow,' mjd=',mjd,' FRF-mjd(-1)=',frows(irow-1)%mjds(1)
!if (irowt > 95) print *,'DEBUG:435: to return unfound; irow=',irow,' mjd=',mjd,' FRF-mjd(__)=',frows(irow)%mjds(1)
        write(msg, '("Telemetry(row=",I5,", Frame(0-63)=", I0.2, ")")') irowt, trows(irow)%fr_6bit;
        ! MJD in the current FRF-Row is larger than the MJD compared with.
        ! If the current one is just infinitesimally different (due to
        ! the floating-point calculation issue), match it.
!if ((irow < 4).and.(irowt>95)) then 
!  tf = is_frf_mjd_similar(mjd, frows(irow)%mjds(1), irow, msg, match=.true.)
!  print *,'DEBUG:437: irow=',irow,' irowt=',irowt,' tf=', tf
!end if
        if (is_frf_mjd_similar(mjd, frows(irow)%mjds(1), irow, msg, match=.true.)) then
          retrow = irow
!print *,'DEBUG:442: irow=',irow,' irowt=',irowt,' ret-T'
          return
        end if

        if (irow < 2) return

        ! Now, check out the previous index "irow - 1". Whether it matches or not, it returns.

        ! MJD in the last FRF-Row may be extremely close to the MJD compared with.
        ! If that is the case, match it.
        if (is_frf_mjd_similar(mjd, frows(irow-1)%mjds(1), irow-1, msg, match=.true.)) then
          retrow = irow - 1
!print *,'DEBUG:443: irow-1=',irow-1,' irowt=',irowt,' ret-T2'
          return
        end if

        ! See if the last one is close by 0.001 sec (=1 ms) to the Telemetry one. (cf. 62.5 ms/frame for H-bit)
        if (is_frf_mjd_similar(mjd, frows(irow-1)%mjds(1), irow-1, msg, match=.false., factor=1.d0)) then
!print *,'DEBUG:444: irow-1=',irow-1,' irowt=',irowt,' ret-F'
          write(stderr, '("WARNING: This time difference is too large for a floating error!")')  ! Extra warning (in addition to that in is_frf_mjd_similar()).
          return
        end if

        ! Finally,
        ! see if the current one is close by 0.001 sec (=1 ms) to the Telemetry one. (62.5 ms/frame for H-bit)
        ! Note this is skipped when irow==1
        if (is_frf_mjd_similar(mjd, frows(irow)%mjds(1), irow, msg, match=.false., factor=1.d0)) then
          write(stderr, '("WARNING: This time difference is too large for a floating error!")')  ! Extra warning (in addition to that in is_frf_mjd_similar()).
print *,'DEBUG:445: irow=',irow,' irowt=',irowt,' ret-F2'
          return
        end if

        return
      end if
    end do
  end function get_matched_frfrow

  ! Gets Array of type(asm_sfrow), matching Telemetry with FRF
  !
  ! The number of the rows (aka SFs) in the returned array is based on
  ! that of the SFs in Telemetry (NOT on that of the FRF).
  !
  ! NOTE: mjds obtained from FRF are for the time in the middle of the SF.
  !   In other words, the start time of the 32nd frame (64 frames/SF).
  !
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

!print *,'DEBUG:390: get-init'
    do irowr=1, size(retrows)
      ! Gets the 32nd frame (=64/2, where 64 frames/SF), as its time is the mjds in FRF.
      irowt = get_telem_row_index_from_fr(NFRAMES_PER_SF/2+1, trows, retrows(irowr)%irowt, retrows(irowr)%nframes) ! defined in asm_fits_common

!do irowt=1, min(3, size(retrows))  !! DEBUG
      ! irowf = get_matched_frfrow(trows(irowt)%mjd, frows, irowt)
!if (irowt > 95) print *,'DEBUG:391: irowt=', irowt
      irowf = get_matched_frfrow(irowt, trows, frows)
!if ((irowt > 95) .and. irowf > 0) & !DEBUG
!  write (*,'("DEBUG:392: (FOUND!) irowf=",I3,"/",I6," for irowt=(",I4,"/",I5,")")') &
!  irowf, size(frows),irowt,size(trows) !DEBUG
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

!print *,'DEBUG:398: matching found: irowr=',irowr,' irowt=',irowt,' irowf=',irowf
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
    end do
  end function get_asm_sfrow


  ! Update asm_sfrow, checking various modes.
  !
  ! invalid flag in the sfrow may be set.
  !
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

  ! Update asm_sfrow, checking various modes.
  !
  subroutine update_asm_sfrow_mode_one(trows, sfrow)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), intent(inout) :: sfrow

    integer :: iget

    !!! This line is commented out; it continues even when there is no matching SF in FRF.
    ! if (.not. sfrow%is_valid) return  ! SF does not exist in FRF

    ! MODE_ASM
    sfrow%mode_asm  = get_val_frb(TELEM_LOC%MODE_ASM,  trows, sfrow) ! F8n+4 W66(=DP) B3: ASM Mode (ON/OFF <=> 1/0)
    
    ! MODE_SLEW
    sfrow%mode_slew = get_val_frb(TELEM_LOC%MODE_SLEW, trows, sfrow) ! F32n+10 W65(=Status) B3:  Slew369 Mode (is ON "1"? (unconfirmed)) ! Ref: Table 5.1.12, pp.209
    
    ! MODE_PHA
    sfrow%mode_PHA  = get_val_frb(TELEM_LOC%MODE_PHA,  trows, sfrow) ! F8n+4 W66(=DP) B4: ASM-PHA/Time Mode (TIME/PHA <=> 1/0)
    
    ! MODE_PHA_W56
    sfrow%mode_PHA_W56  = get_val_frb(TELEM_LOC%MODE_PHA_W56, trows, sfrow) ! F56W66B4  (should be identical to mode_PHA (=F8n+4, W66B4)
    
    ! STAT_ASM
    sfrow%stat_asm_b = get_val_frb(TELEM_LOC%STAT_ASM,  trows, sfrow) ! (int) ON/OFF for ASM F15W65B1  ! (F32n+15, W65(Status)) Table 5.1.12, pp.213

    !character(len=max_fits_char) :: stat_asa = ''; ! ON/OFF for ASM-A    F15W65B2
    !character(len=max_fits_char) :: stat_amc = ''; ! ON/OFF for ASM-AMC  F15W65B3
    !character(len=max_fits_char) :: stat_hv1 = ''; ! ENA/DIS for ASM-HV1 F15W65B4
    !character(len=max_fits_char) :: stat_hv2 = ''; ! ENA/DIS for ASM-HV2 F15W65B5

    ! STAT_RBM
    sfrow%stat_rbm_b = get_val_frb(TELEM_LOC%STAT_RBM,  trows, sfrow) ! (int) ENA/DIS for ASM-RBM F15W65B6

    ! STAT_BDR
    sfrow%stat_bdr_b = get_val_frb(TELEM_LOC%STAT_BDR,  trows, sfrow) ! (int) ENA/DIS for ASM-BDR F15W65B7

    ! BITRATE
    sfrow%bitrate    = get_val_frb(TELEM_LOC%BITRATE,   trows, sfrow) ! (int) Telemetry bit rate (F16W66)
  end subroutine update_asm_sfrow_mode_one

  ! Validate asm_sfrow, checking ASM mode etc.
  !
  ! sfrow%invalid flag in the sfrow may be set.
  !
  subroutine validate_asm_sfrow_mode_one(trows, sfrow)
    type(asm_telem_row), dimension(:), intent(in) :: trows
    type(asm_sfrow), intent(inout) :: sfrow

    if (.not. sfrow%is_valid) return  ! Likely (already) 'no matching with FRF found'

!print *, 'DEBUG:142: start does_validate'
    ! lostf  ! (should be already flagged... just in case)
    if ((sfrow%with_frf) .and. (sfrow%frf%lostf > 0)) then
      sfrow%is_valid = .false.
      sfrow%reason_invalid = get_reason_invalid('lostf', sfrow%frf%lostf) ! defined in asm_fits_common
      return
    end if

    ! MODE_ASM & STAT_ASM
!print *, 'DEBUG:149: mode_asm=', sfrow%mode_asm, ' sfrow%stat_asm=', sfrow%stat_asm, ' istat=', istat_asm 
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !if (sfrow%stat_asm_b .ne. sfrow%mode_asm) then
    !  sfrow%is_valid = .false.
    !  sfrow%reason_invalid = get_reason_invalid('asmoff') ! defined in asm_fits_common
    !  write(stderr,'(A,I4,A,I4,A,I4,A)') 'WARNING: (SF=', sfrow%sfn, ') MODE_ASM (=', sfrow%mode_asm &
    !     , ') (F4 DP B3) differs from STAT_ASM (=', sfrow%stat_asm_b, '="'//get_onoff_enadis(sfrow%stat_asm_b, 'asm')//'")'
    !end if
  end subroutine validate_asm_sfrow_mode_one

  ! Get the first value (n=0 in FXn+Y) of the specified Frame, Word, and maybe Byte
  !
  ! sfrow%invalid flag in the sfrow may be set.
  !
  ! Example: 
  !   For "F8n+1, W23", returns the value (0..255) of F1W23.
  !   For "F8n+4, W0, B0", returns 1 or 0 of F4W0B0.
  !
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
    if (irow < 0) then
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
!print *,'DEBUG:259: sfn=',sfrow%sfn,' kval=',kval,' irow=',irow
    ! Evaluate the Bit
    kval = btest2int_int4_as_1byte(kval, loc_fwb%bit) ! defined in fort_util
  end function get_val_frb

  ! Returns merged FITS header from Telemetry and FRF
  function get_merged_head(tfhead, frfhead) result(rethead)
    type(fits_header), intent(in) :: tfhead, frfhead
    type(fits_header) :: rethead

    rethead = tfhead
    rethead%FRFFILE%val = trim(frfhead%FRFFILE%val)
    rethead%kPASS%val = trim(frfhead%kPASS%val)
    rethead%TARGET1%val = trim(frfhead%TARGET1%val)
    rethead%TARGET2%val = trim(frfhead%TARGET2%val)
  end function get_merged_head

  ! Returns merged FITS header from Telemetry and FRF
  function get_asm_fits_header(tfhead, frfhead, trows, relrows, status) result(rethead)
    type(fits_header), intent(in) :: tfhead, frfhead
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !type(asm_frfrow), dimension(:), intent(in) :: frfrows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    integer, intent(out) :: status
    type(fits_header) :: rethead

    integer, dimension(1) :: ilocs
    !integer, dimension(:), allocatable :: ilocs
    real(kind=dp8) :: mjdtmp, mjd_interval
    integer :: irowt

    ! Initialization
    rethead = get_merged_head(tfhead, frfhead)

    rethead%SFRAMES%val = count(relrows%is_valid)

    rethead%TSTART%val   = UNDEF_REAL
    rethead%TEND%val     = UNDEF_REAL
    rethead%TSTARTMA%val = UNDEF_REAL
    rethead%TENDMA%val   = UNDEF_REAL
    rethead%TSTARTMS%val = UNDEF_REAL
    rethead%TENDMS%val   = UNDEF_REAL

    ! Whether the file contains the data.
    rethead%EXISTDAT%val = .false.  ! any(relrows%is_valid)

    ! Start/End times
    ilocs = findloc(relrows%is_valid, .true.)
    if (ilocs(1) == 0) return  ! No valid data

    rethead%EXISTDAT%val = .true.
    rethead%TSTART%val = sfrow2mjd(relrows(ilocs(1)), trows)  ! defined in asm_fits_common
    rethead%TSTARTMA%val = rethead%TSTART%val

    ilocs = findloc(relrows%is_valid, .true., BACK=.true.)
    mjdtmp = sfrow2mjd(relrows(ilocs(1)), trows)  ! The start time of the last valid Frame
    mjd_interval = mjdtmp - sfrow2mjd(relrows(ilocs(1)-1), trows)
    rethead%TEND%val   = mjdtmp + mjd_interval    ! The end time of the last valid Frame
    rethead%TENDMA%val = rethead%TEND%val

    ! First/Last Eulers
    !..................................................

    ! Start/End times of Slew360 mode
    ilocs = findloc(relrows%mode_slew, 1, MASK=relrows%is_valid)
    if (ilocs(1) == 0) return  ! No ASM-Slew360 data

    rethead%TSTARTMS%val = sfrow2mjd(relrows(ilocs(1)), trows)  ! defined in asm_fits_common
    ilocs = findloc(relrows%mode_slew, 1, MASK=relrows%is_valid, BACK=.true.)
    rethead%TENDMS%val   = sfrow2mjd(relrows(ilocs(1)), trows) + mjd_interval ! The end time of the last valid ASM-Mode Frame
  end function get_asm_fits_header


  !function get_fitshead(trows, sfrow) result(fitshead)
  !  type(asm_telem_row), dimension(:), intent(in) :: trows
  !  type(asm_sfrow), intent(in) :: sfrow
  !  type(fits_header), dimension(2) :: fitshead  ! (ExtensionNo), primary is 1

  !end function get_fitshead

  ! Output FITS header in a HDU of the ASM data FITS file
  subroutine write_asm_fits_header(unit, fhd, status, primary)
    implicit none
    character(len=*), parameter :: Subname = 'write_asm_fits_header'
    integer, intent(in) :: unit
    type(fits_header), intent(in) :: fhd
    integer, intent(out) :: status
    logical, intent(in), optional :: primary  ! if .true., it is for Primary header.
    logical :: is_primary

    is_primary = .false.
    if (present(primary)) then
      is_primary = primary
    end if

    !call FTPKYS(unit, 'TITLE',    fhd%title%val,    fhd%title%comment,    status)
    !call FTPKYS(unit, 'TELESCOP', fhd%telescop%val, fhd%telescop%comment, status)
    !call FTPKYS(unit, 'INSTRUME', fhd%instrume%val, fhd%instrume%comment, status)
    !call FTPKYS(unit, 'SACD',    fhd%sacd%val,    fhd%sacd%comment,    status)
    !call ftpkys(unit, 'FILENAME', fhd%filename%val, fhd%filename%comment, status) ! asm_comm defined in asm_fits_common
    call FTPKYS(unit, fhd%TELESCOP%name, fhd%TELESCOP%val, fhd%TELESCOP%comment, status)
    call warn_ftpcl_status(status, 'ftpkys', trim(Subname)//':TELESCOP')
    call FTPKYJ(unit, fhd%SACD%name, fhd%SACD%val, fhd%SACD%comment, status)
    call warn_ftpcl_status(status, 'ftpkyj', trim(Subname)//':SACD')
    call FTPKYS(unit, fhd%INSTRUME%name, fhd%INSTRUME%val, fhd%INSTRUME%comment, status)
    call FTPKYS(unit, fhd%FILENAME%name, fhd%FILENAME%val, fhd%FILENAME%comment, status)
    call FTPKYS(unit, fhd%FRFFILE%name, fhd%FRFFILE%val, fhd%FRFFILE%comment, status)
    call FTPKYS(unit, fhd%kPASS%name, fhd%kPASS%val, fhd%kPASS%comment, status)

    if (is_primary) return

    call FTPKYL(unit, fhd%EXISTDAT%name, fhd%EXISTDAT%val, fhd%EXISTDAT%comment, status)
    call warn_ftpcl_status(status, 'ftpkyl', trim(Subname)//':EXISTDAT')

    call FTPKYJ(unit, fhd%SFRAMES%name, fhd%SFRAMES%val, fhd%SFRAMES%comment, status)
    call FTPKYD(unit, fhd%TSTART%name,   fhd%TSTART%val,   14, fhd%TSTART%comment, status)
    call FTPKYD(unit, fhd%TEND%name,     fhd%TEND%val,     14, fhd%TEND%comment, status)
    call FTPKYD(unit, fhd%TSTARTMA%name, fhd%TSTARTMA%val, 14, fhd%TSTARTMA%comment, status)
    call FTPKYD(unit, fhd%TENDMA%name,   fhd%TENDMA%val,   14, fhd%TENDMA%comment, status)
    call FTPKYD(unit, fhd%TSTARTMS%name, fhd%TSTARTMS%val, 14, fhd%TSTARTMS%comment, status)
    call FTPKYD(unit, fhd%TENDMS%name,   fhd%TENDMS%val,   14, fhd%TENDMS%comment, status)

    call FTPCOM(unit, OUTFTCOMMENT1, status)  ! defined in asm_fits_common
    !call FTPHIS(unit, HISTORY1, status)  ! defined in asm_fits_common
    
    !call FTPKYS(unit, 'TITLE',    fitshead%title,    asm_comm%title,    status)
    !!call FTPKY[JKLS](funit, 'date', '????', '[day] Creation date of this file', status)
    !!call FTPKY[EDFG](unit,keyword,keyval,decimals,comment, > status)  ! decimals < 0 => G format, -4 means 4 digits below 0
    ! .........
  end subroutine write_asm_fits_header

  !! Output FITS table in writing the ASM data FITS file
  !subroutine write_asm_fits_table(unit, tables, status)
  !  implicit none
  !  type(asm_table_spec), parameter :: tspec = asm_table_spec()

  !  integer, intent(in) :: unit
  !  type(asm_table_col), intent(in) :: tables
  !  integer, intent(out) :: status

  !  integer :: colnum

  !  !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
  !  !! Write elements into an ASCII or binary table column. The ‘felem’ parameter applies only to vector columns in binary tables and is ignored when writing to ASCII tables.
  !  
  !  !colnum = tspec%i%tstart
  !  !!call FTPCLD(unit, colnum, 1, 1, 2, dvalues, status)  ! colnum = 1
  !  !!call ftpkys(unit, 'FILENAME', fitshead%filename, asm_comm%filename, status) ! asm_comm defined in asm_fits_common
  !  !! .........
  !end subroutine write_asm_fits_table

  ! Test output for debugging.
  subroutine write_tmp_fits(fname, status)
    implicit none
    character(len=*), intent(in) :: fname
    integer, intent(out) :: status
    integer :: unit, bitpix, naxis = 0
    integer :: blocksize, naxes(0)
    integer :: group,fpixel,nelements,array(300,200) ! i,j,
    logical :: simple =.true., extend = .true.
    integer :: nhdu
    character(len=30) :: errtext
    call ftgiou(unit, status)
if (IS_DEBUG()) print *,'DEBUG:041:giou, unit=', unit, ' status=',status
if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) unit = 160
call ftinit(unit, '/tmp/out.fits', blocksize, status)
if (IS_DEBUG()) print *,'DEBUG:042:b3ftart-out, status=',status
    call ftclos(unit, status)
    call ftfiou(unit, status)
  end subroutine write_tmp_fits

  ! Warn if failing in writing a column
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

  ! Output FITS file of the ASM data
  subroutine write_cols(unit, trows, relrows, colheads, status)
    implicit none
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
    real(dp8), dimension(:), allocatable :: cold
    integer, dimension(:), allocatable :: colj
    integer(kind=2), dimension(:), allocatable :: coli
    character(len=8), dimension(:), allocatable :: cols8

    naxis2 = sum(relrows%nframes, relrows%is_valid)  ! Number of valid frames.
    ntrows   = size(trows)
    nrelrows = size(relrows)

    allocate(coli(naxis2))
    allocate(colj(naxis2))
    allocate(cold(naxis2))
    allocate(cols8(naxis2))

if (IS_DEBUG()) then ! in asm_fits_common
print *,'DEBUG:2433'
call dump_type(relrows(3), 3)

print *,'DEBUG:2439-6:'
call dump_type(colheads(6))
print *,'DEBUG:2439-7:'
call dump_type(colheads(7))
print *,'DEBUG:2439-8:'
call dump_type(colheads(8))
end if
!FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
    status = 0 
    iframe = 0 
    ittype = 0  ! TTYPEn
    iasm = 0    ! The i-th number of the main ASM data (should be repeated NWORDS_MAIN=96 times)
    ! do ikind=1, size(COL_FORM_UNITS)
    do ikind=1, size(colheads)
!do ikind=2, 4  ! Index in COL_FORM_UNITS
      iout = 0  ! Row number of each output Column
      iend = 0
!ckey = COL_FORM_UNITS(ikind)%key
      ckey = colheads(ikind)%key 
if (IS_DEBUG()) then ! in asm_fits_common
if (trim(ckey) .ne. 'main') then
print *,'DEBUG:2010:ikind=',ikind, ' ckey=', trim(ckey)
end if
end if
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
if (status .ne. 0) then !DEBUG
  print *,'DEBUG:2042: iasm=',iasm,' ittype=',ittype
end if
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
if (IS_DEBUG()) print *,'DEBUG:2310:ikind=',ikind, ' ittype=', ittype,' nelements=naxis2=',naxis2,' size(cold)=',size(cold)
        call FTPCLD(unit, ittype, 1, 1, naxis2, cold, status)  ! colnum = 1
        call warn_ftpcl_status(status, 'FTPCLD', ckey)
        call modify_ttype_comment(unit, ittype, ckey, status)

if (IS_DEBUG()) print *,'DEBUG:234:fds=',cold(5:8)

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
            ! cold(iout:iend) = frfrows(relrows(irelrow)%irowf)%eulers(iprm,1)  ! Same value for the entire SF
            cold(iout:iend) = relrows(irelrow)%frf%eulers(iprm,1)  ! Same value for the entire SF
if (IS_DEBUG()) then ! in asm_fits_common
if ((irelrow > 2) .and. (irelrow < 5)) then
 !print *,'DEBUG:429:iprm=',iprm,' irelrow=',irelrow,' irowf=',relrows(irelrow)%irowf,' iout=', iout, ' eulers=', &
    !frfrows(relrows(irelrow)%irowf)%eulers(:,1), ' This=',frfrows(relrows(irelrow)%irowf)%eulers(iprm,1),  &
  print *,'DEBUG:429:iprm=',iprm,' irelrow=',irelrow,' irowf=',relrows(irelrow)%irowf,' iout=', iout, ' eulers=', &
     relrows(irelrow)%frf%eulers(:,1), ' This=',relrows(irelrow)%frf%eulers(iprm,1),  &
     ' cold=',cold(iout)
end if
end if
          end do
          ittype = ittype + 1  ! FITS Table column number
if (IS_DEBUG()) print *,'DEBUG:2410:ikind=',ikind, ' ittype=', ittype , ' iprm=',iprm
          call FTPCLD(unit, ittype, 1, 1, naxis2, cold, status)  ! colnum = 1
! print *,'DEBUG:430:ittype=',ittype,' iout=',iout,' naxis2=',naxis2,' cold(3)=',cold(66)
          call warn_ftpcl_status(status, 'FTPCLD', ckey)
          call modify_ttype_comment(unit, ittype, ckey, status)

!print *,'DEBUG:334:euler',iprm,'=',cold(63:65)
        !end do
!print *,'DEBUG:332:irowf(2)=',relrows(2)%irowf

      case('SFNum', 'SFNTelem')
        ! Integer*4 columns based on the SF
        do iprm=1, 1
          colj(:) = 0
          iout = 0
          iend = 0
if (IS_DEBUG()) print *,'DEBUG:2510:nrelrows=',nrelrows, ' ittype=', ittype 
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
              !colj(iout:iend) = frfrows(relrows(irelrow)%irowf)%eulers(iprm, 1)
              colj(iout:iend) = relrows(irelrow)%frf%sfn
if (IS_DEBUG()) then ! in asm_fits_common
if (irelrow == 3) then
print *,'DEBUG:2533:in the loop, iout=',iout,' iend=',iend
call dump_type(relrows(3), 3)
print *,'DEBUG:2534:in the loop'
call dump_type(relrows(3)%frf)
end if
end if
            case('SFNTelem')
              colj(iout:iend) = relrows(irelrow)%SFNTelem
            case default
              call err_exit_play_safe(trim(ckey)) ! never happens
            end select
          end do
          ittype = ittype + 1  ! FITS Table column number
if (IS_DEBUG()) print *,'DEBUG:2540:FTPCLJ:nrelrows=',nrelrows, ' ittype=', ittype 
          call FTPCLJ(unit, ittype, 1, 1, naxis2, colj, status)  ! colnum = 1
          call warn_ftpcl_status(status, 'FTPCLJ', ckey)
          call modify_ttype_comment(unit, ittype, ckey, status)
          ! call FTSNUL(unit,colnum,snull > status) ! Define string representation for NULL column
          ! call FTTNUL(unit,colnum,tnull > status) ! Define the integer(!) value to be treated as NULL

if (IS_DEBUG()) print *,'DEBUG:364:sfn',iprm,'=',colj(63:65)
        end do
if (IS_DEBUG()) call dump_type(relrows(5))

      case('SF2bits', 'Mode_ASM', 'Mode_PHA', 'ModeSlew', 'bitrate')  ! 'Fr6bits', 'i_frame', are Frame-based 
        ! Integer*2 columns
        do iprm=1, 1
          coli(:) = 0
!coli(:) = -2
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

      case('Fr6bits', 'FrameNum', 'Status_C', 'DP_C')    
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
            case('FrameNum')
              coli(iout) = int(trows(itrow)%FrameNum, kind=2)
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
!print *,'DEBUG:2340:ikind=',ikind, ' ittype=', ittype,' nelements=naxis2=',naxis2,' size(coli)=',size(coli)
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
!print *,'DEBUG:2340:ikind=',ikind, ' ittype=', ittype,' nelements=naxis2=',naxis2,' size(cols8)=',size(cols8)
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

!      case('ASM_C1', 'ASM_C2')   
!ittype = ittype + 1
      case default
        call err_exit_with_msg('Parameter '//trim(ckey)//' is not yet taken into account.')
      end select
    end do
    if (allocated(coli)) deallocate(coli)
    if (allocated(colj)) deallocate(colj)
    if (allocated(cold)) deallocate(cold)
  end subroutine write_cols

  ! Output FITS file of the ASM data
  subroutine write_asm_evt_fits(outfil, fhead, trows, relrows, status, outcolkeys)
    implicit none
    integer, parameter :: MY_FUNIT = 159  ! arbitrary
    character(len=*), parameter :: Subname = 'write_asm_evt_fits'
    character(len=*), parameter :: Extname = 'ASM table'

    character(len=*), intent(in) :: outfil
    type(fits_header), intent(inout) :: fhead  ! Mainly for 1st-Extension header. EXISTDAT is written.
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !type(asm_frfrow), dimension(:), intent(in) :: frfrows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    integer, intent(out) :: status
    character(len=*), dimension(:), intent(in), optional :: outcolkeys  ! e.g., ['Tstart', 'Euler', SFNum]

    integer :: unit, bitpix, blocksize !, naxis, hdutype, nframes, naxis1
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

if (IS_DEBUG()) then ! in asm_fits_common
call dump_type(fhead)  ! for DEBUG
end if

!!!!!!!!!!!!!!!!!!!!!!!
!! TODO
!! colheads = get_colheads(['Tstart', 'Euler', SFNum])
!! get_nframes_colheads(colheads)  ! interface of chars or t_asm_colhead
!!!!!!!!!!!!!!!!!!!!!!!

    if (present(outcolkeys)) then
if (IS_DEBUG()) call dump_chars(outcolkeys, 'DEBUG:143: outcolkeys=')
      colheads = get_colheads(outcolkeys)
if (IS_DEBUG()) print *,'DEBUG:0095: size(colheads)=',size(colheads)
    else
      colheads = get_colheads()  ! Column Header info
                                 ! %(key, type, prm%(form, unit, comm, dim))
    end if
if (IS_DEBUG()) then ! in asm_fits_common
if (size(colheads) > 94) then
call dump_type(colheads(95))  ! for DEBUG
call dump_type(colheads(97))  ! for DEBUG
call dump_type(colheads(98))  ! for DEBUG
end if
end if
!do i=99,size(colheads)  ! for DEBUG
!  print *,'i=',i        ! for DEBUG
!  call dump_asm_colhead(colheads(i))  ! for DEBUG
!end do                  ! for DEBUG

    ! Get an unused Logical Unit Number to use to create the FITS file
    call ftgiou(unit,status)
    if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) then
      write(stderr,'("WARNING: Failed in ftgiou(): unit = ",I12,". Manually reset to ",I3)') unit, MY_FUNIT
      unit = 159
    end if

    ! create the new empty FITS file blocksize=1
    !call ftinit(unit,filename,blocksize,status)
    call ftinit(unit,'!'//outfil,blocksize,status)
    if (status .ne. 0) then
      call FTGERR(status, errtext)
      write(stderr,'("ERROR: Failed in ftinit() with status=", I12,": ",A)') status, trim(errtext)
    end if

    call FTGERR(status, errtext)  ! for Debugging
    call FTGHDN(unit, nhdu)  ! CHDU: Current HDU
if (IS_DEBUG()) print *,'DEBUG:0010: test-open1-status=',status,' / HDU=',nhdu,' / ',trim(errtext)

    ! initialize parameters about the FITS image (300 x 200 16-bit integers)
    simple=.true.
    bitpix=16  ! signed 2-byte, 8: unsigned 1-byte, -32: real, -64: double
    extend=.true.
    ! write the required (Primary) header keywords
    call ftphpr(unit, simple, bitpix, 0, naxes, 0, 1, extend, status) ! Because naxis=0, naxes is ignored.
    ! write other optional keywords to the header
    call write_asm_fits_header(unit, fhead, status, primary=.true.)
!call ftpkyj(unit,'EXPOSURE',1500,'Total Exposure Time',status)  ! DEBUG
    
    ! Extension
    
!ttypes = colheads(97:101)%type
!tforms = colheads(97:101)%prm%form
!tunits = colheads(97:101)%prm%unit
    ttypes = colheads%type
    tforms = colheads%prm%form
    tunits = colheads%prm%unit
    !call FTIBIN(unit,nrows,tfields,ttype,tform,tunit,Extname,varidat > status) ! nrows should be 0 ! FiTs-Insert-BINary-table
    !call FTIBIN(unit,0,2,ttypes,tforms,tunits,'TestBinExt',.true., status) ! Creates an extension with basic header and moves to it.
    !call FTIBIN(unit, 0, size(ttypes) &
if (IS_DEBUG()) print *,'DEBUG:0030: size(tforms)=TFIELDS=',size(tforms)
    call FTIBIN(unit, 0, size(tforms) &
              , ttypes, tforms, tunits, Extname, .true., status) ! Creates an extension with basic header and moves to it.
    call warn_ftpcl_status(status, 'FTIBIN', Subname)
if (IS_DEBUG()) then ! in asm_fits_common
if (size(tforms) > 96) then
print *,'DEBUG:0032:tforms(97)=',trim(tforms(97))
end if
end if

call FTGHDN(unit, nhdu)
call FTGERR(status, errtext)
if (IS_DEBUG()) print *,'test-new-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)

    call write_asm_fits_header(unit, fhead, status, primary=.false.)
    !fhead%EXISTDAT%val = any(relrows%is_valid)

    call write_cols(unit, trows, relrows, colheads, status)  !!!!!!!!!!!!!!!

!    ! Write Table (double precision)
!    !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
!    call FTPCLD(unit,1,1,1,2,dvalues, status)  ! colnum = 1
!    call FTGHDN(unit, nhdu)  ! => CHDU: Current HDU
!    call FTGERR(status, errtext)
!print *,'test-dval-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)
!    
!    !call ftpkys(unit,'TDIM2','(20,1)','for Character in Binary table',status)
!    !call FTPCLS(unit,2,1,1,2,svalues, status)  ! colnum = 2
!    call FTPCLD(unit,2,1,1,2,d2values, status)
!    call FTGHDN(unit, nhdu)
!    call FTGERR(status, errtext)
!print *,'test-char-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)  ! If wrong, 309  / not an ASCII (A) column
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
if (IS_DEBUG()) print *,'DEBUG: test-close-status=',status,' / ',trim(errtext)
    call ftfiou(unit, status)

    if (allocated(ttypes)) deallocate(ttypes)
    if (allocated(tforms)) deallocate(tforms)
    if (allocated(tunits)) deallocate(tunits)
  end subroutine write_asm_evt_fits

  ! Output FITS file of the ASM data
  subroutine write_asm_fits(fname, fitshead, trows, frfrows, relrows, status)
    implicit none
    character(len=*), parameter :: Extname = 'ASM table'

    character(len=*), intent(in) :: fname
    type(fits_header), intent(in) :: fitshead  ! Mainly for 1st-Extension header
    type(asm_telem_row), dimension(:), intent(in) :: trows
    !type(asm_table_col), dimension(:, :), intent(in) :: tables ! (Row)
    type(asm_frfrow), dimension(:), intent(in) :: frfrows
    type(asm_sfrow), dimension(:), intent(in) :: relrows
    integer, intent(out) :: status

    integer :: unit, bitpix, naxis = 2
    integer :: blocksize, naxes(2)
    integer :: group,fpixel,nelements,array(300,200) ! i,j,
    logical :: simple =.true., extend = .true.
    integer :: nhdu
    character(len=80), dimension(2) :: ttypes = (/ 'TIME', 'Char' /), tunits, &
         tforms = (/ '1D ', '20A' /)  ! 20A20?
         !tforms = (/ '1D   ', '20A20' /)  ! 20A20?
         !tforms = (/ '1D   ', '1D   ' /)
    real(kind=dp8), dimension(2) :: dvalues = (/ 12345.6, 7.89 /), d2values = (/ 0.023, 0.0045 /)
    character(len=20), dimension(2) :: svalues = (/ 'saisho', 'tsugi1' /)
    character(len=80) :: comment, keyword, snull, msg
    character(len=30) :: errtext
    type(t_asm_colhead), dimension(:), allocatable :: colheads

    bitpix=16  ! signed 2-byte, -32: real, -64: double
    naxis=2

if (IS_DEBUG()) print *,'DEBUG:125:start-out'

    ! Get an unused Logical Unit Number to use to create the FITS file
    call ftgiou(unit, status)
    if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) then
      write(stderr,'("WARNING: Failed in ftgiou(): unit = ",I12,". Manually reset to 159.")') unit
      unit = 159
    end if

if (IS_DEBUG()) print *,'DEBUG:140:aft-giou;unit=',unit, ' file=',trim(fname)

    ! create the new empty FITS file blocksize=1
call ftinit(unit, '!/tmp/out.fits', blocksize, status)
    !call ftinit(unit, '!'//trim(fname), blocksize, status)
if (IS_DEBUG()) print *,'DEBUG:142:b3ftart-out, status=',status
    if (status .ne. 0) then
      call FTGERR(status, errtext)
      write(stderr,'("ERROR: Failed in ftinit() with status=", I12,": ",A)') status, trim(errtext)
    end if
    call FTGHDN(unit, nhdu)  ! CHDU: Current HDU
if (IS_DEBUG()) print *,'DEBUG:write-open1-status=',status,' / HDU=',nhdu,' / ',trim(errtext)
    call ftphpr(unit,simple,bitpix,0,naxes,0,1,extend,status)
    !call ftpkyj(unit,'EXPOSURE',1500,'Total Exposure Time',status)

    call write_asm_fits_header(unit, fitshead, status, primary=.true.)

    colheads = get_colheads()  ! Column Header info
                               ! %(key, type, prm%(form, unit, comm, dim))

    !!! Extension
    
    !ttypes = (/ 'TIME', 'String' /)
    !tforms = (/ '1D', '20A' /)  ! Double
    !tunits = (/ 'sec', '' /)
    tunits(1) = 'sec'
    tunits(2) = ''

    !call FTIBIN(unit,nrows,tfields,ttype,tform,tunit,Extname,varidat > status) ! nrows should be 0 ! FiTs-Insert-BINary-table
    call FTIBIN(unit, 0, size(ttypes) &
              , ttypes, tforms, tunits, Extname, .true., status) ! Creates an extension with basic header and moves to it.
    ! Status check...
    call FTGHDN(unit, nhdu)
    call FTGERR(status, errtext)
    if (status .ne. 0) then
      write(stderr,'("ERROR: Failed to create a table: status=",i4," / HDU=",i4,a)') status,nhdu,' / '//trim(errtext)
    end if

    call write_asm_fits_header(unit, fitshead, status)

    ! Write Table (double precision)
    !FTPCL[SLBIJKEDCM](unit,colnum,frow,felem,nelements,values, > status) ! frow: 1st row?, felem: 1st element?
    call FTPCLD(unit,1,1,1,2,dvalues, status)  ! colnum = 1
    call FTGHDN(unit, nhdu)
    call FTGERR(status, errtext)
if (IS_DEBUG()) print *,'DEBUG: dval-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)
    
    call ftpkys(unit,'TDIM2','(20,1)','for Character in Binary table',status)
    call FTPCLS(unit,2,1,1,2,svalues, status)  ! colnum = 2
    !call FTPCLD(unit,2,1,1,2,d2values, status)
    call FTGHDN(unit, nhdu)
    call FTGERR(status, errtext)
if (IS_DEBUG()) print *,'DEBUG: char-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)  ! If wrong, 309  / not an ASCII (A) column

    call ftclos(unit, status)
    call ftfiou(unit, status)
  end subroutine write_asm_fits


!  ! SF-based old style
!  subroutine out_asm_fits_old(fname, fitshead, ardata, nrows)
!    character(len=*), intent(in) :: fname
!    type(asm_header), dimension(:), intent(in) :: fitshead  ! (iSabuFrame), None for primary header
!    type(asm_telem_row),  dimension(:, :), intent(in) :: ardata ! (iSabuFrame, Row)
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
!    !call get_ttypes(ttypes)
!    !call get_tforms(tforms)
!    !call get_tunits(tunits)
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
!      do j=1, NUM_INSTR*NCHANS_PHA  ! ==96
!        aryi(:) = 0
!        do i=1, nrows(isabu)
!          aryi(i) = ardata(isabu, i)%asmdats(j)
!        end do
!        call FTPCLE(funit, 1, ini+3*2+j, 1, 1, nrows(isabu), aryi, status)
!      end do
!print *, 'DEBUG48: '
!    end do
!
!    call ftclos(funit, status)
!    call ftfiou(funit, status)
!  end subroutine out_asm_fits_old
end module asm_fitsout

