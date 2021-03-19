! Routines to read a Telemetry FITS file
!
module asm_read_telemetry
  use err_exit
  use fort_util
  use asm_fits_common
  use asm_aux

  implicit none 

  integer, parameter, private :: MAXAXES = 2
  integer, parameter :: NBYTESFORHEADER = 16
contains

  integer function get_safe_unit(defunit) result(funit)
    integer, intent(in), optional :: defunit
    integer :: status=-999
    character(len=1024) :: errmsg
    integer :: i, mydefunit

    if (present(defunit)) then
      mydefunit = defunit
    else
      mydefunit = 156
    end if

    funit = 0
    do i=1, 100
      call ftgiou(funit, status)
      if (status == 0) then
        exit
      else
        ! It seems status is never zero... But if I just accept it, it does not seem to cause a problem.

        ! write(stderr,'("Failed to get a safe file unit: iteration=", i3)') i
        ! funit = mydefunit
      end if
    end do

    if (funit == mydefunit) then
      write(stderr,'("Failed to get a safe file unit: Default is chosen: ", i6)') funit
    else if (funit == 0) then
      errmsg = 'Failed to get a safe file unit for Telemetry'
      call err_exit_if_status(status, errmsg) ! defined in err_exit
      stop  ! redundant
    end if
  end function get_safe_unit

  ! Make and get fits_header and asm_telem_row of the telemetry from File
  !
  ! Returns an Array of the Telemetry object and corresponding FITS-header object.
  !
  ! Essentially, a wrapper of
  !   read_telemetry()
  !   get_telem_raws2types()
  !   add_mjd2telem()
  subroutine mk_telem_rows(fname, tfhead, telm_rows)
    character(*), intent(in) :: fname  ! Telemetry filename
    type(fits_header), intent(out) :: tfhead
    type(asm_telem_row), dimension(:), allocatable, intent(out) :: telm_rows

    integer(kind=1), dimension(:, :), allocatable :: headers, telems ! (word(=byte), row)

    call read_telemetry(trim(fname), tfhead, headers, telems)
    telm_rows = get_telem_raws2types(headers, telems)
    call add_mjd2telem(tfhead, telm_rows)
    deallocate(headers)
    deallocate(telems)
  end subroutine mk_telem_rows


  ! Make and get FRF fits_header and asm_frfrow from a FRF File
  !
  ! Returns an Array of the FRF object and corresponding FITS-header object.
  !
  ! Essentially, a wrapper.
  !
  subroutine mk_frf_rows(fname, frfhead, frfrows)
    character(*), intent(in) :: fname  ! FRF filename
    type(fits_header), intent(out) :: frfhead
    type(asm_frfrow), dimension(:), allocatable, intent(out) :: frfrows

    frfrows = get_frf_types(fname)
    frfhead = get_frf_head(fname)
  end subroutine mk_frf_rows


  ! Returns FRF FITS-header object
  function get_frf_head(fname) result(frfhead)
    integer, parameter :: MY_FUNIT = 157  ! arbitrary
    character(*), intent(in) :: fname  ! FRF filename
    type(fits_header) :: frfhead

    integer :: funit, status=-999, blocksize, hdutype
    integer :: nrows = -99

    funit = get_safe_unit(defunit=MY_FUNIT)

    ! Open and move to the 1st extension
    call ftopen(funit, fname, 0, blocksize, status)  ! 0: readonly
    call err_exit_if_status(status, 'Failed to open the FITS: '//trim(fname))
    call FTMAHD(funit, 2, hdutype, status)           ! Move to the 1st extention
    call err_exit_if_status(status, 'Failed to move to the 1st extension: '//trim(fname))

    frfhead%FRFFILE%val  = trim(basename(fname))

    !------ Read header keywords

    ! FiTs_Get_Number_of_RoWs
    call FTGNRW(funit, nrows, status) ! cf. FTGNRWLL() for INT*64
    frfhead%NAXIS2%val = nrows 

    ! Get some other header data
    ! Type: String
    call ftgkys(funit, frfhead%EXTNAME%name,  frfhead%EXTNAME%val,  frfhead%EXTNAME%comment,  status)
    call ftgkys(funit, frfhead%kPASS%name,    frfhead%kPASS%val,    frfhead%kPASS%comment,    status)
    call ftgkys(funit, frfhead%TARGET1%name,  frfhead%TARGET1%val,  frfhead%TARGET1%comment,  status)
    call ftgkys(funit, frfhead%TARGET2%name,  frfhead%TARGET2%val,  frfhead%TARGET2%comment,  status)
    ! Type: Integer
    call ftgkyj(funit, frfhead%R_S_FLAG%name, frfhead%R_S_FLAG%val, frfhead%R_S_FLAG%comment, status)
    call ftgkyj(funit, frfhead%TOTAL_SF%name, frfhead%TOTAL_SF%val, frfhead%TOTAL_SF%comment, status)
    frfhead%TOTSFFRF%val = frfhead%TOTAL_SF%val

    call ftclos(funit, status)
    ! call FTGERR(status, errtext)
    call ftfiou(funit, status)
  end function get_frf_head


  ! Read telemetry file and RETURN headers, telems
  !
  ! Description:
  !   Return 2 allocated arrays (headers and telems), as well as an object fhead
  !   headers is an Array of  16 words(=bytes) as  (16 Integer*1): (word, row).
  !   telems  is an Array of 128 words(=bytes) as (128 Integer*1): (word, row).
  !   Note that their values above 127 do NOT represent the correct number when
  !   interpreted as Fortran Integer(kind=1);
  !   e.g., the byte value 255 (0b11111111) appears to be -1 as opposed to 255.
  !   Use fort_util::unsigned1_to_int4() to convert it (scalar or Array)
  !   into a standard Integer(kind=4) (scalar/Array).
  subroutine read_telemetry(fname, fhead, headers, telems)
    integer, parameter :: MY_FUNIT = 156  ! arbitrary
    character(*), intent(in) :: fname  ! Telemetry filename
    type(fits_header), intent(out) :: fhead
    integer(kind=1), dimension(:, :), allocatable, intent(out) :: headers, telems ! (word(=byte), row)

    integer(kind=1), dimension(:), allocatable :: ardata  ! Temporary array
    integer :: funit, status=-999, blocksize, hdutype
    integer :: nrows = -99
    integer :: datacode, repeat, num_axis, width
    integer(kind=4) :: nbytes
    integer, dimension(MAXAXES) :: naxes
    character(len=1024) :: errmsg
    character(len=80) :: comment, snull, msg
    integer :: i, j, nc

    funit = get_safe_unit(defunit=MY_FUNIT)

    ! Open and move to the 1st extension
    call ftopen(funit, fname, 0, blocksize, status)  ! 0: readonly
    call err_exit_if_status(status, 'Failed to open the FITS: '//trim(fname))
!WRITE (*,'("DEBUG: blocksize=", i0)') blocksize
    call FTMAHD(funit, 2, hdutype, status)           ! Move to the 1st extention
!WRITE (*,'("DEBUG: hdutype=",   i0)') hdutype
    call err_exit_if_status(status, 'Failed to move to the 1st extension: '//trim(fname))

    !------ Read header keywords

    call ftgkyj(funit, fhead%FRAMES%name, fhead%FRAMES%val, fhead%FRAMES%comment, status)  ! "ftgkyj" for Integer
!WRITE (*,'("DEBUG: Number of frames=", i6, " / ", a)') fhead%FRAMES%val, trim(fhead%FRAMES%comment)

    call ftgkys(funit, fhead%DATE__OBS%name, fhead%DATE__OBS%val, fhead%DATE__OBS%comment, status)
    call err_exit_if_status(status, 'Failed to get DATE-OBS in '//trim(fname))
!WRITE (*,*) 'DEBUG: DATE-OBS: ' // trim(fhead%DATE__OBS%val)

    ! FiTs_Get_Number_of_RoWs
    call FTGNRW(funit, nrows, status) ! cf. FTGNRWLL() for INT*64
    if ((status .ne. 0) .or. (nrows .ne. fhead%FRAMES%val)) then
      msg = 'Inconsistent FRAMES and NAXIS2: ('
      write(errmsg, '(a, i6, ", ", i6, ")")') trim(msg), fhead%FRAMES%val, nrows
      call err_exit_with_msg(errmsg)
      stop  ! redundant
    end if
    fhead%NAXIS2%val = nrows 

    ! FiTs_Get_Number_of_CoLumns
    call FTGNCL(funit, nc, status) ! read TFIELDS
    if ((status .ne. 0) .or. (nc .ne. 1)) then
      write(errmsg, '("TFIELDS (number of columns) is not 1 but ", i1)') nc
      call err_exit_with_msg(errmsg)
      stop  ! redundant
    end if

    ! FiTs_GeT_CoLumn (Get the datatype of a column): FTGTCL(unit,colnum, > datacode,repeat,width,status)
    call FTGTCL(funit, 1, datacode, repeat, width, status)
    if ((datacode .ne. 11) .or. (repeat .ne. nbytespercard)) then  ! namely, if TFORM1 != '144B'
      errmsg = 'The format (TFORM1) is not 144B.'
      call err_exit_with_msg(errmsg)
      stop  ! redundant
    end if
      
    ! Get some other header data
    ! Type: String
    call ftgkys(funit, fhead%EXTNAME%name,  fhead%EXTNAME%val,  fhead%EXTNAME%comment,  status)
    call ftgkys(funit, fhead%TELESCOP%name, fhead%TELESCOP%val, fhead%TELESCOP%comment, status)
    ! Type: Integer
    call ftgkyj(funit, fhead%SACD%name,     fhead%SACD%val,     fhead%SACD%comment,     status)
    call ftgkyj(funit, fhead%D_STA_C1%name, fhead%D_STA_C1%val, fhead%D_STA_C1%comment, status)
    call ftgkyj(funit, fhead%D_STA_C2%name, fhead%D_STA_C2%val, fhead%D_STA_C2%comment, status)
    call ftgkyj(funit, fhead%D_END_C1%name, fhead%D_END_C1%val, fhead%D_END_C1%comment, status)
    call ftgkyj(funit, fhead%D_END_C2%name, fhead%D_END_C2%val, fhead%D_END_C2%comment, status)

    fhead%INSTRUME%val = 'ASM'
    fhead%FILENAME%val = trim(basename(fname))

if (IS_DEBUG()) then ! in asm_fits_common
print *,'DEBUG:9232: fhead:'
call dump_type(fhead, 1) !! DEBUG
end if
    naxes = (/ (-999, i = 1, size(naxes)) /)
    ! FiTs_Get_Table_DiMension
    call FTGTDM(funit, 1, maxaxes, num_axis, naxes, status)
    call err_exit_if_status(status, 'Failed to get Table dimension')

    !!! Comment: num_axis is NOT 2 but 1: naxes == (144, UNDEFINED)
    !if (num_axis .ne. 2) then 
    !  print *, 'nrows = nfields = ', nrows
    !  print *, 'naxes = ', naxes
    !  write(errmsg, '("NAXIS is not 2 but ", i8)') num_axis
    !  call err_exit_with_msg(errmsg)
    !  stop  ! redundant
    !end if

    nbytes = naxes(1)*nrows
    allocate(ardata(nbytes), STAT=status)
    if (status /= 0) STOP "*** Not enough memory ***"

    ! FTGTBB(unit,frow,startchar,nchars, > array,status)
    call FTGTBB(funit, 1, 1, nbytes, ardata, status)
    call err_exit_if_status(status, 'Failed to get Table dimension')

    allocate(headers(nbytesforheader, nrows), STAT=status)
    allocate(telems( nbytespercard  , nrows), STAT=status)

    !print *, 'DEBUG(01): nrows=nfields=', nrows
    do i=1, nrows ! 144 per card (header + telemetry-block)
      j = (i-1)*nbytespercard + 1
      if (i .gt. 12030) then
    !   print *, 'DEBUG(02): i,j= ', i, j
      end if

      headers(:, i) = ardata(j:j+15)     ! Timestamp info etc
      telems( :, i) = ardata(j+16:j+143) ! Main telemetry data
    end do
    !print *, 'DEBUG(03): after-loop '
    if (allocated(ardata)) deallocate(ardata, STAT=status)

    call ftclos(funit, status)
    call ftfiou(funit, status)

  end subroutine read_telemetry

  ! Raw telemetry arrays to an Array of type(asm_telem_row)
  !
  ! Description:
  !   headers are an Array of  16 words(=bytes) as  (16 Integer*1): (word, row).
  !   telems  are an Array of 128 words(=bytes) as (128 Integer*1): (word, row).
  !   Note that their values as Integers do NOT represent the number above 127;
  !   e.g., the byte value 255 (0b11111111) is treated as -1 as opposed to 255.
  !   Use fort_util::unsigned1_to_int4() to convert it (scalar or Array)
  !   into a standard Integeri (scalar/Array).
  ! 
  !   Here it is converted into an Array of type(asm_telem_row) with INTEGER*4 
  function get_telem_raws2types(headers, telems) result(retrows)
    integer(kind=1), dimension(:, :), allocatable, intent(in) :: headers, telems ! (word(=byte), row)
    type(asm_telem_row), dimension(size(headers, 2)) :: retrows
    
    integer :: irow, irow16, iasm, itel, i, j, k
    integer(kind=1) :: sf4, fr64
    character(len=8) :: s8

    do irow=1, size(headers, 2) !unsigned1_to_int4() defined in fort_util
      retrows(irow)%month      = unsigned1_to_int4(headers(1, irow)) ! Byte00
      retrows(irow)%day        = unsigned1_to_int4(headers(2, irow)) ! Byte01 
      retrows(irow)%hour       = unsigned1_to_int4(headers(3, irow)) ! Byte02 
      retrows(irow)%minute     = unsigned1_to_int4(headers(4, irow)) ! Byte03
      retrows(irow)%second     = unsigned1_to_int4(headers(5, irow)) ! Byte04
      retrows(irow)%millisec1  = unsigned1_to_int4(headers(6, irow)) ! Byte05; Upper (more significant (larger) digits)
      retrows(irow)%millisec2  = unsigned1_to_int4(headers(7, irow)) ! Byte06; Lower
      retrows(irow)%Counter_A1 = unsigned1_to_int4(headers(8, irow)) ! Byte07;
      retrows(irow)%Counter_A2 = unsigned1_to_int4(headers(9, irow)) ! Byte08; ! FRAME counter between [0:63]
      retrows(irow)%Counter_B1 = unsigned1_to_int4(headers(10, irow)) ! Byte09;
      retrows(irow)%Counter_B2 = unsigned1_to_int4(headers(11, irow)) ! Byte10;
       ! A1, B1, B2 are the top, med, lower 8-bis of the 24-bit TI counter.
       !   WB: TI counter value is (I think): byte07*65536 + byte09*256 + byte10
      retrows(irow)%real_or_stored = unsigned1_to_int4(headers(12, irow)) ! Byte11; real(1) or stored(2)
      retrows(irow)%bitrate    = unsigned1_to_int4(headers(13, irow)) ! Byte12; bit-rate-low(0) or high(1);

      retrows(irow)%millisec_i4 = int( retrows(irow)%millisec1*256 + retrows(irow)%millisec2)
      retrows(irow)%second_real = real(retrows(irow)%second + retrows(irow)%millisec_i4/1000.0d0)
         ! second(Byte04)+millisec(= byte05 * 256 + byte06) (cf. readfits_SF_WD.c);
      retrows(irow)%TIcounter   = &  ! (cf. Telemetry FITS header comment)
           retrows(irow)%Counter_A1 * 65536 &
         + retrows(irow)%Counter_B1 *   256 &
         + retrows(irow)%Counter_B2

      retrows(irow)%i_frame = irow

      retrows(irow)%w_fi     = unsigned1_to_int4(telems(w_no('fi',     from1=.true.), irow)) ! W3, namely "FI"; defined in fort_util and asm_fits_common
      retrows(irow)%STAT_OBS = unsigned1_to_int4(telems(w_no('status', from1=.true.), irow))
      retrows(irow)%DPID_OBS = unsigned1_to_int4(telems(w_no('dp',     from1=.true.), irow))
      retrows(irow)%pi_mon   = unsigned1_to_int4(telems(w_no('pi_mon', from1=.true.), irow))

      write(s8, '(B8.8)') retrows(irow)%STAT_OBS 
      retrows(irow)%STAT_OBS_B8 = s8
      write(s8, '(B8.8)') retrows(irow)%DPID_OBS 
      retrows(irow)%DPID_OBS_B8 = s8

      ! Get 2-bit SF number and 6-bit Frame number from the Telemetry 16-byte header.
      call get_sf4_fr64(irow, telems, sf4, fr64)
      retrows(irow)%sf_2bit = sf4
      retrows(irow)%fr_6bit = fr64

      ! Array data for ACS and ASM-in-the-common (2 parts)
      j = w_no('pi_mon', from1=.true.)
      retrows(irow)%acss = unsigned1_to_int4(telems(j:j+DIM_ACS_C-1, irow))
      j = w_no('asm1_commons', from1=.true.)
      retrows(irow)%asm1_commons = unsigned1_to_int4(telems(j:j+DIM_ASM_C-1, irow))
      j = w_no('asm2_commons', from1=.true.)
      retrows(irow)%asm2_commons = unsigned1_to_int4(telems(j:j+DIM_ASM_C-1, irow))

      ! Get the main ASM data (See Table 5.1.1 (pp.200) and Table 5.5.5--6 (pp.233--234) for the format)
      ! Note Word (Wn) starts from n=0 in the Ginga Interim Report, whereas
      ! the index of asmdats() starts from 1.
      !
      ! asmdats(1)  : W4
      ! asmdats(2)  : W5
      ! ...
      ! asmdats(12) : W15
      ! asmdats(13) : W20
      ! asmdats(14) : W21
      ! ...
      ! asmdats(24) : W31
      ! asmdats(25) : W36
      ! ...
      ! asmdats(96) : W127
      do irow16=1, 8
        call calc_rows_asmdats_telem(irow16, iasm, itel)
        retrows(irow)%asmdats(iasm:iasm+11) = unsigned1_to_int4(telems(itel:itel+11,irow))
      end do
    end do
  end function get_telem_raws2types

  ! Add trows%mjd (Real*8) and trows%year (like 1989 (Integer*4))
  !
  subroutine add_mjd2telem(tfhead, trows)
    type(fits_header), intent(in) :: tfhead
    type(asm_telem_row), dimension(:), intent(inout) :: trows

    integer(kind=ip4), dimension(7) :: time, time_prev ! (YYYY, MM, DD, hh, mm, ss, msec)
    real(kind=dp8) :: mjd
    integer :: ntrows
    integer :: irowt ! I-th-ROW-Telemetry
    integer(kind=ip4) :: year_cur, month_cur  ! Currently processing Year and month of Telemetry
      ! This is required because each telemetry header-row does not 
      ! contain the year information, which must be guessed from
      ! the change, if any, of the month in a subsequent row.

    ntrows = size(trows)
    read(tfhead%DATE__OBS%val(1:4), '(I4)') year_cur
    read(tfhead%DATE__OBS%val(6:7), '(I2)') month_cur
    time_prev(:) = 0
    time_prev(1) = year_cur
    time_prev(2) = month_cur

    ! write (*, '("Calculating MJDs")', advance='no') ! Initialization for Progress bar
    do irowt=1, ntrows
    !do irowt=1, min(ntrows, 5) !! for Debugging
      !if (mod(irowt, 10000) == 0) then  ! Progress bar
      !  write (*, '(I0.1, "k")', advance='no') irowt/1000
      !else if (mod(irowt, 1000) == 0) then
      !  write (*, '(".")', advance='no')
      !end if

      ! To check whether the year has chnaged during the observation.
      if (trows(irowt)%month < month_cur) then
        ! Month decreased (probably from 12 to 1)
        !write(stderr, *)  ! To make it easier to read in the terminal (considering the Progress bar on STDOUT)
        write(stderr, '("WARNING: (row=", i0.6, ") In the single telemetry, month decreases from "&
             &, i0.2, " to ", i0.2, ", hence year is increased from "&
             &, i4.4, " to ", i4.4, " in this and subsequent rows in the Telemetry.")') &
             irowt, month_cur, trows(irowt)%month, year_cur, year_cur+1
             !&, i2.2, " to ", i2.2, ", hence year is increased from "&
        month_cur = trows(irowt)%month
        year_cur = year_cur + 1
      else if (trows(irowt)%month > month_cur) then
        ! Month increased, e.g., 9 to 10.
        month_cur = trows(irowt)%month
      else
        ! Do nothing b/c no change in month_cur
      end if

      ! Year has been determined. Now convert it to the MJD and record it.
      time = [year_cur &
            , trows(irowt)%month &
            , trows(irowt)%day   &
            , trows(irowt)%hour  &
            , trows(irowt)%minute  &
            , trows(irowt)%second  &
            , trows(irowt)%millisec_i4 ]
      trows(irowt)%year = year_cur
!print *,'DEBUG:147: irowt=',irowt,' time=',time
      call MJULIA(time, mjd)  ! mjd.f in ginga_tool
!print *,'DEBUG:148: mjd=',mjd
      trows(irowt)%mjd  = mjd
    end do
    write (*, *)  ! To clear advance='no' in write()
  end subroutine add_mjd2telem


  ! Read FRF and RETURN an allocated Array of type(asm_frfrow).
  !
  ! Description:
  function get_frf_types(fname) result(frfrows)
    integer, parameter :: MY_FUNIT = 151  ! arbitrary
    character(len=*), intent(in) :: fname  ! Telemetry filename
    type(asm_frfrow), dimension(:), allocatable :: frfrows

    integer(kind=1), dimension(:), allocatable :: ardata  ! Temporary array
    integer :: funit, status=-999, blocksize, hdutype, nframes, naxis1
    integer :: ncols = -99, nrows = -99
    integer :: datacode, repeat, num_axis, width
    integer(kind=4) :: nbytes
    integer, dimension(MAXAXES) :: naxes
    character(len=1024) :: errmsg
    character(len=1024) :: s, s1, s2
    character(len=80) :: comment, snull, msg
    integer :: i, j, k, irow

    INTEGER :: SFN
    INTEGER :: CNTL,COND,TIMES(7),TCNL,TCHK
    INTEGER :: SYNC(0:63),LOSTF,SFI,BITRAT,RELSTR,TIME(7) ! BITRAT: 0(H), 1(M), 2(L)
    INTEGER :: SUNPS(4),EFLAGS(4),NSAMPL, isampl
    real(kind=dp8) :: MJD, MJDS(4)  ! MJD is dummy
    real(kind=dp8) :: RBUFFS(17,4), ELVYS(4)
    !DOUBLE PRECISION    MJD,MJDS(4)

    funit = get_safe_unit(MY_FUNIT)

    call OPNFRF(funit, fname, cond)
    ! FiTs_Get_Number_of_RoWs
    call FTGNRW(funit, nrows, status) ! cf. FTGNRWLL() for INT*64

    allocate(frfrows(nrows), STAT=status)
!print *, 'DEBUG: nrows=', nrows

    call SFGET(cond)  ! Next SABU-Frame  ! cond is set 0(Normal) or 9(EOF, data are not set)

    irow = 0
    do while(cond .eq. 0)  ! cond is set at the end of the last loop.
!do i=1, 3  !!!!!!!!!!!!!!! 1--3 for testing/DEBUG
      irow = irow + 1
      call GETOAT(MJD, BITRAT, MJDS,RBUFFS,SUNPS,ELVYS,EFLAGS,NSAMPL) ! MJD, BITRAT are dummy.
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
        !  EFLAGS (OUT): CONDITION OF THE EARTH OCCULTATION                      
        !                0: NOT OCCULTED, 1: OCCULTED BY THE DARK EARTH         
        !                2: OCCULTED BY SUN SHONE EARTH                         
        !  NSAMPL (OUT): NUMBER OF THE ORBIT AND ATTITUDE DATA                  
        !                NSAMPL=1 FOR BITRATE H,M ,  =4 FOR BITRATE L           
      call SFCHCK(sync, lostf, sfn, bitrat, relstr, times)

!if (irow .le. 3) then
!  WRITE (s,'("19",i2,"-",i0.2,"-",i2," ",i0.2,":",i2,":",i2,".",i0.3)') times(1:7)
!  WRITE (*,'("--- Iteration: ", i1, " SFN=", i4, " MJD= ",a)') i, sfn, trim(s)
!  WRITE (*,'("MJDs=(", es17.9, ",", es17.9, ",", es17.9, ",", es17.9, ")")') mjds
!  WRITE (*,'("Euler1/", i1, "(1)=", f10.5)') nsampl, rbuffs(1,nsampl)
!  WRITE (*,'("Euler1/", i1, "(2)=", f10.5)') nsampl, rbuffs(2,nsampl)
!  WRITE (*,'("Euler1/", i1, "(3)=", f10.5)') nsampl, rbuffs(3,nsampl)
!end if

      !! See type(asm_frfrow) in asm_fits_common.f90
      frfrows(irow)%sync = sync      ! Array(0:63), 0=OK, 1=NG
      frfrows(irow)%lostf = lostf    ! Number of frames with SYNG=NG
      frfrows(irow)%sfn = sfn        ! SF Number (by SIRIUS)
      frfrows(irow)%bitrate = bitrat ! 0: high, 1: medium, 2: low
      frfrows(irow)%relstr  = relstr ! 0: real-data, 1: stored data
      frfrows(irow)%stime   = times  ! Start time of SF in (Y,M,D,h,m,s,ms) in INTEGER

      frfrows(irow)%mjds = mjds ! MJD OF THE ORBIT AND ATTITUDE
      frfrows(irow)%rbuffs = rbuffs ! (4, 17) RBUFF(J,*)  *=1,NSAMPL ! for backup/debugging
         ! NOTE: Index order is reversed from the others.
      frfrows(irow)%eulers(       :, :) = rbuffs(1:3, :) ! [J=1-3] EURLER ANGLES  (Z-Y-Z)
      frfrows(irow)%d_eulers(     :, :) = rbuffs(4:6, :) ! [J=4-6] DOT EURLER ANGLES
      frfrows(irow)%lon_lat(      :, :) = rbuffs(8:9,   :) ! [J=8-9] LONGITUDE, LATTITUDE
      frfrows(irow)%coords_earth( :, :) = rbuffs(11:12, :) ! [J=11-12] ALPHA,DELTA OF THE EARTH CENTER (1950 EQUINOX)
      frfrows(irow)%coords_magnet(:, :) = rbuffs(14:15, :) ! [J=14-15] ALPHA,DELTA OF THE MAGNETIC FIELD
      frfrows(irow)%coords_sun(   :, :) = rbuffs(16:17, :) ! [J=16-17] ALPHA,DELTA OF THE SUN
      frfrows(irow)%height     = rbuffs(7,  :)  ! [J=7] HEIGHT
      frfrows(irow)%dist_earth = rbuffs(10, :) ! [J=10] DISTANCE FROM THE EARTH CENTER                 
      frfrows(irow)%cor        = rbuffs(13, :) ! [J=13] CUT OFF RIGIDITY
      frfrows(irow)%sunps = sunps ! PRESENCE OF SUNSHINE, 1/0=YES/NO                        
      frfrows(irow)%elvys = elvys ! ELEVATION OF YAXIS FROM THE EARTH EDGE                  
      frfrows(irow)%eflags = eflags ! CONDITION OF THE EARTH OCCULTATION                      
      frfrows(irow)%nsampl = nsampl ! NUMBER OF THE ORBIT AND ATTITUDE DATA                  

      call SFGET(cond)  ! Next SABU-Frame  ! cond is set 0(Normal) or 9(EOF, data are not set)
    end do

    call CLSFRF(cond)
    call ftfiou(funit, status)
  end function get_frf_types

end module asm_read_telemetry

