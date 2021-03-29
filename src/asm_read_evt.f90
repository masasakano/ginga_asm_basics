! Routines to read an ASM event FITS file and output as QDP/PCO files
!
module asm_read_evt
  use err_exit
  use fort_util
  use asm_consts

  implicit none 

  ! NOTE:
  ! This parameter is defined in COL_FORM_UNITS in asm_fits_common.f90
  ! Therefore, ideally, the value should be read like this:
  !    tmpfu = get_element('Tstart', COL_FORM_UNITS)
  !    coltemplate = trim(tmpfu%root)
  ! However, it is (re)defined here for simplicity and in order for
  ! this routine not to require "use asm_fits_common'
  character(len=*), parameter, private :: COLNAME_TSTART = 'Tstart'

contains

  !------------------------------------------------------------
  ! Read multiple channels from FITS and returns a (Integer*2) 2-dim Array (value, detector) for the summed data
  !------------------------------------------------------------
  function get_asm_summed_chan(funit, chan_l_h, nrows) result(retchans)
    implicit none
    character(len=*), parameter :: Subname = 'get_asmevt_summed_chan'
    integer, intent(in) :: funit, nrows
    integer, dimension(2), intent(in) :: chan_l_h ! Low and high channels to sum
    integer(kind=ip2), dimension(nrows, NUM_INSTR) :: retchans  ! NUM_INSTR defined in asm_fits_common

    integer(kind=ip2), dimension(nrows) :: tmpchs
    ! integer(kind=ip2) :: nullval  ! if 0, null value is not considered.
    logical :: anyf
    integer :: status, colnum, ich, idet
    character(len=30) :: errtext

    do idet=1, NUM_INSTR
      retchans(:, idet) = 0_ip2 
      do ich=chan_l_h(1), chan_l_h(2)
        colnum = (idet-1)*NCHANS_PHA + ich + 1   ! NCHANS_PHA defined in asm_fits_common
        tmpchs = 0_ip2
        status = 0  ! NOTE: Even if one of FTCGVI fails, the next one is attempted from scratch.
        ! FiTs_GeT_Column_Value: FTGCV[SBIJKEDCM](unit,colnum,frow,felem,nelements,nullval, >values,anyf,status)
        !  anyf is True if any of the values is undefined.
        call FTGCVI(funit, colnum, 1, 1, nrows, 0, tmpchs, anyf, status)
        !call FTGCVI(funit, colnum, 1, 1, nrows, UNDEF_INT2, tmpchs, anyf, status)
        if (status .ne. 0) then
          call FTGERR(status, errtext)
          write(stderr,'("WARNING: (",A,") Failed(1) in FTGCVI() with Status=",A," (",A,"): colnum=",A)') &
             Subname, trim(ladjusted_int(status)), trim(errtext), trim(ladjusted_int(colnum))

          !! ------------ Comment ------------
          !! Rerun FTGCVI() with INTEGER*2 status, and it may work if GINGA_DEBUG=1
          !! There is no reason this works, when the others do not, but it seemed to do so in some cases (in the past?)!
          status = 0  ! status is reset to 0 so this attempt may succeed.
          call FTGCVI(funit, colnum, 1, 1, nrows, 0, tmpchs, anyf, status)
          if (status .ne. 0) then
            call FTGERR(int(status), errtext)
            write(stderr,'("ERROR: (",A,") Failed(2) in FTGCVI() with Status=",A," (",A,"): colnum=",A)') &
               Subname, trim(ladjusted_int(int(status))), trim(errtext), trim(ladjusted_int(colnum))
          end if
        else
          !if (IS_DEBUG()) write(stderr,'("NOTE: Success in FTGCVI()")')
          if (anyf) write(stderr,'("ERROR: (",A,") anyf is TRUE: colnum=",A)') &
             Subname, trim(ladjusted_int(colnum))
        end if

        retchans(:, idet) = retchans(:, idet) + tmpchs
      end do
    end do
  end function get_asm_summed_chan

  !------------------------------------------------------------
  ! Read ASM fits and returns required Arrays to output.
  !------------------------------------------------------------
  subroutine asm_time_row_det_band(fname, chans, artime, outchans)
    implicit none
    character(len=*), parameter :: Subname = 'asmevt_time_row_det_band'
    integer, parameter :: MY_FUNIT = 61  ! arbitrary
    character(len=*), intent(in) :: fname  ! fname for ASM.fits
    integer, dimension(:,:), intent(in) :: chans  ! ((Low,High), i-th-band)
    real(kind=dp8), dimension(:), allocatable, intent(out) :: artime
    integer(kind=ip2), dimension(:,:,:), allocatable, intent(out) :: outchans ! (Row(channel-values), Detector, Band(Low(1)/High(2)))
    integer :: funit, blocksize, hdutype, status=-999, colnum
    integer :: nrows, iband
    logical :: existdat, anyf
    character(len=1024) :: comment
    character(len=30) :: errtext
    character(len=MAX_FITS_CHAR) :: coltemplate
    logical :: success_ftgiou

    success_ftgiou = .true.
    status = 0
    call FTGIOU(funit, status)
    if (status .ne. 0) then
      call FTGERR(status, errtext)
      write(stderr,'("WARNING: (",A,") Failed in ftgiou(): status= ",A," (",A,")")', advance='no') &
         Subname, trim(ladjusted_int(status)), trim(errtext)
      if ((funit > 999) .or. (funit < 9)) then
        write(stderr,'(": unit = ",I12,". Manually reset to ",I3)') funit, MY_FUNIT
        funit = MY_FUNIT
        success_ftgiou = .false.
      else
        write(stderr,'(": unit = ",I12,", which is used nonetheless.")') funit
        !write(stderr,'(": unit = ",I12,", which is reset to ",I3)') funit, MY_FUNIT
        !funit = MY_FUNIT
        !success_ftgiou = .false.
      end if
    end if
    status = 0
    call FTOPEN(funit, trim(fname), 0, blocksize, status)  ! 0: readonly
    call err_exit_if_status(status, 'Failed to open the FITS to read: '//trim(fname))

    ! Move to the 1st extention
    call FTMAHD(funit, 2, hdutype, status)
    call err_exit_if_status(status, 'Failed to move to the 1st extension: '//trim(fname))

    call FTGKYL(funit, 'EXISTDAT', existdat, comment, status)
    if (.not. existdat) then
      write(stderr, '(A)') 'Input FITS ('//trim(fname)//') contains no data. No files are created.'
      call EXIT(0)
    end if

    ! FiTs_Get_Number_of_RoWs
    call FTGNRW(funit, nrows, status) ! cf. FTGNRWLL() for INT*64
    if ((status .ne. 0) .or. (nrows .le. 0)) then
      call err_exit_with_msg('Number of rows are strange: '//trim(ladjusted_int(nrows))//' / status='//trim(ladjusted_int(status)))
      stop  ! redundant
    end if

    allocate(outchans(nrows, NUM_INSTR, size(chans, 2))) ! (Row, Detector, Band)
    allocate(artime(nrows)) ! for Tstart
    outchans = -99
    artime = UNDEF_DOUBLE

    ! Get summed channle data
    do iband=1, size(chans, 2)
      outchans(:,:,iband) = get_asm_summed_chan(funit, chans(:, iband), nrows)
    end do

    ! Get Tstart (Time column)
    coltemplate = COLNAME_TSTART
    !tmpfu = get_element('Tstart', COL_FORM_UNITS) ! better routine, but needs use asm_fits_common
    !coltemplate = trim(tmpfu%root)

    ! Get column number for Tstart (should be 97)
    ! FTGCNO(unit,casesen,coltemplate, > colnum,status) ! FiT_Get_Column_from_Name_to_nO
    call FTGCNO(funit, .false., trim(coltemplate), colnum, status)

    ! Get the Array of Tstart
    ! FTGCV[SBIJKEDCM](unit,colnum,frow,felem,nelements,nullval, > values,anyf,status) ! FiT_Get_Column_Value
    call FTGCVD(funit, colnum, 1, 1, nrows, UNDEF_DOUBLE, artime, anyf, status)

    call FTCLOS(funit, status)
    call err_exit_if_status(status, 'Failed to close the FITS: '//trim(fname))
    status = 0
    if (success_ftgiou) call FTFIOU(funit, status)

  end subroutine asm_time_row_det_band

  !------------------------------------------------------------
  ! Write QDP/PCO
  !------------------------------------------------------------
  subroutine write_qdp(fname, outroot, chans, artime, outchans)
    implicit none
    integer, parameter :: MY_FUNIT = 62  ! arbitrary
    character(len=*), parameter :: Fmt_MJD = 'E20.14'
    !real(dp8), parameter :: ys1 = 0.75, ys2 = 0.9+1/60.d0, yd1 = -(0.1+1/30.d0)  ! QDP Y-starting positions 1 and 2 and difference
    real(dp8), parameter :: ys1 = 0.78, ys2 = 0.93+1/60.d0, yd1 = -(0.1+1/30.d0)  ! QDP Y-starting positions 1 and 2 and difference
    character(len=*), intent(in) :: fname, outroot  ! fname for ASM.fits
    integer, dimension(:,:), intent(in) :: chans  ! ((Low,High), i-th-band) ! for displaying purpose only
    real(dp8), dimension(:), intent(in) :: artime
    integer(kind=ip2), dimension(:,:,:), intent(in) :: outchans ! (Row(channel-values), Detector(6), Band(Low(1)/High(2)))
    integer :: status ! IOSTAT of open()
    real(dp8) :: mjd_offset = UNDEF_DOUBLE  ! All Time is offset with this MJD value (n.b., the value of the first row is 0).
    character(len=2048) :: qdpfile, pcofile
    integer :: iy, idet, irow, iband, statustmp
    integer :: unit, colnum
    character(len=5), dimension(size(chans, 2)) :: strchs
    logical :: success_ftgiou

    qdpfile = trim(outroot)//'.qdp'
    pcofile = trim(outroot)//'.pco'
    mjd_offset = artime(1)  ! The offset is the first row of artime (=Tstart in the ASM event file)

    do iband=1,size(chans,2)
      if (chans(1, iband) == chans(2, iband)) then
        write(strchs(iband), '(I0.2)') chans(1, iband)
      else
        write(strchs(iband), '(I0.2,"-",I0.2)') chans(1, iband), chans(2, iband)
      end if
    end do

    status = 0
    success_ftgiou = .true.
    call FTGIOU(unit, status) ! Just get a safe IO unit
    if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) then
      write(stderr,'("WARNING: Failed in ftgiou(): unit = ",I12,". Manually reset to ",I3)') unit, MY_FUNIT
      unit = MY_FUNIT
      success_ftgiou = .false.
    end if

    ! QDP file
    open(UNIT=unit, file=qdpfile, IOSTAT=status, STATUS='UNKNOWN')  ! clobber=yes (i.e., overwrite, maybe)
    write(unit, '("@",A)') trim(basename(trim(pcofile)))
    write(unit, '("! MJD-offset for Tstart = ",'//Fmt_MJD//')') mjd_offset
    write(unit, '("! Tstart ")', advance='no')
    iy = 1
    do idet=1, size(outchans, 2)
      do iband=1, size(outchans, 3)
        iy = iy + 1
        write(unit, '(" Y"I1,"FW",I1,"_CH",A)', advance='no') mod(idet-1,2)+1, (idet-1)/2+1, strchs(iband)
      end do
    end do
    write(unit, '(A)') ''

    do irow=1, size(outchans, 1)
      write(unit, '('//Fmt_MJD//')', advance='no') artime(irow)-mjd_offset
      do idet=1, size(outchans, 2)
        do iband=1, size(outchans, 3)
          write(unit, '(" ",I4)', advance='no') outchans(irow, idet, iband)
        end do
      end do
      write(unit, '(A)') ''
    end do
    close(UNIT=unit, IOSTAT=statustmp)

    ! PCO file
    open(UNIT=unit, file=pcofile, IOSTAT=status, STATUS='UNKNOWN')  ! clobber=yes (i.e., overwrite, maybe)
    write(unit, '("CSIZ  0.60")')
    write(unit, '("LAB T ASM light curves (CH:",A,") from MJD=",'//Fmt_MJD//')') &
       trim(join_chars(strchs, ', ')), mjd_offset
    write(unit, '("LAB F ",A)') trim(basename(fname))

    ! If default (aka, 6 detectors for 2 bands):
    if ((size(outchans, 2) == 6) .and. (size(outchans, 3) == 2)) then
      do idet=1, size(outchans, 2)
        write(unit, '("win ",I2)') idet+1
        if (idet == 1) then
          write(unit, '("LAB T ASM light curves (CH:",A,") from MJD=",'//Fmt_MJD//')') &
             trim(join_chars(strchs, ', ')), mjd_offset
          write(unit, '("LAB F ",A)') trim(basename(fname))
        end if
        write(unit, '("yplot ",I2," ",I2)') idet*2, idet*2+1
        write(unit, '("loc 0 ",F12.10," 1 ",F12.10)') ys1+(idet-1)*yd1, ys2+(idet-1)*yd1
        write(unit, '("LAB Y Y",I1,"FW",I1)') mod(idet-1,2)+1, (idet-1)/2+1
        write(unit, '("R Y",I1," -0.5 255.5")') idet+1
        if (idet .ne. size(outchans, 2)) write(unit, '("lab nx off")')
      end do
    end if
    write(unit, '("LAB X Tstart-MJD(=",'//Fmt_MJD//',") [day]")') mjd_offset
    write(unit, '("R X")')
    
    close(UNIT=unit, IOSTAT=statustmp)
    if (success_ftgiou) call FTFIOU(unit, statustmp)
  end subroutine write_qdp

  !------------------------------------------------------------
  ! Read ASM fits and write QDP/PCO (Parent subroutine)
  !------------------------------------------------------------
  subroutine read_asm_write_qdp(fname, outroot, chans) !, status)
    implicit none
    character(len=*), intent(in) :: fname, outroot  ! fname for ASM.fits
    integer, dimension(:,:), intent(in) :: chans  ! ((Low,High), i-th-band)
    real(kind=dp8), dimension(:), allocatable :: artime
    integer(kind=ip2), dimension(:,:,:), allocatable :: outchans

    call asm_time_row_det_band(fname, chans, artime, outchans)
    call write_qdp(fname, outroot, chans, artime, outchans)

    if (allocated(artime)) deallocate(artime)
    if (allocated(outchans)) deallocate(outchans)
  end subroutine read_asm_write_qdp
end module asm_read_evt

