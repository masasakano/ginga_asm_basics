! Routines to read an ASM event FITS file
!
module asm_read_evt
  use err_exit
  use fort_util
  !use asm_fits_common
  !use asm_aux

  implicit none 

  !!! all copied from asm_fits_common
  integer, parameter, public ::  dp8 = 8 ! FITS file REAL8
  integer, parameter, public ::  ip4 = 4 ! "J" in FITSIO; FITS file INTEGER4
  integer, parameter, public ::  ip2 = 2 ! "I" in FITSIO
  integer, parameter, public :: MAX_FITS_CHAR = 68
  integer, parameter, public :: NUM_INSTR = 6, NCHANS_PHA = 16, NCHANS_TIME = 8  ! Number of channels in each mode 
  real(dp8),    parameter :: UNDEF_DOUBLE = -1024.0_dp8
  integer, parameter :: LEN_READABLE_KEY = 32  ! Char-length of maximum human-readable key name
  integer, parameter :: LEN_T_ARGV = 1024
contains

  !! simulated the one in asm_fits_common
  !logical function IS_DEBUG() result(ret)
  !  character(len=1024) :: env_debug
  !  integer :: status = -999

  !  env_debug = ''
  !  !ret = .false.
  !  !ret = .true.
  !  call GET_ENVIRONMENT_VARIABLE('GINGA_DEBUG', env_debug, STATUS=status)
  !  if ((status == 1) .or. (status == 2)) then  ! 1 for non-existent, 2 for environment var. not-supported by the system
  !    ret = .false.
  !  !else if ((trim(env_debug) == 'false') .or. (trim(env_debug) == 'no')) then  ! 1 for non-existent, 2 for environment var. not-supported by the system
  !  !  ret = .false.
  !  else
  !    ret = .true.
  !  end if
  !end function IS_DEBUG

  !!! Returns true if the environmental variable DEBUG is set and NOT 'false' or 'no'.
  !!logical function IS_DEBUG() result(ret)
  !!  character(len=1024) :: env_debug
  !!  integer :: status
  !!  integer, save :: prev_result = -99

  !!  ! To avoid repeatedly accessing the system to get the environmental variable.
  !!  if (prev_result .ge. 0) then  ! This IF-statment is redundant (but is left for readability).
  !!    select case(prev_result)
  !!    case(0)
  !!      ret = .false.
  !!      return
  !!    case(1)
  !!      ret = .true.
  !!      return
  !!    end select
  !!  end if
  !!      
  !!  call GET_ENVIRONMENT_VARIABLE('GINGA_DEBUG', env_debug, STATUS=status)
  !!  if ((status == 1) .or. (trim(env_debug) == 'false') .or. (trim(env_debug) == 'no')) then  ! 1 for non-existent, 2 for environment var. not-supported by the system
  !!    ret = .false.
  !!    prev_result = 0
  !!  else
  !!    ret = .true.
  !!    prev_result = 1
  !!  end if
  !!end function IS_DEBUG

  !!! simulated the one in asm_fits_common
  !! dump all_argv
  !subroutine dump_all_argv(rows, nargv)
  !  character(len=*), parameter :: Subname = 'dump_all_argv'
  !  type(t_argv), dimension(:), intent(in) :: rows
  !  integer, intent(in), optional :: nargv  ! Optionally specify the size of the Array if it is not size(rows).
  !  integer :: i, nsize_rows

  !  nsize_rows = size(rows)
  !  if (present(nargv)) then
  !    if (nsize_rows < nargv) then
  !      call err_exit_with_msg('('//Subname//') given nargv=('//trim(ladjusted_int(nargv))//') > '//trim(ladjusted_int(nsize_rows)))
  !    end if
  !    nsize_rows = nargv
  !  else
  !  end if
  !  
  !  print *, '--------- all argv (size=', size(rows), ') ---------' 
  !  do i=1, nsize_rows
  !    print *, trim(ladjusted_int(i))//': ('//trim(rows(i)%key)//') ', trim(rows(i)%val)
  !  end do
  !end subroutine dump_all_argv

  !! simulated the one in asm_fits_common  ! Original: get_int_from_key_argv(key, ary)
  !!function get_int_from_key(key, ary) result(iret)
  !function get_int_from_val(key, ary) result(iret)
  !  character(len=*), intent(in) :: key
  !  type(t_argv), dimension(:), intent(in) :: ary
  !  integer :: iret
  !  character(len=LEN_T_ARGV) :: s

  !  s = get_val_from_key(trim(key), ary)
  !  read(s, '(I65)') iret
  !end function get_int_from_key

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
    integer(kind=ip2) :: statusi2
    character(len=30) :: errtext

    ! nullval = -999
!if (IS_DEBUG()) print *,'DEBUG:3259: nrows=',trim(ladjusted_int(nrows)),' si=',size(tmpchs),' chan_l_h=',chan_l_h
    do idet=1, NUM_INSTR
      retchans(:, idet) = 0_ip2 
      do ich=chan_l_h(1), chan_l_h(2)
        colnum = (idet-1)*NCHANS_PHA + ich + 1   ! NCHANS_PHA defined in asm_fits_common
!if (IS_DEBUG() .and. (idet == 2)) print *,'DEBUG:3261: colnum=',colnum,' nrows=',nrows
        tmpchs = 0_ip2
        ! FiTs_GeT_Column_Value: FTGCV[SBIJKEDCM](unit,colnum,frow,felem,nelements,nullval, >values,anyf,status)
        !  anyf is True if any of the values is undefined.
        call FTGCVI(funit, colnum, 1, 1, nrows, 0, tmpchs, anyf, status)
        !call FTGCVI(funit, colnum, 1, 1, nrows, UNDEF_INT2, tmpchs, anyf, status)
!if (.true.) then
        if (status .ne. 0) then
          call FTGERR(status, errtext)
          write(stderr,'("ERROR: (",A,") Failed in FTGCVI() with Status=",A," (",A,"): colnum=",A)') &
             Subname, trim(ladjusted_int(status)), trim(errtext), trim(ladjusted_int(colnum))

!          call FTGCVI(funit, colnum, 1, 1, nrows, 0, tmpchs, anyf, status)
!          if (status .ne. 0) then
!            call FTGERR(int(status), errtext)
!            write(stderr,'("ERROR: (",A,") Failed(2) in FTGCVI() with Status=",A," (",A,"): colnum=",A)') &
!               Subname, trim(ladjusted_int(int(status))), trim(errtext), trim(ladjusted_int(colnum))

          !! ------------ Comment ------------
          !! Rerun FTGCVI() with INTEGER*2 status, and it may work if GINGA_DEBUG=1
          !! There is no reason this works, when the others do not, but it seems to do so!
          call FTGCVI(funit, colnum, 1, 1, nrows, 0, tmpchs, anyf, statusi2)
          if (statusi2 .ne. 0) then
            call FTGERR(int(statusi2), errtext)
            write(stderr,'("ERROR: (",A,") Failed(2) in FTGCVI() with Status=",A," (",A,"): colnum=",A)') &
               Subname, trim(ladjusted_int(int(statusi2))), trim(errtext), trim(ladjusted_int(colnum))
          end if
        else
          !if (IS_DEBUG()) write(stderr,'("NOTE: Success in FTGCVI()")')
          if (anyf) write(stderr,'("ERROR: (",A,") anyf is TRUE: colnum=",A)') &
             Subname, trim(ladjusted_int(colnum))
        end if

        retchans(:, idet) = retchans(:, idet) + tmpchs
      end do
!if (IS_DEBUG() .and. (idet == 2)) print *,'DEBUG:3268: sum=',sum(retchans(:, idet)),' for idet=',trim(ladjusted_int(idet))
!if (IS_DEBUG() .and. (idet == 5)) print *,'DEBUG:3269:  87(det=5)=',retchans( 87, idet)
!if (IS_DEBUG() .and. (idet == 2)) print *,'DEBUG:3269: 110(det=2)=',retchans(110, idet)
    end do
!if (IS_DEBUG())                   print *,'DEBUG:3270:  87=',retchans(87, :)
!if (IS_DEBUG())                   print *,'DEBUG:3270: 110=',retchans(110, :)
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
!!! from asm_fits_common
!    type(t_form_unit) :: tmpfu
    logical :: success_ftgiou

!write(stderr,'("")', advance='no')
    success_ftgiou = .true.
    call FTGIOU(funit, status)
!if (IS_DEBUG()) write(stderr,'("(",A,") UNIT= ",A)') Subname, trim(ladjusted_int(funit))
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
!funit = MY_FUNIT  ! DEBUG
!if (IS_DEBUG()) print *,'DEBUG:278:funit=',funit,' fname=',trim(fname),' blocksize=',blocksize
    call FTOPEN(funit, trim(fname), 0, blocksize, status)  ! 0: readonly
    !call FTOPEN(funit, trim(fname), 0, blocksize, status)  ! 0: readonly
    !call ftopen(funit, fname, 0, blocksize, status)  ! 0: readonly
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
!if (IS_DEBUG()) print *,'DEBUG:3245: out-sizes=',size(outchans, 1),' s2=',size(outchans, 2),' s3=',size(outchans, 3)
    outchans = -99
    !artime = 0.0d0

    ! Get summed channle data
    do iband=1, size(chans, 2)
      outchans(:,:,iband) = get_asm_summed_chan(funit, chans(:, iband), nrows)
!if (IS_DEBUG()) print *,'DEBUG:3369: iband=',iband,' sum=',sum(outchans(:,2,iband))
    end do

    ! Get Tstart (Time column)
    !tmpfu = get_element('Tstart', COL_FORM_UNITS)
    !coltemplate = trim(tmpfu%root)
    coltemplate = 'Tstart'

    ! Get column number for Tstart (should be 97)
    ! FTGCNO(unit,casesen,coltemplate, > colnum,status) ! FiT_Get_Column_from_Name_to_nO
    call FTGCNO(funit, .false., trim(coltemplate), colnum, status)

    ! Get the Array of Tstart
    ! FTGCV[SBIJKEDCM](unit,colnum,frow,felem,nelements,nullval, > values,anyf,status) ! FiT_Get_Column_Value
    call FTGCVD(funit, colnum, 1, 1, nrows, UNDEF_DOUBLE, artime, anyf, status)

    call FTCLOS(funit, status)
    call err_exit_if_status(status, 'Failed to close the FITS: '//trim(fname))
    if (success_ftgiou) call FTFIOU(funit, status)

  end subroutine asm_time_row_det_band

  !------------------------------------------------------------
  ! Write QDP/PCO
  !------------------------------------------------------------
  subroutine write_qdp(fname, outroot, chans, artime, outchans, status)
    implicit none
    integer, parameter :: MY_FUNIT = 62  ! arbitrary
    real(dp8), parameter :: ys1 = 0.75, ys2 = 0.9+1/60.d0, yd1 = -(0.1+1/30.d0)  ! QDP Y-starting positions 1 and 2 and difference
    character(len=*), intent(in) :: fname, outroot  ! fname for ASM.fits
    integer, dimension(:,:), intent(in) :: chans  ! ((Low,High), i-th-band) ! for displaying purpose only
    real(dp8), dimension(:), intent(in) :: artime
    integer(kind=ip2), dimension(:,:,:), intent(in) :: outchans ! (Row(channel-values), Detector(6), Band(Low(1)/High(2)))
    integer, intent(out), optional :: status
    character(len=2048) :: qdpfile, pcofile
    integer :: iy, idet, irow, iband, statustmp
    integer :: unit, colnum
    character(len=5), dimension(size(chans, 2)) :: strchs

    qdpfile = trim(outroot)//'.qdp'
    pcofile = trim(outroot)//'.pco'

!if (IS_DEBUG()) print *,'DEBUG:3142: out-beg sum=',sum(outchans(:,2,1))
    do iband=1,size(chans,2)
      if (chans(1, iband) == chans(2, iband)) then
        write(strchs(iband), '(I0.2)') chans(1, iband)
      else
        write(strchs(iband), '(I0.2,"-",I0.2)') chans(1, iband), chans(2, iband)
      end if
    end do

    call FTGIOU(unit, status) ! Just get a safe IO unit
    if ((status .ne. 0) .and. ((unit > 999) .or. (unit < 9))) then
      write(stderr,'("WARNING: Failed in ftgiou(): unit = ",I12,". Manually reset to ",I3)') unit, MY_FUNIT
      unit = MY_FUNIT
    end if
!unit = MY_FUNIT  ! DEBUG

!if (IS_DEBUG()) print *,'DEBUG:3145: size1=',size(outchans,1),'sizes=',size(outchans, 2),' s3=',size(outchans, 3)
    ! QDP file
    open(UNIT=unit, file=qdpfile, IOSTAT=status, STATUS='UNKNOWN')  ! clobber=yes (i.e., overwrite, maybe)
    write(unit, '("@",A)') trim(pcofile)
    write(unit, '("! Tstart ")', advance='no')
    iy = 1
    do idet=1, size(outchans, 2)
      do iband=1, size(outchans, 3)
        iy = iy + 1
        write(unit, '(" Y"I1,"FW",I1,"_CH",A)', advance='no') mod(idet-1,2)+1, (idet-1)/2+1, strchs(iband)
      end do
    end do
    write(unit, '(A)') ''

!if (IS_DEBUG()) print *,'DEBUG:3157: out-after sum=',sum(outchans(:,2,1))
    do irow=1, size(outchans, 1)
      write(unit, '(E20.14)', advance='no') artime(irow)
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
    write(unit, '("LAB T ASM light curves (CH:",A,") of",A)') trim(join_chars(strchs, ', ')), trim(basename(fname))
    write(unit, '("LAB F ",A)') trim(basename(fname))
    !iy = 1
    do idet=1, size(outchans, 2)
    !  do iband=1, size(outchans, 3)
    !    iy = iy + 1
    !    write(unit, '("LAB G",I1," Y"I1,"FW",I1,"_CH",A)') iy, mod(idet-1,2)+1, (idet-1)/2+1, strchs(iband)
    !  end do
     !write(unit, '("LAB G",I1," Y"I1,"FW",I1)') idet+1, mod(idet-1,2)+1, (idet-1)/2+1
    end do

    ! If default (aka, 6 detectors for 2 bands):
    if ((size(outchans, 2) == 6) .and. (size(outchans, 3) == 2)) then
      do idet=1, size(outchans, 2)
        write(unit, '("win ",I2)') idet+1
        if (idet == 1) then
          write(unit, '("LAB T ASM light curves (CH:",A,") of",A)') trim(join_chars(strchs, ', ')), trim(basename(fname))
          write(unit, '("LAB F ",A)') trim(basename(fname))
        end if
        write(unit, '("yplot ",I2," ",I2)') idet*2, idet*2+1
        write(unit, '("loc 0 ",F12.10," 1 ",F12.10)') ys1+(idet-1)*yd1, ys2+(idet-1)*yd1
        write(unit, '("LAB Y Y"I1,"FW",I1)') mod(idet-1,2)+1, (idet-1)/2+1
        if (idet .ne. size(outchans, 2)) write(unit, '("lab nx off")')
      end do
    end if
    write(unit, '("LAB X Tstart")')
    write(unit, '("R X")')
    
    close(UNIT=unit, IOSTAT=statustmp)
    call FTFIOU(unit, statustmp)
  end subroutine write_qdp

  !------------------------------------------------------------
  ! Read ASM fits and write QDP/PCO (Parent subroutine)
  !------------------------------------------------------------
  subroutine read_asm_write_qdp(fname, outroot, chans) !, status)
    implicit none
    character(len=*), intent(in) :: fname, outroot  ! fname for ASM.fits
    integer, dimension(:,:), intent(in) :: chans  ! ((Low,High), i-th-band)
    integer :: status
    real(kind=dp8), dimension(:), allocatable :: artime
    integer(kind=ip2), dimension(:,:,:), allocatable :: outchans

    call asm_time_row_det_band(fname, chans, artime, outchans)
    call write_qdp(fname, outroot, chans, artime, outchans, status)

    if (allocated(artime)) deallocate(artime)
    if (allocated(outchans)) deallocate(outchans)
  end subroutine read_asm_write_qdp
end module asm_read_evt

