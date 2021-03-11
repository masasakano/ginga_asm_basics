program asmmkevt

  use iso_fortran_env, only : stderr=>ERROR_UNIT   

  use err_exit
  use fort_util
  use asm_fits_common
  use asm_aux
  use asm_read_telemetry
  use asm_fitsout

  implicit none

!integer, parameter :: dp = kind(1.d0) ! defined in asm_fits_common
  integer :: Maxaxes, nbytepercard, nbyteforheader
  parameter(Maxaxes = 2, nbytepercard = 144, nbyteforheader = 16 )
  character(len=*), parameter :: Subname = 'main'
  integer sfn                                 
      INTEGER   SYNC(0:63),LOSTF,BITRAT,RELSTR ! ! BITRAT: 0(H), 1(M), 2(L)
  !integer(kind=ip4), dimension(7) :: time ! (YYYY, MM, DD, hh, mm, ss, msec)
      INTEGER   COND,TIMES(7)

      INTEGER   SUNPS(4),EFLAGS(4),NSAMPL                        
  real(kind=dp8) :: mjd
  real(kind=dp8) ::     MJDS(4)
  real(kind=dp8) ::     RBUFFS(17,4),ELVYS(4)

  integer :: i, j
  character(len=1024) :: errmsg, arg, fname = '', telfil='', frffil = '', outfil = '', s
  character(len=30) :: errtext

  integer :: funit, status=-999, blocksize !, hdutype, nframes, naxis1
  integer, dimension(Maxaxes) :: naxes

  type(fits_header) :: tfhead
  integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
  type(asm_telem_row), dimension(:), allocatable :: trows

  type(fits_header) :: frfhead
  type(asm_frfrow), dimension(:), allocatable :: frfrows
  type(asm_sfrow), dimension(:), allocatable :: relrows

!---------- TEST      
      integer unit,bitpix,naxis !status,blocksize,naxes(2)
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

  !type(t_argv), dimension(:), allocatable :: allargv
  type(t_argv), dimension(3) :: argv

   !! USAGE: /asmmkevt ../../ginga_samples/ginga_sirius_P198804280220.fits ../../ginga_samples/FR880428.S0220.fits ../../ginga_samples/mkevt_out_test.fits
  argv = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')] 
  i = -1  ! i=0 is for $0 (index for all the original ARGV)
  j =  0  ! Index only for the main arugment.
  do
    i = i+1
    call get_command_argument(i, arg)
    if (i == 0) cycle
    if (len_trim(arg) == 0) exit

    if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
      print *, 'USAGE: asmmkevt [-h] Telemetry.fits FRF.fits out.fits'
      call EXIT(0)
    end if
    j = j + 1
    if (j > 3) call err_exit_with_msg('The number of the main argument must be exactly 3, but given more.')
    argv(j)%val = arg
    if (i == 1) then
      fname = arg  ! Telemetry
      telfil= arg
      write (*,*) 'Telemetry: '//trim(arg)
    else if (i == 2) then
      frffil = arg ! FRF
      write (*,*) 'FRF:       '//trim(arg)
    else if (i == 3) then
      outfil = arg ! FRF
      write (*,*) 'Outfile:   '//trim(arg)
    end if
  end do
  if (j < 3) call err_exit_with_msg('The number of the main argument must be exactly 3, but given only '//trim(ladjusted_int(j)))

  WRITE (*,*) 'fname=' // TRIM(fname)  ! maybe syntax error strictly?

  ! Get (fill) headers, telems: raw byte Array(word(byte), row)
!  call read_telemetry(fname, headers, telems) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (.false.) then
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

    end if
    
  !---------------- MAIN ------------------
    
  ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
  call read_telemetry(trim(get_val_from_key('telemetry', argv)), tfhead, headers, telems) ! tfhead: Telemetry-Fits-HEADer
  trows = get_telem_raws2types(headers, telems)
  call add_mjd2telem(tfhead, trows)

  ! Get the FRF
  !call mk_frf_rows(trim(frffil), frfhead, frfrows)
  call mk_frf_rows(trim(get_val_from_key('FRF', argv)), frfhead, frfrows)

  !--- get ASM_sfrows (for relation)
  relrows = get_asm_sfrow(trows, frfrows)
  call update_asm_sfrow_modes(trows, relrows) !, skip_validate=.true.)

  !call write_asm_fits(trim(DEF_FNAME_OUT), tfhead, trows, frfrows, relrows, status)
  !call write_asm_evt_fits(outfil, tfhead, trows, relrows, status)
  call write_asm_evt_fits(get_val_from_key('outfile', argv), tfhead, trows, relrows, status)

if (.false.) then
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
end if

end program asmmkevt


