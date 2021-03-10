program writeimage
integer, parameter :: dp = kind(1.d0)
  integer unit,bitpix,naxis
    integer status,blocksize,naxes(2)
  integer group,fpixel,nelements,array(300,200)
  integer  i,j,
  character(len=80) :: filename
  logical simple,extend

  character(len=30) :: errtext  ! error message (up to 30 characters in specification)

  integer :: nhdu, tfields
  character(len=80), dimension(2) :: ttypes = (/ 'TIME', 'Char' /), tunits, &
     tforms = (/ '1D ', '20A' /)  ! 20A20?
  real(kind=dp), dimension(2) :: dvalues = (/ 12345.6, 7.89 /), d2values = (/ 0.023, 0.0045 /)
  character(len=20), dimension(2) :: svalues = (/ 'saisho', 'tsugi1' /)
    
      status=0
! Name of the FITS file to be created:
filename='!'//'ATESTFILE.FITS'  ! '!' for clobber
! Get an unused Logical Unit Number to use to create the FITS file
call ftgiou(unit,status)
! create the new empty FITS file blocksize=1
call ftinit(unit,filename,blocksize,status)
call FTGERR(status, errtext)  ! Gets an error message
call FTGHDN(unit, nhdu)  ! CHDU: Current HDU (Primary: 1)
print *,'test-open1-status=',status,' / HDU=',nhdu,' / ',trim(errtext)

! initialize parameters about the FITS image (300 x 200 16-bit integers)
simple=.true.
      bitpix=16
      naxis=2
      naxes(1)=300
      naxes(2)=200
extend=.true.
! write the required header keywords
call ftphpr(unit,simple,bitpix,naxis,naxes,0,1,extend,status)
! initialize the values in the image with a linear ramp function
do j=1,naxes(2)
          do i=1,naxes(1)
              array(i,j)=i+j
            end do
          end do
      
! write the array to the FITS file group=1
fpixel=1
nelements=naxes(1)*naxes(2)
call ftpprj(unit,group,fpixel,nelements,array,status)

! write another optional keyword to the header
call ftpkyj(unit,'EXPOSURE',1500,'Total Exposure Time',status)

! Creates a binary table extension 'TestBinExt' (nrows=0(initialization))
tfields = 2  ! number of columns
call FTIBIN(unit,0,tfields,ttypes,tforms,tunits,'TestBinExt',.true., status) ! Creates an extension with basic header and moves to it.
call FTGHDN(unit, nhdu)  ! Current HDU number (1st-Extension=2)
call FTGERR(status, errtext)
print *,'test-new-ext=',status,' / HDU=',nhdu,' / ',trim(errtext)
call ftpkys(unit,'MY_HEAD','Arbitrary','My comment 01',status)  ! A header in the new extension

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

! close the file and free the unit number
call ftclos(unit, status)
call ftfiou(unit, status)

end program writeimage  ! end (in Fortran 77)
