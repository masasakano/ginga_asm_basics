
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
!   * status (W65): (Slew360 Mode: F32n+10 W65 B3)
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


