program asmmkevt

  use iso_fortran_env, only : stderr=>ERROR_UNIT   

  use err_exit
  use fort_util
  use asm_consts
  use asm_fits_common
  use asm_aux
  use asm_read_telemetry
  use asm_fitsout

  implicit none

  character(len=*), parameter :: FITS_CREATOR = 'asmmkevt ver.'//ASM_BASICS_VERSION ! defined in asm_fits_common
  character(len=*), parameter :: Subname = 'main'

  character(len=1024) :: arg, telfil='', frffil = '', outfil = '', comname=''
  integer :: i, j, status=-999
  logical :: torf

  type(fits_header) :: tfhead, outhead
  type(asm_telem_row), dimension(:), allocatable :: trows

  type(fits_header) :: frfhead
  type(asm_frfrow), dimension(:), allocatable :: frfrows
  type(asm_sfrow), dimension(:), allocatable :: relrows

  type(t_argv), dimension(3) :: argv

  !! USAGE: ./asmmkevt ../samples/ginga_sirius_P198804280220.fits.gz ../samples/FR880428.S0220.fits.gz ../../ginga_samples/mkevt_out_test.fits
  argv = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')] 
  i = -1  ! i=0 is for $0 (index for all the original ARGV)
  j =  0  ! Index only for the main arugment.
  do
    i = i+1
    call get_command_argument(i, arg)  ! Fortran 2003
    if (i == 0) then  ! i=0 is for $0
      comname = trim(arg)
      cycle
    end if
    if (len_trim(arg) == 0) exit

    if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
      print *, 'USAGE: asmmkevt [-h] Telemetry.fits FRF.fits out.fits'
      print *, ' NOTE: you may set environmental variable GINGA_CHATTER=4 to make it quieter.'
      call EXIT(0)
    end if
    j = j + 1
    if (j > 3) call err_exit_with_msg('The number of the main arguments must be exactly 3, but given more.')
    argv(j)%val = arg
    if (i == 1) then
      telfil= arg  ! Telemetry
      write (*,*) 'Telemetry: '//trim(arg)
    else if (i == 2) then
      frffil = arg ! FRF
      write (*,*) 'FRF:       '//trim(arg)
    else if (i == 3) then
      outfil = arg ! FRF
      write (*,*) 'Outfile:   '//trim(arg)
    end if
  end do
  if (j < 3) call err_exit_with_msg('The number of the main arguments must be exactly 3, but given only '//trim(ladjusted_int(j)))

!if (IS_DEBUG()) WRITE (*,*) 'fname=' // TRIM(fname)

  ! FileTest
  inquire(file=trim(telfil), exist=torf)
  if (.not. torf) call err_exit_with_msg('File does not exist: '//trim(telfil))
  inquire(file=trim(frffil), exist=torf)
  if (.not. torf) call err_exit_with_msg('File does not exist: '//trim(frffil))

  !---------------- MAIN ------------------

  ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
  call mk_telem_rows(telfil, tfhead, trows)

  ! Get the FRF
  call mk_frf_rows(trim(get_val_from_key('FRF', argv)), frfhead, frfrows)

  !--- get ASM_sfrows (for relation)
  relrows = get_asm_sfrow(trows, frfrows)
  call update_asm_sfrow_modes(trows, relrows) !, skip_validate=.true.)

  outhead = get_asm_fits_header(tfhead, frfhead, trows, relrows, FITS_CREATOR)
  call write_asm_evt_fits(get_val_from_key('outfile', argv), outhead, trows, relrows, status, comname, argv(:)%val)

  call print_proc_stats(trows, relrows, frfrows)

  if (allocated(trows)) deallocate(trows)
  if (allocated(frfrows)) deallocate(frfrows)
  if (allocated(relrows)) deallocate(relrows)

end program asmmkevt


