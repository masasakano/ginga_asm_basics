program asmtelemetryout

  use iso_fortran_env, only : stderr=>ERROR_UNIT   

  use err_exit
  use fort_util
  use asm_fits_common
  use asm_aux
  use asm_read_telemetry
  use asm_fitsout

  implicit none

!integer, parameter :: dp = kind(1.d0) ! defined in asm_fits_common
  character(len=*), parameter :: Subname = 'main'

  integer :: i, j, n_mainargs, istart_main

  integer :: status=-999 !, hdutype, nframes, naxis1
  logical :: torf

  type(fits_header) :: tfhead, outhead
  !integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
  type(asm_telem_row), dimension(:), allocatable :: trows

  type(fits_header) :: frfhead
  type(asm_frfrow), dimension(:), allocatable :: frfrows
  type(asm_sfrow), dimension(:), allocatable :: relrows

  character(len=1024) :: arg, comname
  type(t_argv), dimension(:), allocatable :: allargv
  !type(t_argv), dimension(3) :: argv
  !argv = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')] 

  !! USAGE: ./asmtelemetryout ../samples/ginga_sirius_P198804280220.fits ../samples/FR880428.S0220.fits ../../../ginga_samples/mkevt_out_test.fits

  !-- Handle the command-line arguments.

if (IS_DEBUG()) print *,'DEBUG:002: command_argument_count()=',command_argument_count()
  allocate(allargv(command_argument_count()))  ! F2003 standard  (_excluding_ $0)
if (IS_DEBUG()) print *,'DEBUG:003: size(allargv)=',size(allargv)
  !allargv(2:4) = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')]
  !allargv(2) = t_argv(key='telemetry')
  !allargv(3) = t_argv(key='FRF')
  !allargv(4) = t_argv(key='outfile')

  i = -1  ! i=0 is for $0 (index for all the original ARGV)
  istart_main = -1  ! Index where the main arugment starts.
  do
    i = i+1
    call get_command_argument(i, arg)  ! Fortran 2003
    if (i == 0) then  ! i=0 is for $0
      comname = trim(arg)
      cycle
    end if
    if (len_trim(arg) == 0) exit

    if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
      print *, 'USAGE: asmtelemetryout [-h] Telemetry.fits FRF.fits out.fits KEY1 [Key2 [Key3 ...]]'
      print *, ' Keys: Tstart|Euler|SFNum|SFNTelem|SF2bits|Fr6bits|i_frame|Status_C|Status_S|DP_C|DP_S' &
                   //'|ACS_C|ASM_C1|ASM_C2|bitrate|ASM_Mode|Slew360|Time_PHA|F56W66B4|'
      print *, ' NOTE: you may set environmental variable GINGA_CHATTER=4 to make it quieter.'
      call EXIT(0)
    end if
    if (istart_main < 0) istart_main = i
    j = i-istart_main+1
    select case(j)
    case(1)
      allargv(j) = t_argv(key='telemetry', val=arg)
      inquire(file=trim(allargv(j)%val), exist=torf)
      if (.not. torf) call err_exit_with_msg('File does not exist: '//trim(allargv(j)%val))
    case(2)
      allargv(j) = t_argv(key='FRF', val=arg)
      inquire(file=trim(allargv(j)%val), exist=torf)
      if (.not. torf) call err_exit_with_msg('File does not exist: '//trim(allargv(j)%val))
    case(3)
      allargv(j) = t_argv(key='outfile', val=arg)
    case default
      allargv(j) = t_argv(key='key'//ladjusted_int(j-3), val=arg)
    end select
  end do

  if (istart_main == -1) then
    n_mainargs = size(allargv)
  else
    n_mainargs = size(allargv) - istart_main + 1
  end if
  if (n_mainargs < 3) call err_exit_with_msg( &
     'The number of the main arguments must be at least 3, but given only '//trim(ladjusted_int(n_mainargs)))
if (IS_DEBUG()) print *,'DEBUG:024: istart_main=',istart_main, ' n_mainargs=',n_mainargs

  call dump_all_argv(allargv) ! DEBUG and for information

  !---------------- MAIN ------------------
    
  ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
  call mk_telem_rows(trim(get_val_from_key('telemetry', allargv)), tfhead, trows)
  !call read_telemetry(trim(get_val_from_key('telemetry', allargv)), tfhead, headers, telems) ! tfhead: Telemetry-Fits-HEADer
  !trows = get_telem_raws2types(headers, telems)
  !call add_mjd2telem(tfhead, trows)

  ! Get the FRF
  call mk_frf_rows(trim(get_val_from_key('FRF', allargv)), frfhead, frfrows)

  !--- get ASM_sfrows (for relation)
  relrows = get_asm_sfrow(trows, frfrows)
  call update_asm_sfrow_modes(trows, relrows, skip_validate=.true.) ! skip_validate: Major difference from asmmkevt.f90

  !outhead = get_merged_head(tfhead, frfhead)
  outhead = get_asm_fits_header(tfhead, frfhead, trows, relrows, status)

  if (n_mainargs == 3) then
    call write_asm_evt_fits(get_val_from_key('outfile', allargv), outhead, trows, relrows, status &
       , comname, allargv(:)%val)
  else
if (IS_DEBUG()) then ! in asm_fits_common
print *,'DEBUG:032: ' 
call dump_all_argv(allargv(istart_main+3:)) ! DEBUG
end if
    call write_asm_evt_fits(get_val_from_key('outfile', allargv), outhead, trows, relrows, status &
       , comname, allargv(:)%val, allargv(istart_main+3:)%val)
  end if

  call print_proc_stats(trows, relrows, frfrows)

  if (allocated(allargv)) deallocate(allargv)
end program asmtelemetryout


