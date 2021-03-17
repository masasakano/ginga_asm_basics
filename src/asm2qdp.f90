program asm2qdp

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
  integer, parameter :: DefNChans = 2, DefNBands = 2

  integer :: i, j, n_mainargs, istart_main, nargs
  logical :: torf

  integer, dimension(DefNChans, DefNBands) :: chans  ! ((Low,High), i-th-band)

  character(len=1024) :: arg, comname = ''
  type(t_argv), dimension(:), allocatable :: allargv
  character(len=255) :: env_ginga_chatter
  integer :: chatter = -99  ! read only once
  !type(t_argv), dimension(3) :: argv
  !argv = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')] 

  !! USAGE: ./asmtelemetryout ../../../ginga_samples/ginga_sirius_P198804280220.fits ../../../ginga_samples/FR880428.S0220.fits ../../../ginga_samples/mkevt_out_test.fits

  !-- Handle the command-line arguments.

  if (chatter < 0) then
    call GET_ENVIRONMENT_VARIABLE('GINGA_CHATTER', env_ginga_chatter)
    read(env_ginga_chatter, '(I2)') chatter
    if (chatter == 0) chatter = 10
  end if

  nargs = command_argument_count()
if (IS_DEBUG()) print *,'DEBUG:002: command_argument_count()=',command_argument_count()
  allocate(allargv(nargs))  ! F2003 standard  (_excluding_ $0)
if (IS_DEBUG()) print *,'DEBUG:003: size(allargv)=',size(allargv)
  !allargv(2:4) = [t_argv(key='telemetry'), t_argv(key='FRF'), t_argv(key='outfile')]
  !allargv(2) = t_argv(key='telemetry')
  !allargv(3) = t_argv(key='FRF')
  !allargv(4) = t_argv(key='outfile')

  istart_main = -1  ! Index where the main arugment starts.
  do i=0, nargs
    call get_command_argument(i, arg)  ! Fortran 2003
    if (i == 0) then  ! i=0 is for $0
      comname = trim(arg)
      cycle
    end if
    ! if (len_trim(arg) == 0) exit

    if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
      print *, 'USAGE: '//trim(basename(comname))//' [-h] ASM.fits OUT_ROOT [CH_A CH_B]'
      print *, ' Output: OUT_ROOT.qdp and OUT_ROOT.pco'
      print *, ' NOTE: If CH_A and B (0..15) are specified, two channnels are output.'
      print *, '       Otherwise CH00-07 and 08-15 are output.'
      call EXIT(0)
    end if
    if (istart_main < 0) istart_main = i
    j = i-istart_main+1
    select case(j)
    case(1)
      allargv(j) = t_argv(key='asmfits', val=arg)
      inquire(file=trim(allargv(j)%val), exist=torf)
      if (.not. torf) call err_exit_with_msg('File does not exist: '//trim(allargv(j)%val))
    case(2)
      allargv(j) = t_argv(key='outfile', val=arg)
    case(3, 4)
      allargv(j) = t_argv(key='arg_ch'//ladjusted_int(j-2), val=arg)
    case default
    end select
  end do

  if (3 < chatter) call dump_all_argv(allargv) ! DEBUG and for information

  if (istart_main == -1) then
    n_mainargs = size(allargv)
  else
    n_mainargs = size(allargv) - istart_main + 1
  end if
  if ((n_mainargs .ne. 2) .and. (n_mainargs .ne. 4)) call err_exit_with_msg( &
     'The number of the main arguments must be either 2 or 4, but given '//trim(ladjusted_int(n_mainargs)))
if (IS_DEBUG()) print *,'DEBUG:024: istart_main=',istart_main, ' n_mainargs=',n_mainargs

  select case(nargs)
  case(2)
    !chans(:, 1) = [0, 7]
    !chans(:, 2) = [8, 15]
    chans(1, 1) = 0  ! Default: 00-07 ch
    chans(2, 1) = 7
    chans(1, 2) = 8  ! Default: 08-15 ch
    chans(2, 2) = 15
  case(4)
    !read(trim(get_val_from_key('arg_ch1', allargv)), '(I2)') i
    chans(:, 1) = get_int_from_key('arg_ch1', allargv)
    !read(trim(get_val_from_key('arg_ch2', allargv)), '(I2)') i
    chans(:, 2) = get_int_from_key('arg_ch2', allargv)
  case default
    call err_exit_play_safe() ! should never happen
  end select
  
  !---------------- MAIN ------------------
    
if (IS_DEBUG()) print *,'DEBUG:992: starting... chans=',chans, ' asmfits=', trim(get_val_from_key('asmfits', allargv))
  call read_asm_write_qdp(get_val_from_key('asmfits', allargv) &
                        , get_val_from_key('outfile', allargv), chans)

  if (allocated(allargv)) deallocate(allargv)
end program asm2qdp


