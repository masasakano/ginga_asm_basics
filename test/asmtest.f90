!
! common routines for testing
!
module test_common
  implicit none
  integer, parameter, public :: dp = kind(1.d0)  ! Common in the test routines.

  !character(len=*), parameter :: SAMPLE_DIR = '../../../../ginga_samples'
  character(len=*), parameter :: SAMPLE_DIR = '../../ginga_samples'
  character(len=*), parameter :: DEF_FNAME_TELEMETRY = SAMPLE_DIR//'/ginga_sirius_R199006190617.fits'
  character(len=*), parameter :: DEF_FNAME_TELEMETRY2= SAMPLE_DIR//'/ginga_sirius_P198804280220.fits'
  character(len=*), parameter :: DEF_FNAME_FRF       = SAMPLE_DIR//'/FR880428.S0220.fits'
  character(len=*), parameter :: DEF_FNAME_OUT       = SAMPLE_DIR//'/asmtest_out.fits'
  integer, dimension(16),  parameter :: HEADERS_T1_I4 = [  &  !  16 defined as parameter in read_telemetry()
     6, 19, 8, 18, 32, 1, 112, 0,  78, 184, 2, 1, 2, 64, 64, 64 &
     ]
  integer, dimension(16),  parameter :: HEADERS_T2_I79 = [  &  ! Row=79(=79th row; F15) of DEF_FNAME_TELEMETRY2
     4, 27, 9, 45, 46, 3,  64, 9, 206,   9, 9, 2, 1,  0,  0,  0 &
     ]
  !integer(kind=1), dimension(16),  parameter :: HEADERS_T1 = int(HEADERS_T1_INT, 1)  !  16 defined as parameter in read_telemetry()
  !integer(kind=1), dimension(16),  parameter :: HEADERS_T1 = int([  &  !  16 defined as parameter in read_telemetry()
  !   6, 19, 8, 18, 32, 1, 112, 0, 78, 184, 2, 1, 2, 64, 64, 64 &
  !   ], 1)
  !integer(kind=1), dimension(16),  parameter :: HEADERS_T1 = [  &  !  16 defined as parameter in read_telemetry()
  !integer(kind=1), dimension(16),  parameter :: HEADERS_T1 = [  &  !  16 defined as parameter in read_telemetry()
  !   6_1, 19_1, 8_1, 18_1, 32_1, 1_1, 112_1, 0_1, 78_1, 184_1, 2_1, 1_1, 2_1, 64_1, 64_1, 64_1 ]

  !integer(kind=1), dimension(128), parameter :: TELEMS_T1 = [  &   ! 128 defined as parameter in read_telemetry()
  integer, dimension(128), parameter :: TELEMS_T1_I4 = [  &   ! 128 defined as parameter in read_telemetry()
       250, 243,  32,  78,   1, 0, 1, 0, 0, 0, 0, 48, 0, 0, 0, 1 &
     , 159,   1, 211,  53, 192, 0, 0, 0, 0, 0, 0,  1, 0, 0, 0, 0 &
     ,  66, 154,   0,  38,   0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0 &
     ,   0,   0,   0,   0,   0, 0, 0, 0, 1, 0, 0,  0, 0, 0, 0, 0 &
     ,  95, 208, 252, 113,   0, 0, 1, 0, 0, 0, 0,  0, 0, 0, 0, 0 &  ! Status(W65)=208, DP(W66)=252, PI_MON=113 
     , 154, 255, 165, 215,   0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0 &
     ,  33,  11,   0,  15,   0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 1, 0 &
     ,   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0,  0, 1, 0, 0, 0 &
     ]
  integer, dimension(128), parameter :: TELEMS_T2_R79 = [  &   ! Row=79(=79th row; F15) of DEF_FNAME_TELEMETRY2
       250, 243,  32, 206,   0,  1,  2,  0,  2,  1,  0,  0,  0,  0,  2,  0 &
     , 123,   9,  99, 247,   0,  2,  1,  1,  0,  1,  0,  0,  0,  1,  0,  1 &
     ,  66,  44, 192,  65,   0,  1,  0,  0,  1,  0,  0,  1,  0,  2,  0,  0 &
     ,  11,   5,  12,   7,   2,  3,  0,  1,  0,  0,  0,  0,  2,  1,  0,  0 &
     ,  89, 208, 244,  33,   0,  0,  1,  0,  0,  0,  0,  1,  0,  1,  0,  1 &
     ,  27, 194, 194,  53,   1,  1,  1,  1,  0,  0,  0,  1,  1,  0,  1,  1 &
     , 153,  95,   4,  84,   1,  1,  0,  0,  1,  1,  0,  0,  1,  0,  1,  0 &
     ,  14,  10,  15,   8,   0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  2,  0 &
     ]

contains
  ! Print the stats info at the end of each subroutine-level testing
  subroutine print_teststats(subname, nsuccess, ifailed)
    character(*), intent(in) :: subname
    integer,      intent(in) :: nsuccess  ! Number of succesful trials, i-th failure
    integer,      intent(in), optional :: ifailed ! i-th failure

    if (present(ifailed)) then
      write(*, '(" Tests(", a, "): Only succeeded in ", i3, " tests before failed at ", i3, "-th.")') &
         trim(subname), nsuccess, ifailed
    else
      write(*, '(" Tests(", a, "): Succeeded in all ", i3, " tests.")') &
         trim(subname), nsuccess
    end if
  end subroutine print_teststats
end module test_common

module test_fort_util
  use unittest
  use err_exit
  use test_common
  use fort_util

  implicit none 
contains

  ! run tests of routines in fort_util.f90
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_fort_util() result(ret_status)
    character(*), parameter :: subname = 'run_test_fort_util'
    integer,      parameter :: TOT_NTESTS = 26
    logical, dimension(TOT_NTESTS) :: ress
    ! logical, external :: btest_int4_as_1byte

    integer :: status=-999
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

    ress = [ &
        assert_equal ('00000001', dump_int1_binary(1_1),   subname, 'dump_int1_binary') &
      , assert_equal ('01111111', dump_int1_binary(127_1), subname, 'dump_int1_binary') &
      , assert_equal ('11111111', dump_int1_binary(-1_1),  subname, 'dump_int1_binary') &
      , assert_equal (3, int4_to_unsigned1(3),   subname, 'for (3(int1=>4))') &
      , assert_equal(-1, int4_to_unsigned1(255), subname, 'for (255(int1=>4))') &
      , assert_equal ('01111111', dump_int_lowest_8bits(127_4), subname, 'dump_int_lowest_8bits(127)') &
      , assert_equal ('01111111', dump_int1_binary(int4_to_unsigned1(127_4)), subname, 'dump_int1_binary(int4-1)') &
      , assert_equal ('11111111', dump_int_lowest_8bits(255_4), subname, 'dump_int_lowest_8bits(255)') &
      , assert_equal ('11111111', dump_int1_binary(int4_to_unsigned1(255_4)), subname, 'dump_int1_binary(int4-2)') &
      , assert_equal ('01111111', dump_int_lowest_8bits(383_4), subname, 'dump_int_lowest_8bits(383)') &
      , assert_equal ('11111111', dump_int_lowest_8bits(-1_4),  subname, 'dump_int_lowest_8bits(-1)') &
      , assert_equal ('01111111', dump_int_lowest_8bits(127_4, ensure_range=.true.), subname, 'lowest_8(127,ensure)') &
      , assert_equal ('UNDEF', trim(dump_int_lowest_8bits(-1_4, ensure_range=.true.)), subname, 'lowest_8(-1,ensure)') &
      , assert_equal ('UNDEF', trim(dump_int_lowest_8bits(256, ensure_range=.true.)), subname, 'lowest_8(256,ensure)') &
      , assert_equal ('00000000_00000000_00000000_01111111', dump_int_binary(127_4), subname, 'dump_int4_binary(127)') &
      , assert_equal ('00000000_00000000_00000001_01111111', dump_int_binary(383_4), subname, 'dump_int4_binary(383)') &
      , assert_equal ('11111111_11111111_11111111_11111111', dump_int_binary(-1_4),  subname, 'dump_int4_binary(-1)') &
      , assert(    btest_int4_as_1byte(  1, 7), subname, 'for (  1,7)='//dump_int1_binary(int4_to_unsigned1_scalar(1)))   & ! TRUE  (b00000001)
                                                                                                                            !                ^
      , assert_not(btest_int4_as_1byte(  2, 7), subname,' for (  2,7)='//dump_int1_binary(int4_to_unsigned1_scalar(2)))   & ! FALSE (b00000010)
                                                                                                                            !                ^
      , assert(    btest_int4_as_1byte(  2, 6), subname, 'for (  2,6)='//dump_int1_binary(int4_to_unsigned1_scalar(2)))   & ! TRUE  (b00000010)
                                                                                                                            !               ^ 
      , assert_not(btest_int4_as_1byte(254, 7), subname,' for (254,7)='//dump_int1_binary(int4_to_unsigned1_scalar(254))) & ! FALSE (b11111110)
                                                                                                                            !                ^
      , assert(    btest_int4_as_1byte(255, 7), subname, 'for (255,7)='//dump_int1_binary(int4_to_unsigned1_scalar(255))) & ! TRUE  (b11111111)
                                                                                                                            !                ^
      , assert_equal(1, btest2int_int4_as_1byte(  2, 6), subname, 'Int for (  2,6)=') & ! TRUE  (b00000010)
      , assert_equal(0, btest2int_int4_as_1byte(254, 7), subname, ' Int for (254,7)=') & ! FALSE (b11111110)
      , assert_equal('c.d', trim(basename('/a/b/c.d')),  subname, ' basename(/a/b/c.d)') &
      , assert_equal('c.d', trim(basename('c.d')),       subname, ' basename(/a/b/c.d)') &
      ]

    do j=1, TOT_NTESTS
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
  end function run_test_fort_util
end module test_fort_util

!
! for fits_common
!
module test_fits_common
  use iso_fortran_env, only : stderr=>ERROR_UNIT
  use unittest
  use err_exit
  use test_common
  use asm_fits_common

  implicit none 
contains
  subroutine run_test_fits_common
  end subroutine run_test_fits_common

  ! run tests of get_ttype (asm_fits_common.f90)
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_get_ttype() result(ret_status)
    character(*), parameter :: subname = 'run_test_get_ttype'
    type t_ts_get_ttype
      integer :: i;
      character(64) :: str;
    end type t_ts_get_ttype
    type(t_ts_get_ttype), dimension(7), parameter :: expecteds = (/&
         t_ts_get_ttype( 1, 'Y11CH00/Y11L00') &
       , t_ts_get_ttype( 9, 'Y11CH08/Y11H00') &
       , t_ts_get_ttype(17, 'Y21CH00/Y21L00') &
       , t_ts_get_ttype(32, 'Y21CH15/Y21H07') &
       , t_ts_get_ttype(33, 'Y12CH00/Y12L00') &
       , t_ts_get_ttype(96, 'Y23CH15/Y23H07') &
       , t_ts_get_ttype(13, 'Y11CH12/Y11H04') &
       /)

    character(len=99) :: optmsg
    integer :: j
    integer :: ntests = 0
    logical :: res

    ret_status = 0  ! normal ends (n.b., returned value)

    do j=1, size(expecteds)
      ntests = ntests + 1
      write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      res = assert_equal(expecteds(j)%str, get_ttype_main(expecteds(j)%i), trim(subname), optmsg)
      !res = assert_equal(expecteds(j)%str, get_tform(expecteds(j)%i), trim(subname), optmsg)
      if (res) cycle

      ! Failed
      write(*, '(" Tests(", a, "): Only succeeded in ", i3, " tests before failed at ", i3, "-th.")') &
         trim(subname), ntests-1, j
      ret_status = 1
      return
    end do
    write(*,   '(" Tests(", a, "): Succeeded in all ", i3, " tests.")') &
         trim(subname), ntests
  end function run_test_get_ttype

  ! run tests of misc in fits_common
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_get_colheads() result(ret_status)
    character(*), parameter :: subname = 'run_test_get_colheads'
    integer,      parameter :: TOT_NTESTS = 13
    logical, dimension(TOT_NTESTS) :: ress

    character(len=8), dimension(3), parameter :: ChKeys = ['Tstart ', 'Euler  ', 'Fr6bits']
    character(len=6), dimension(3), parameter :: char2tests = ['someth', '      ', 'main  ']
    character(len=8) :: sk
    type(t_asm_colhead), dimension(size(ChKeys)) :: colheads1
    !type(t_form_unit) :: tmpfu
    type(t_asm_colhead), dimension(:), allocatable :: colheads, colheads2

    integer :: status=-999
    integer :: j, exq1

    ret_status = 0  ! normal ends (n.b., returned value)

    exq1 = size(COL_FORM_UNITS) - 2 + NWORDS_MAIN + 3  ! nb, 3 for Euler1-3

    do j=1, size(ChKeys)
      sk = ChKeys(j)
      colheads1(j) = t_asm_colhead(key=sk, type=trim(sk), prm=get_element(sk, COL_FORM_UNITS))
    end do

    colheads  = get_colheads()
    colheads2 = get_colheads(ChKeys)

    ress = [ &
        assert_equal(exq1, get_ncols_colheads(), trim(subname), 'get_ncols_colheads()') &
      , assert_equal(2,    get_ncols_colheads(COL_FORM_UNITS(4:5)), trim(subname), 'get_ncols_colheads(4:5)') &
      , assert_equal(5,    get_ncols_colheads(ChKeys), trim(subname), 'get_ncols_colheads(3-Chars)') &
      , assert_equal(5,    get_ncols_colheads(colheads1), trim(subname), 'get_ncols_colheads(colheads1)') &
      , assert_equal(exq1, size(get_colheads()), trim(subname), 'size(get_colheads())') &
      , assert(allocated(colheads2), trim(subname), 'allocated? colheads2') &
      , assert_equal(5,    size(colheads2), trim(subname), 'size(colheads2)') &
      , assert_equal('Tstart', trim(colheads2(1)%type), trim(subname), 'colheads2(1)%type') &
      , assert_equal('Euler2', trim(colheads2(3)%type), trim(subname), 'colheads2(3)%type') &
      , assert_equal('Euler3', trim(colheads2(4)%type), trim(subname), 'colheads2(4)%type') &
      , assert_equal('Y11CH00/Y11L00', trim(colheads(1)%type), trim(subname), 'colheads(1)%type') &
      , assert_equal('Y11CH08/Y11H00', trim(colheads(9)%type), trim(subname), 'colheads(9)%type') &
      , assert_equal('Y23CH15/Y23H07', trim(colheads(96)%type),trim(subname), 'colheads(96)%type') &
      ]

    do j=1, TOT_NTESTS
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    if (allocated(colheads2)) deallocate(colheads2)
  end function run_test_get_colheads

  ! run tests of misc in fits_common
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_common_misc() result(ret_status)
    character(*), parameter :: subname = 'run_test_common_misc'
    integer,      parameter :: TOT_NTESTS = 8
    logical, dimension(TOT_NTESTS) :: ress

    character(len=6), dimension(3), parameter :: char2tests = ['someth', '      ', 'main  ']
    type(t_form_unit) :: tmpfu
    type(t_asm_colhead), dimension(:), allocatable :: colheads

    integer :: status=-999
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

!print *,'DEBUG:711:test:'
    tmpfu = get_element('main', COL_FORM_UNITS)
!print *,'DEBUG:712:test:',tmpfu

    colheads = get_colheads()

    ress = [ &
        assert_equal(3, get_index('main', char2tests), trim(subname), 'get_index') &
      , assert_smaller_than(0, get_index('naiyo', char2tests, silent=.true.), trim(subname), 'get_index') &
      , assert_equal('main', trim(get_element('main', char2tests)), trim(subname), 'get_element') &
      , assert_equal(1, get_index('main', COL_FORM_UNITS), trim(subname), 'get_index_fu') &
      , assert_equal(96, tmpfu%dim, trim(subname), 'get-96') &
      , assert_smaller_than(0, get_index('naiyo', COL_FORM_UNITS, silent=.true.), trim(subname), 'get_index') &
      , assert_greater_than(96, size(colheads), trim(subname), 'size(colheads)') &
      , assert_equal(colheads(8)%key, colheads(8)%prm%key, trim(subname), 'colheads key') &  ! 8 is random
      !, assert_equal(colheads%key, colheads%prm%key, trim(subname), 'colheads key') &
      ]

    do j=1, TOT_NTESTS
      if (ress(j)) cycle

!print *,'DEBUG:713:test-fail:'
      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
!print *,'DEBUG:714:test-sucx:'
  end function run_test_common_misc
end module test_fits_common

!
! for asm_read_telemetry
!
module test_read_telemetry
  use unittest
  use err_exit
  use test_common
  use asm_fits_common
  use asm_read_telemetry

  implicit none 
contains
  ! run tests of read_telemetry
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_read_telemetry() result(ret_status)
    character(*), parameter :: subname = 'run_test_read_telemetry'
    integer,      parameter :: TOT_NTESTS = 2
    logical, dimension(TOT_NTESTS) :: ress

    type(fits_header) :: fhead
    integer(kind=1), dimension(:, :), allocatable :: headers, telems ! (word, row)

    integer :: status=-999
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

    call read_telemetry(DEF_FNAME_TELEMETRY, fhead, headers, telems)
    ress = [ &
        assert_equal(HEADERS_T1_I4, headers(:, 1), trim(subname), .true., 'for Headers') &
      , assert_equal(TELEMS_T1_I4,  telems(:, 1),  trim(subname), .true., 'for Telems') &
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    if (allocated(headers)) deallocate(headers, STAT=status)
    if (allocated(telems))  deallocate(telems,  STAT=status)
  end function run_test_read_telemetry

  ! run tests of get_telem_raws2types()
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_get_telem_raws2types() result(ret_status)
    character(*), parameter :: subname = 'run_test_get_telem_raws2types'
    integer,      parameter :: TOT_NTESTS = 11
    type(fits_header) :: fhead
    integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
    type(asm_telem_row), dimension(:), allocatable :: telm_rows

    integer :: j
    logical, dimension(TOT_NTESTS) :: ress

    ret_status = 0  ! normal ends (n.b., returned value)

    call read_telemetry(DEF_FNAME_TELEMETRY, fhead, headers, telems)
    telm_rows = get_telem_raws2types(headers, telems)

    ress = [ &
         assert_equal(  1, telm_rows(1)%i_frame,     subname, 'i_frame in Telm-Header') &
       , assert_equal(HEADERS_T1_I4(1), telm_rows(1)%month, subname, 'for Day in Telm-Header') &
       , assert_equal(368, telm_rows(1)%millisec_i4, subname, 'for millisec_i4 from Telm-Header') &
           ! Byte(1, 112) => 368, which is consistent with "DATE-OBS: 1990-06-19T08:18:32.3680"
       , assert_equal( 78, telm_rows(1)%w_fi,    subname, 'for FI in Telemetry') &
       , assert_equal(telm_rows(1)%w_fi, telm_rows(1)%Counter_A2, subname, 'Frame for Telemetry head/main') &
       , assert_equal(  1, telm_rows(1)%sf_2bit, subname, 'for sf_2bit from Telemetry') &
       , assert_equal( 14, telm_rows(1)%fr_6bit, subname, 'for fr_6bit from Telemetry') &
       , assert_equal(208, TELEMS_T1_I4(w_no('status', from1=.true.)), subname, 'for sanity: TELEMS_T1_I4') &
       , assert_equal(TELEMS_T1_I4(w_no('status', from1=.true.)), telm_rows(1)%STAT_OBS, subname, 'for Status from Tel') &
       , assert_equal(TELEMS_T1_I4(w_no('dp',     from1=.true.)), telm_rows(1)%DPID_OBS, subname, 'for DP from Tel') &
       , assert_equal(TELEMS_T1_I4(w_no('pi_mon', from1=.true.)), telm_rows(1)%pi_mon,   subname, 'for PI_MON from Tel') &
         ! Status(STAT_OBS:W65)=208, DP(DPID_OBS:W66)=252, PI_MON=113
       ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    
    deallocate(headers)
    deallocate(telems)
    deallocate(telm_rows)

    call read_telemetry(DEF_FNAME_TELEMETRY2, fhead, headers, telems)
    telm_rows = get_telem_raws2types(headers, telems)
!call dump_asm_telem_row(telm_rows(1))
!call dump_asm_telem_row(telm_rows(2))
    deallocate(headers)
    deallocate(telems)
    deallocate(telm_rows)

  end function run_test_get_telem_raws2types

  ! run tests of add_mjd2telem()
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_add_mjd2telem() result(ret_status)
    character(*), parameter :: subname = 'run_add_mjd2telem'
    integer,      parameter :: TOT_NTESTS = 7  ! Number of tests
    logical, dimension(TOT_NTESTS) :: ress

    type(fits_header) :: tfhead, tfhead2
    integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
    type(asm_telem_row), dimension(:), allocatable :: telm_rows
    type(asm_telem_row), dimension(4) :: trows2
    integer :: j
    integer, parameter :: SF2F4  = 1*64+4  ! =68
    integer, parameter :: SF2F15 = 1*64+15 ! =79
    integer, parameter :: IROW_STATUS = TELEM_WORD_FROM0%status + 1 ! =65+1
    integer, parameter :: IROW_DP     = TELEM_WORD_FROM0%dp + 1     ! =66+1

    ret_status = 0  ! normal ends (n.b., returned value)

    ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
    call read_telemetry(DEF_FNAME_TELEMETRY2, tfhead, headers, telems)
    telm_rows = get_telem_raws2types(headers, telems)
    call add_mjd2telem(tfhead, telm_rows)
call dump_asm_telem_row(telm_rows(1))  ! DEBUG
call dump_asm_telem_row(telm_rows(2))  ! DEBUG
call dump_asm_telem_row(telm_rows(SF2F4))  ! =68(F4)  for DEBUG
call dump_asm_telem_row(telm_rows(SF2F15)) ! =79(F15) for DEBUG

    ! Test data of transition from the end of a year to the next.
    tfhead2%DATE__OBS%val   = '1988-12-31T23:59:56.1230'
    do j=1, 2
      trows2(j)%month      = 12
      trows2(j)%day        = 31
      trows2(j)%hour       = 23
      trows2(j)%minute     = 59
      trows2(j)%second     = 56
      trows2(j)%millisec_i4 = 123
    end do
    trows2(2)%second = 58
    do j=3, 4
      trows2(j)%month      = 1
      trows2(j)%day        = 1
      trows2(j)%hour       = 0
      trows2(j)%minute     = 0
      trows2(j)%second     = 0
      trows2(j)%millisec_i4 = 123
    end do
    trows2(4)%second = 2

    !!write(*,'("DEBUG: trows2(:)%month=",I2)') trows2(:)%month 
    !do j=1, 4
    !  write(*,'("DEBUG: trows2(",i1.1,")%month=",I2)') j, trows2(j)%month 
    !end do

    write(stderr,'("NOTE: A warning re year-increase should be printed immediately below.")')
    call add_mjd2telem(tfhead2, trows2)  ! add mjd, year 

    ress = [ &
        assert_equal(1988,       telm_rows(1)%year,    subname, 'Year=1988(!?)') &
      , assert_in_delta(4.727840690E+04_dp, telm_rows(1)%mjd, 7e-4_dp, subname, 'MJD=4.727840690E+04') & ! 1-minute precision; nb., 1e-8 for 1 ms
      , assert_equal(1988,       trows2(1)%year,    subname, 'trows2(1)%year=1988(!?)') &
      , assert_equal(1988,       trows2(2)%year,    subname, 'trows2(2)%year=1988(!?)') &
      , assert_equal(1989,       trows2(3)%year,    subname, 'trows2(3)%year=1989(!?)') &
      , assert_equal(int4_to_unsigned1(TELEMS_T2_R79(IROW_STATUS)), telems(IROW_STATUS, SF2F15), subname, 'SF2F15-Status') &
      , assert_equal(int4_to_unsigned1(TELEMS_T2_R79(IROW_DP)),     telems(IROW_DP,     SF2F15), subname, 'SF2F15-DP') &
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    deallocate(headers)
    deallocate(telems)
    deallocate(telm_rows)
  end function run_add_mjd2telem

  ! run tests of mk_telem_rows()
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_mk_telem_rows() result(ret_status)
    character(*), parameter :: subname = 'run_mk_telem_rows'
    integer,      parameter :: TOT_NTESTS = 2  ! Number of tests
    logical, dimension(TOT_NTESTS) :: ress

    type(fits_header) :: tfhead
    integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
    type(asm_telem_row), dimension(:), allocatable :: telm_rows
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

    ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
    call read_telemetry(DEF_FNAME_TELEMETRY2, tfhead, headers, telems)
    telm_rows = get_telem_raws2types(headers, telems)
    call add_mjd2telem(tfhead, telm_rows)
    deallocate(headers)
    deallocate(telems)

    ress = [ &
        assert_equal(1988,       telm_rows(1)%year,    subname, 'Year=1988(!?)') &
      , assert_in_delta(4.727840690E+04_dp, telm_rows(1)%mjd, 7e-4_dp, subname, 'MJD=4.727840690E+04') & ! 1-minute precision; nb., 1e-8 for 1 ms
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    deallocate(telm_rows)
  end function run_mk_telem_rows

  ! run tests of read_frf
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_read_frf() result(ret_status)
    character(*), parameter :: subname = 'run_test_read_frf'
    integer,      parameter :: TOT_NTESTS = 3  ! Number of tests
    logical, dimension(TOT_NTESTS) :: ress

    type(asm_frfrow), dimension(:), allocatable :: sfrows
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

    sfrows = get_frf_types(DEF_FNAME_FRF)
    !print *, 'DEBUG: size(sfrows)=', size(sfrows)
    call dump_asm_frfrow(sfrows(1))
    call dump_asm_frfrow(sfrows(2))

    ress = [ &
        assert_equal(  27,       sfrows(2)%stime(3),    subname, 'for STIME(DAY) for i=2') &
      , assert_equal(   3,       sfrows(2)%sfn,         subname, 'for SFN for i=2') &
      , assert_in_delta(3.23861_dp, sfrows(2)%eulers(1,1), 0.0001_dp, subname, 'for Euler1 for i=2') &
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
  end function run_test_read_frf

  ! run tests of mk_frf_rows
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_mk_frf_rows() result(ret_status)
    character(*), parameter :: subname = 'run_mk_frf_rows'
    integer,      parameter :: TOT_NTESTS = 6  ! Number of tests
    logical, dimension(TOT_NTESTS) :: ress

    type(fits_header) :: frfhead
    type(asm_frfrow), dimension(:), allocatable :: sfrows
    integer :: j

    ret_status = 0  ! normal ends (n.b., returned value)

    call mk_frf_rows(DEF_FNAME_FRF, frfhead, sfrows)

    ress = [ &
        assert_equal(  27,       sfrows(2)%stime(3),    subname, 'for STIME(DAY) for i=2') &
      , assert_equal(   3,       sfrows(2)%sfn,         subname, 'for SFN for i=2') &
      , assert_in_delta(3.23861_dp, sfrows(2)%eulers(1,1), 0.0001_dp, subname, 'for Euler1 for i=2') &
      , assert_equal('GINGA_FRF', frfhead%EXTNAME%val,  subname, 'for EXTNAME') &
      , assert_equal(36048,       frfhead%NAXIS2%val,   subname, 'for NAXIS2') &
      , assert_equal(frfhead%TOTAL_SF%val, frfhead%TOTSFFRF%val, subname, 'for TOTAL_SF=TOTSFFRF') &
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)
    deallocate(sfrows)
  end function run_mk_frf_rows
end module test_read_telemetry

!
! for asm_read_telemetry
!
module test_asm_fitsout
  use unittest
  use err_exit
  use test_common
  use asm_fits_common
  use asm_read_telemetry  ! required
  use asm_fitsout

  implicit none 
contains
  ! run tests of get_asm_sfrow
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_get_asm_sfrow() result(ret_status)
    character(*), parameter :: subname = 'run_test_get_asm_sfrow'
    integer,      parameter :: TOT_NTESTS = 8  ! Number of tests
    logical, dimension(TOT_NTESTS) :: ress
    logical, dimension(TOT_NTESTS) :: tfs
    character(128), dimension(TOT_NTESTS) :: msg

    type(fits_header) :: tfhead
    integer(kind=1), dimension(:, :), allocatable :: headers, telems !  (word, row)
    type(asm_telem_row), dimension(:), allocatable :: trows

    type(fits_header) :: frfhead
    type(asm_frfrow), dimension(:), allocatable :: frfrows
    character(len=LEN_PROC_STATS), dimension(:), allocatable :: ar_strs_stats

    type(asm_sfrow), dimension(:), allocatable :: relrows0, relrows
    integer :: j, status
    integer :: sumf
    integer(kind=ip4), dimension(7) :: time ! (YYYY, MM, DD, hh, mm, ss, msec)
    real(kind=dp8) :: mjd

    tfs(:) = .false.
    ret_status = 0  ! normal ends (n.b., returned value)

    ! Get telm_rows from the default data with add_mjd2telem(tfhead, telm_rows)
    call read_telemetry(DEF_FNAME_TELEMETRY2, tfhead, headers, telems) ! tfhead: Telemetry-Fits-HEADer
    trows = get_telem_raws2types(headers, telems)
    call add_mjd2telem(tfhead, trows)

    ! Get the FRF
    call mk_frf_rows(DEF_FNAME_FRF, frfhead, frfrows)

print *, 'DEBUG:780: size(frfrows)=',size(frfrows), ' size(trows)=',size(trows)
    !--- get initialized ASM_sfrows (for relation)
    relrows0 = get_ams_sfrow_init(trows)

    sumf = 0
    do j=1, size(relrows0)
      sumf = sumf + relrows0(j)%nframes 
      if ((relrows0(j)%nframes < 0) .or. (64 < relrows0(j)%nframes)) then 
        print *, 'DEBUG781:ERROR: row=',j,' nframes=',relrows0(j)%nframes,' irowt=',relrows0(j)%irowt
      end if
    end do

!print *, 'DEBUG791:'
!call dump_asm_telem_row(trows(15104))
!call dump_asm_telem_row(trows(15105))
!call dump_asm_telem_row(trows(15168))
!call dump_asm_telem_row(trows(15169))
!call dump_asm_telem_row(trows(15232))
!call dump_asm_telem_row(trows(15233))

    ! Prepartion of messages for the tests with relrows0
    j=3
    write(msg(j), '("(size(trows)=(",i5,") > size(relrows0)=(",i5,"))")') size(trows), size(relrows0)
print *, 'DEBUG794: size(relrows0)=', trim(msg(j))
    j=4
    if (any(relrows0%nframes < 0)) tfs(j) = .true.  ! Should be false.
    !if (all(relrows0%nframes > 0)) tfs(j) = .true. ! Should be true. (Equivalent to the above.
    msg(j) = 'There are negative nframes in relrows0'
    j=5
    write(msg(j), '("(size(trows)=(",i5,") == sumf=(",i8,")")') size(trows), sumf

!!------------DEBUG
!!    FRF(1)%mjd = 47278.406896203698
!time = [1988, 4, 27, 9, 45, 7, 832]  ! from Telemetry FITS header.
!call MJULIA(time, mjd)  ! mjd.f in ginga_tool
!print *, 'DEBUG: MJD=',mjd,' for 1988-04-27T09:45:07.8320'  ! => 47278.406340648187
!!                           --------- asm_telem_row (           1 ) ---------
!!                       TIME: 04-27 09:45:07. 832(7.8320) = MJD( 47278.406340648187)
!                                                   ! fr(1)mjd=   47278.406896203698  ! 48 sec later (96frames/Bit-M)
!!        161  mjd= 47278.407266574097
!! irowt= 225  mjd= 47278.407636944437,  3  fr(irow)mjd=   47278.407636944437

    !--- get ASM_sfrows (for relation)
    relrows = get_asm_sfrow(trows, frfrows)

    !j=8
    !write(msg(j), '("(size(trows)=(",i5,") > size(relrows)=(",i5,")")') size(trows), size(relrows)

print *, 'DEBUG:135: update'
    !call update_asm_sfrow_modes(trows, relrows, skip_validate=.true.)
    call update_asm_sfrow_modes(trows, relrows)

    ar_strs_stats = calc_proc_stats(trows, relrows)  ! allocatable
    write(*,'("----------- Processing Statistics -----------")')
    do j=1, size(ar_strs_stats)
      write(*,'(A)') trim(ar_strs_stats(j))
    end do
    write(*,'("---------------------------------------------")')
    if (allocated(ar_strs_stats)) deallocate(ar_strs_stats)
 
    !------------ Run
print *,'DEBUG:123:start'
call write_tmp_fits(DEF_FNAME_OUT, status)
print *,'DEBUG:124:start'
    !call write_asm_fits(trim(DEF_FNAME_OUT), tfhead, trows, frfrows, relrows, status)
print *,'DEBUG:888:end writing'
    !------------

    ress = [ &
      !--- tests of relrows0
        assert_greater_than(0, size(relrows0), subname, '(0 < size(relrows0))?') &
      , assert_equal(1, relrows0(1)%irowt, subname, '(1==relrows0(1)%irowt)?') &
      , assert_smaller_than(size(trows), size(relrows0), subname, trim(msg(3))) &
      , assert_not(tfs(4), subname, trim(msg(4))) &
      , assert_equal(size(trows), sumf, subname, trim(msg(5))) &
      !--- tests of relrows
      , assert_equal(size(relrows0), size(relrows), subname, '(size(relrows0) == size(relrows))?') &
      !, assert_smaller_than(size(trows), size(relrows), subname, trim(msg(8))) &  ! redundant
      , assert_equal(3, frfrows(2)%sfn,   subname, 'for SFN for i=2') & ! The first Telemetry frame does not exist in FRF
      , assert_equal(0, frfrows(2)%lostf, subname, 'for lostf for i=2') &
      !, assert_in_delta(3.23861_dp, frfrows(2)%eulers(1,1), 0.0001_dp, subname, 'for Euler1 for i=2') &
      ]

    do j=1, TOT_NTESTS
      ! write(optmsg, '("for get_ttype(i=", i2, ")")') expecteds(j)%i
      if (ress(j)) cycle

      ! Failed
      call print_teststats(subname, nsuccess=j-1, ifailed=j)
      ret_status = 1
      return
    end do
    call print_teststats(subname, TOT_NTESTS)

    do j=1, 3 !size(relrows)
!do j=1, size(relrows)
      call dump_asm_sfrow(relrows(j), irow=j)
    end do
    call dump_asm_sfrow(get_sfrow_from_telem_row_index(79, relrows))

    deallocate(headers)
    deallocate(telems)
    deallocate(frfrows)
    deallocate(relrows0)
    deallocate(relrows)
  end function run_test_get_asm_sfrow
end module test_asm_fitsout

! -------------------------------------------------------------------

program asmtest
  use test_fits_common
  use test_fort_util
  use test_read_telemetry
  use test_asm_fitsout

  implicit none
  integer :: status = 0

if (.true.) then
  status = status + run_test_fort_util()
  status = status + run_test_get_ttype()
  status = status + run_test_read_telemetry()
  status = status + run_test_get_telem_raws2types()
  status = status + run_add_mjd2telem()
  status = status + run_mk_telem_rows()
  status = status + run_test_read_frf() 
  status = status + run_mk_frf_rows() 
  status = status + run_test_get_asm_sfrow()
  status = status + run_test_common_misc() 
  status = status + run_test_get_colheads() 
else
print *, 'DEBUG997: after run_test_get_telem_raws2types()'  ! Without this, SEGfault...
  status = status + run_add_mjd2telem()
end if
  if (status .ne. 0) then
    call EXIT(status)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
  end if
end program asmtest

