! Common constants in this package
!
module asm_consts
  implicit none 

  integer, parameter, public ::  dp8 = 8 ! FITS file REAL8
  integer, parameter, public ::  ip4 = 4 ! "J" in FITSIO; FITS file INTEGER4
  integer, parameter, public ::  ip2 = 2 ! "I" in FITSIO
  integer, parameter, public :: MAX_FITS_CHAR = 68
  integer, parameter, public :: NUM_INSTR = 6, NCHANS_PHA = 16, NCHANS_TIME = 8  ! Number of channels in each mode 
  real(dp8),    parameter :: UNDEF_DOUBLE = -1024.0_dp8
  real(dp8),    parameter :: UNDEF_REAL   = -1024.0_dp8  ! for backward compatibility (though the name suggests it to be for the standard REAL)
  integer(ip4), parameter :: UNDEF_INT  = -999_ip4
  integer(ip2), parameter :: UNDEF_INT2 = -999_ip2
  integer, parameter :: LEN_READABLE_KEY = 32  ! Char-length of maximum human-readable key name

end module asm_consts
