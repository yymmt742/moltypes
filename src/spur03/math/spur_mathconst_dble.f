module spur_mathconst_dble
implicit none
public
  double precision,parameter :: NAPIER    = 2.7182818284590452353602874d0
  double precision,parameter :: PI        = 3.1415926535897932384626433d0
  double precision,parameter :: ANG2RAD   = 1.74532925199432957690768d-2     ! PI/180
  double precision,parameter :: RAD2ANG   = 1.8000000000000000000000000d2 / PI ! 180/PI
  double precision,parameter :: HTR2KCM   = 6.27509d2
  double precision,parameter :: EC2KCM    = 3.320522d2                     !
  double precision,parameter :: HTR2EV    = 2.721162d1
  double precision,parameter :: HTR2NM    = 4.556288d1
  double precision,parameter :: KCM2NM    = 2.85907104d4
  double precision,parameter :: EV2KCM    = 2.306d2
  double precision,parameter :: EV2NM     = 1.23984d3
  double precision,parameter :: BOHR      = 5.291772106712d-1              ! [Angstr]
  double precision,parameter :: ANG2BOHR  = 1.d0/BOHR
  double precision,parameter :: AU2DEBYE  = 4.803204544d0
  double precision,parameter :: DEBYE2AU  = 1.d0/4.803204544d0
  double precision,parameter :: ELCON     = 8.854187817d0                  ! [10e-12 F/m]
  double precision,parameter :: ELCHG     = 1.602176620898d0               ! [10e-19 C]
  double precision,parameter :: PLANK     = 6.62607004081d0                ! [10e-34 Js]
  double precision,parameter :: DIRAC     = 1.05457180013d0                ! [10e-34 Js]
!
  double precision,parameter :: ZERO      = 0.0000000000000000d0
  double precision,parameter :: HALF      = 0.5000000000000000d0
  double precision,parameter :: ONE       = 1.0000000000000000d0
!
  double precision,parameter :: PETA      = 1.0000000000000000d15
  double precision,parameter :: TERA      = 1.0000000000000000d12
  double precision,parameter :: GIGA      = 1.0000000000000000d9
  double precision,parameter :: MEGA      = 1.0000000000000000d6
  double precision,parameter :: KILO      = 1.0000000000000000d3
  double precision,parameter :: MILLI     = 1.0000000000000000d-3
  double precision,parameter :: MICRO     = 1.0000000000000000d-6
  double precision,parameter :: NANO      = 1.0000000000000000d-9
  double precision,parameter :: PICO      = 1.0000000000000000d-12
  double precision,parameter :: FEMTO     = 1.0000000000000000d-15
!
  double precision,parameter :: SQRT2     = 1.4142135623730950d0
  double precision,parameter :: SQRT3     = 1.7320508075688773d0
  double precision,parameter :: SQRT5     = 2.2360679774997897d0
  double precision,parameter :: SQRT7     = 2.6457513110645906d0
  double precision,parameter :: GOLD      = 1.6180339887498948d0
  double precision,parameter :: SILVER    = 2.4142135623730950d0
  double precision,parameter :: EULER     = 2.71828182845904524d0
contains
  pure elemental function convert_si(from,to,v) result(res)
  character(*),intent(in),optional     :: from,to
  double precision,intent(in),optional :: v
  double precision                     :: res
    res = ONE
    if(present(v)) res = v
!
    if(present(from))then
      select case(from(1:1))
      case ('f') ; res = res * FEMTO
      case ('p') ; res = res * PICO
      case ('n') ; res = res * NANO
      case ('u') ; res = res * MICRO
      case ('m') ; res = res * MILLI
      case ('k') ; res = res * KILO
      case ('M') ; res = res * MEGA
      case ('G') ; res = res * GIGA
      case ('T') ; res = res * TERA
      case ('P') ; res = res * PETA
      end select
    endif
!
    if(present(to))then
      select case(to(1:1))
      case ('P') ; res = res * FEMTO
      case ('T') ; res = res * PICO
      case ('G') ; res = res * NANO
      case ('M') ; res = res * MICRO
      case ('k') ; res = res * MILLI
      case ('m') ; res = res * KILO
      case ('u') ; res = res * MEGA
      case ('n') ; res = res * GIGA
      case ('p') ; res = res * TERA
      case ('f') ; res = res * PETA
      end select
    endif
  end function convert_si
!
end module spur_mathconst_dble
