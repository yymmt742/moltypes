module spur_mathconst_dble
implicit none
public
  double precision,parameter :: PI        = 3.1415926535897932384626433d0
  double precision,parameter :: ANG2RAD   = 0.0174532925199432957690768d0  ! PI/180
  double precision,parameter :: HTR2KCM   = 627.509d0
  double precision,parameter :: EC2KCM    = 332.0522d0                     ! 
  double precision,parameter :: HTR2EV    = 27.21162d0
  double precision,parameter :: HTR2NM    = 45.56288d0
  double precision,parameter :: KCM2NM    = 28590.7104d0
  double precision,parameter :: EV2KCM    = 23.06d0
  double precision,parameter :: EV2NM     = 1239.84d0
  double precision,parameter :: BOHR      = 0.5291772106712d0              ! [Angstr]
  double precision,parameter :: AU2DEBYE  = 4.803204544d0
  double precision,parameter :: ANG2BOHR  = 1.d0/BOHR
  double precision,parameter :: PLANK     = 6.62607004081d0                ! [10e-34 Js]
  double precision,parameter :: DIRAC     = 1.05457180013d0                ! [10e-34 Js]
  double precision,parameter :: ZERO      = 0.d0
  double precision,parameter :: HALF      = 0.5d0
  double precision,parameter :: ONE       = 1.d0
  double precision,parameter :: SQRT2     = 1.4142135623730950d0
  double precision,parameter :: SQRT3     = 1.7320508075688773d0
  double precision,parameter :: SQRT5     = 2.2360679774997897d0
  double precision,parameter :: SQRT7     = 2.6457513110645906d0
  double precision,parameter :: GOLD      = 1.6180339887498948d0
  double precision,parameter :: SILVER    = 2.4142135623730950d0
  double precision,parameter :: EULER     = 2.71828182845904524d0
end module spur_mathconst_dble
