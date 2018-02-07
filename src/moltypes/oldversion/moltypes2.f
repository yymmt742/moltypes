!-----------------------atomdata.f------------------------!
!                        Ver.1.0.0                        !
!This is Common Linked List of Atom Data.                 !
!---------------------------------------------------------!
module moltypes2
  use amberprmtopio
  implicit none
  private
  public :: moltraj
!
  character(6),parameter     :: MOLFMTS(16) = ["xyz   ","pdb   ","prmtop","mdcrd ",&
                                              &"mol2  ","rst7  ","restrt","netcdf",&
                                              &"nc    ","log   ","dcd   ","chg   ",&
                                              &"charge","      ","      ","      "]
!
  integer,parameter          :: LenAtmName = 20                     ! For AtmName Length
  integer,parameter          :: LenResName = 20                     ! For ResName Length
  integer,parameter          :: LenAtmType = 6                      ! For AtmType Length
  integer,parameter          :: NFRAME_MAX = 100000                 ! For AtmType Length
!
  double precision,parameter :: StdChgR    = 18.2223d0                  ! For Amber Unit
  double precision,parameter :: StdChg     = 0.0548778d0                !    1.0/18.2223
  double precision,parameter :: StdVelR    = 20.455d0                   ! For Amber Unit
  double precision,parameter :: StdVel     = 0.0488878d0                !   1.d0/20.455d0
!
  type moltraj
    private
    type(amberprmtop),allocatable       :: ambtop(:)
  contains
    final                               :: MolTrajDestractor
  end type moltraj
!
contains
  pure subroutine MolTrajDestractor(this)
  type(MolTraj),intent(inout)  :: this
    if(allocated(this%ambtop)) deallocate(this%ambtop)
  end subroutine MolTrajDestractor
end module moltypes2
