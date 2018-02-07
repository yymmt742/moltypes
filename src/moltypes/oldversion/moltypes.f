!-----------------------atomdata.f------------------------!
!                        Ver.1.0.0                        !
!This is Common Linked List of Atom Data.                 !
!---------------------------------------------------------!
module moltypes
  use spur_vector
  use spur_io
  use spur_optparse
  use periodic_tables
  use moltypes_amberprmtop
  implicit none
  private
  public :: trajectory
  public :: UsePeriodicTable
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
  type(periodic_table)       :: pt
!
  type molinfo
    private
    !$TITLE SECTION
    character(:),allocatable  :: UnitName                                ! Atom List Name
    character(:),allocatable  :: Title                                  ! Atom List Title
    character(:),allocatable  :: Caption                              ! Atom List Caption
    integer                   :: NATM=-1,NRES=-1           !Number of Atom/NATM*3/Residue
    !$DATA SECTION
    logical,allocatable       :: mask(:)
    type(vector_character)    :: AtmName, ResName, AtmType   !atom name/res name/atomtype
    type(vector_character)    :: UniqAtm, UniqRes, UniqTyp     !unique atom/res name/type
    type(vector_double)       :: chg,     mass,    vdw      !point charges/mass/vdw radii
    type(vector_integer)      :: Idx,     Residue          !atom/residue/unique res index
    type(vector_integer)      :: bond,    angle,   dihed       !bonding/angle/dihed index
    type(vector_integer)      :: Element                                   !atomic number
    type(ErrorHandle)         :: ERR
  contains
    final     :: molinfoDestractor
  end type molinfo
!
  type trajectory
    private
    type(molinfo)                       :: top
    type(AmberPrmtop)                   :: ambtop
    integer                             :: NATM = -1, NTRJ = -1   !natms/nsteps
    integer                             :: Stack = -1
    double precision,allocatable,public :: xyz(:,:,:)
    double precision,allocatable,public :: box(:,:)
    logical,allocatable                 :: selection(:)
  contains
    procedure,private   :: TrjLoadSeq
    procedure,private   :: TrjLoadOne
    generic             :: load               => TrjLoadSeq,TrjLoadOne
    procedure           :: TrjExportSeq
    procedure           :: TrjExportOne
    generic             :: export             => TrjExportSeq,TrjExportOne
    procedure           :: n_atoms            => TrjNatoms
    procedure           :: n_residues         => TrjNresidues
    procedure           :: n_frames           => TrjNframe
    procedure           :: atomnames          => TrjAtomnames
    procedure           :: residuenames       => TrjResiduenames
    procedure           :: coordinates        => TrjCoordinates
    procedure           :: boxes              => TrjBoxes
    procedure,private   :: TrjReciprocalBox
    procedure,private   :: TrjReciprocalBoxAll
    generic             :: reciprocal_boxes   => TrjReciprocalBox,TrjReciprocalBoxAll
    procedure           :: charges            => TrjCharges
    procedure           :: residue_index      => TrjResidueIndex
    procedure           :: uniq_residue_index => TrjUniqResidueIndex
    procedure           :: atomselect         => TrjAtomSelect
    procedure           :: center_coordinates => TrjCenterCoordinates
    procedure           :: center_residue     => TrjCenterResidue
    procedure           :: pack               => TrjPack
    procedure           :: unwrap             => TrjUnwrap
    procedure           :: reserve            => TrjReserve
    procedure,private   :: tfass              => TrjAssignFile
    procedure,private   :: tfsass             => TrjAssignFiles
    procedure,private   :: ttass              => TrjAssignTrj
    procedure,private   :: tdtass             => TrjAssignDTrj
    procedure,private   :: trtass             => TrjAssignRTrj
    procedure,private   :: tdcass             => TrjAssignDCrd
    procedure,private   :: trcass             => TrjAssignRCrd
    generic             :: assignment(=)      => tfass,tfsass,ttass,tdtass,trtass,tdcass,trcass
    procedure,private   :: ttadd              => TrjAdd
    generic             :: operator(+)        => ttadd
    procedure,private   :: ttsub              => TrjSub
    generic             :: operator(-)        => ttsub
    procedure,private   :: ttmul              => TrjMul
    generic             :: operator(*)        => ttmul
    procedure,private   :: ttdiv              => TrjDiv
    generic             :: operator(/)        => ttdiv
    procedure,private   :: ttcrs              => TrjCross
    generic             :: operator(.dot.)    => ttcrs
    procedure           :: clear              => TrjClear
    final               :: TrjDestractor
  end type trajectory
!
contains
  include "readmask.f"
  include "trajectory_operators.f"
  include "moltypeio.f"
  include "moltypetools.f"
!
  pure subroutine TrjExpand(this)
  class(Trajectory),intent(inout)  :: this
  integer                          :: OldStack
  double precision,allocatable     :: TmpXYZ(:,:,:),TmpBox(:,:)
    if(this%Stack>=this%NTRJ.and.this%Stack>0)RETURN
    if(this%Stack<=0)this%Stack = 1
!
    OldStack = this%Stack
    do while(this%NTRJ>this%Stack)
      this%Stack = this%Stack * 2
    enddo
!
    allocate(TmpXYZ(3,this%NATM,this%Stack))
    if(allocated(this%XYZ))TmpXYZ(1:3,1:this%NATM,1:OldStack) = this%XYZ(1:3,1:this%NATM,1:OldStack)
    call move_alloc(from=TmpXYZ,to=this%XYZ)
    allocate(TmpBox(3,this%Stack))
    if(allocated(this%Box))TmpBox(1:3,1:OldStack) = this%Box(1:3,1:OldStack)
    call move_alloc(from=TmpBox,to=this%Box)
    RETURN
  end subroutine TrjExpand
!
  subroutine UsePeriodicTable(path)
  character(*),intent(in) :: path
    call pt%load(path)
  end subroutine UsePeriodicTable
!
  pure integer function TrjNatoms(this) result(res)
  class(Trajectory),intent(in)  :: this
    res = this%NATM ; RETURN
  end function TrjNatoms
!
  pure integer function TrjNresidues(this) result(res)
  class(Trajectory),intent(in)  :: this
  type(vector_integer)          :: ures
    if(this%NATM<=0)then
      res = 0 ; RETURN
    endif
    ures = pack(this%top%residue%lookup(),this%selection)
    ures = ures%uniq()
    res = ures%size() ; RETURN
  end function TrjNresidues
!
  pure integer function TrjNframe(this) result(res)
  class(Trajectory),intent(in)  :: this
    res = this%NTRJ ; RETURN
  end function TrjNframe
!
  pure function TrjCoordinates(this,ntrj) result(res)
  class(Trajectory),intent(in)  :: this
  integer,intent(in)            :: ntrj
  double precision              :: res(3,maxval([this%NATM,1],1))
  integer                       :: i,j
    if(this%NATM<=0.or.this%NTRJ<ntrj.or.ntrj<=0)then
      res = 0.d0 ; RETURN
    endif
    j = 0
    do i = 1,size(this%selection)
      if(this%selection(i))then
        j = j + 1 ; res(:,j) = this%xyz(:,i,ntrj)
      endif
    enddo
  end function TrjCoordinates
!
  pure function TrjBoxes(this,ntrj) result(res)
  class(Trajectory),intent(in)  :: this
  integer,intent(in)            :: ntrj
  double precision              :: res(3)
    if(this%NATM<=0.or.this%NTRJ<ntrj.or.ntrj<=0)then
      res = 0.d0 ; RETURN
    endif
    res(:) = this%box(:,ntrj)
  end function TrjBoxes
!
  pure function TrjReciprocalBox(this,ntrj) result(res)
  class(Trajectory),intent(in)  :: this
  integer,intent(in)            :: ntrj
  double precision              :: res(3)
    if(this%NATM<=0.or.this%NTRJ<ntrj.or.ntrj<=0)then
      res = 0.d0 ; RETURN
    endif
    if(any(this%box(:,ntrj)<10E-5))then
      res = 0.d0
    else
      res = 1.d0 / this%box(:,ntrj)
    endif
  end function TrjReciprocalBox
!
  pure function TrjReciprocalBoxAll(this) result(res)
  class(Trajectory),intent(in)  :: this
  double precision              :: res(3,this%NTRJ)
  integer                       :: i
    if(this%NATM<=0.or.this%NTRJ<=0)then
      res = 0.d0 ; RETURN
    endif
    do i=1,this%NTRJ
      if(any(this%box(:,i)<10E-5))then
        res(:,i) = 0.d0
      else
        res(:,i) = 1.d0 / this%box(:,i)
      endif
    enddo
  end function TrjReciprocalBoxAll
!
  pure function TrjAtomNames(this) result(res)
  class(Trajectory),intent(in)            :: this
  character(this%top%atmname%maxlength()) :: res(maxval([this%NATM,1],1))
    if(this%top%atmname%empty().or.this%NATM<=0)then
      res = ''
    else
      res = pack(this%top%atmname%lookup(),this%selection)
    endif
  end function TrjAtomNames
!
  pure function TrjResidueNames(this) result(res)
  class(Trajectory),intent(in)            :: this
  character(this%top%atmname%maxlength()) :: res(maxval([this%NATM,1],1))
    if(this%top%resname%empty().or.this%NATM<=0)then
      res = ''
    else
      res = pack(this%top%resname%lookup(),this%selection)
    endif
  end function TrjResidueNames
!
  pure function TrjResidueIndex(this) result(res)
  class(Trajectory),intent(in)            :: this
  integer,allocatable                     :: res(:)
  integer                                 :: i,j
    if(this%top%residue%empty().or.this%NATM<=0)then
      allocate(res(1)) ; res = 0
    else
      allocate(res(this%NATM))
      res = pack(this%top%residue%lookup(),this%selection)
    endif
  end function TrjResidueIndex
!
  pure function TrjUniqResidueIndex(this) result(res)
  class(Trajectory),intent(in)            :: this
  type(vector_integer)                    :: tmp,ures
  integer,allocatable                     :: res(:)
    tmp  = pack(this%top%residue%lookup(),this%selection)
    ures = tmp%uniq()
    allocate(res(ures%size()))
    res = ures%lookup()
  end function TrjUniqResidueIndex
!
  pure function TrjCharges(this,resid) result(res)
  class(Trajectory),intent(in)  :: this
  integer,intent(in),optional   :: resid
  double precision,allocatable  :: res(:)
    if(this%top%chg%empty().or.this%NATM<=0)then
      allocate(res(1)) ; res = 0.d0
    else
      if(present(resid))then
        allocate(res(count(this%selection.and.this%top%residue%lookup()==resid)))
        res = pack(this%top%chg%lookup(),this%selection.and.this%top%residue%lookup()==resid)
      else
        allocate(res(count(this%selection)))
        res = pack(this%top%chg%lookup(),this%selection)
      endif
    endif
  end function TrjCharges
!
  pure function Trjvdwbox(x,y,z,Natm) result(res)
  double precision,intent(in) :: x(Natm),y(Natm),z(Natm)
  integer,intent(in)          :: Natm
  double precision            :: res(1:3)
    res(1) = maxval(x,1) - minval(x,1) + 2.d0
    res(2) = maxval(y,1) - minval(y,1) + 2.d0
    res(3) = maxval(z,1) - minval(z,1) + 2.d0
    RETURN
  end function Trjvdwbox
!
  pure subroutine TrjAtomSelect(this,Mask,Add)
  class(trajectory),intent(inout)  :: this
  character(*),intent(in)          :: MASK
  logical,intent(in),optional      :: Add
    if(.not.allocated(this%selection).or..not.allocated(this%top%mask))RETURN
    if(present(Add))then
      if(Add)then
        if(trim(MASK)=='')RETURN
        this%selection = IOR(this%selection,ReadMask(this%top,MASK))
        this%NATM = count(this%selection) ; RETURN
      endif
    endif
    this%selection = ReadMask(this%top,MASK)
    this%NATM = count(this%selection) ; RETURN
  end subroutine TrjAtomSelect
!
  pure subroutine TrjPack(this,Mask)
  class(trajectory),intent(inout)  :: this
  character(*),intent(in),optional :: MASK
  logical,allocatable              :: selection(:)
  double precision,allocatable     :: xyz(:,:,:),Box(:,:)
  type(vector_integer)             :: UniqResidue
  integer                          :: NATM,i,j,k
    if(this%NATM<=0)RETURN
    if(present(MASK))this%selection = ReadMask(this%top,MASK)
    NATM = size(this%selection)
    this%NATM = count(this%selection)
    call move_alloc(this%xyz,xyz)
    call move_alloc(this%box,box)
    call move_alloc(this%selection,selection)
    j = 0
    do i = 1,size(this%top%mask)
      if(this%top%mask(i))then
        j = j + 1 ; this%top%mask(i) = selection(j)
      endif
    enddo
!
    this%top%idx = pack([(i,i=1,size(this%top%mask))],this%top%mask)
    if(.not.this%top%AtmName%empty())then
      this%top%AtmName = pack(this%top%AtmName%lookup(),selection)
      this%top%UniqATM = this%top%AtmName%Uniq()
    endif
    if(.not.this%top%ResName%empty())then
      this%top%ResName = pack(this%top%ResName%lookup(),selection)
      this%top%UniqRes = this%top%ResName%Uniq()
    endif
    if(.not.this%top%AtmType%empty())then
      this%top%AtmType = pack(this%top%AtmType%lookup(),selection)
      this%top%UniqTyp = this%top%AtmType%Uniq()
    endif
!
    if(.not.this%top%chg%empty())     this%top%chg     = pack(this%top%chg%lookup(),selection)
    if(.not.this%top%mass%empty())    this%top%mass    = pack(this%top%mass%lookup(),selection)
    if(.not.this%top%vdw%empty())     this%top%vdw     = pack(this%top%vdw%lookup(),selection)
    if(.not.this%top%element%empty()) this%top%element = pack(this%top%element%lookup(),selection)
!
    if(.not.this%top%Residue%empty())then
      this%top%Residue = pack(this%top%Residue%lookup(),selection)
      UniqResidue  = this%top%Residue%Uniq()
    endif
    this%top%NRES = maxval([UniqResidue%size(),1],1)
!
    allocate(this%xyz(3,this%NATM,this%NTRJ))
    allocate(this%box(3,this%NTRJ))
!
    do k = 1,this%NTRJ
      j = 0
      do i = 1,NATM
        if(selection(i))then
          j = j + 1 ; this%xyz(:,j,k) = xyz(:,i,k)
        endif
      enddo
    enddo
    this%Box(:,1:this%NTRJ) = Box(:,1:this%NTRJ)
    allocate(this%selection(this%NATM))
    this%selection = .TRUE.
    RETURN
  end subroutine TrjPack
!
  pure subroutine MolinfoDestractor(this)
  type(molinfo),intent(inout)  :: this
    if(allocated(this%UnitName))deallocate(this%UnitName)
    if(allocated(this%Title))   deallocate(this%Title)
    if(allocated(this%Caption)) deallocate(this%Caption)
    if(allocated(this%mask))    deallocate(this%mask)
    this%NATM = -1  ; this%NRES = -1
    call this%chg%clear()     ; call this%mass%clear()    ; call this%vdw%clear()
    call this%AtmName%clear() ; call this%ResName%clear() ; call this%AtmType%clear()
    call this%UniqAtm%clear() ; call this%UniqRes%clear() ; call this%UniqTyp%clear()
    call this%Idx%clear()     ; call this%residue%clear() ; call this%element%clear()
    call this%bond%clear()    ; call this%angle%clear()   ; call this%dihed%clear()
    RETURN
  end subroutine MolinfoDestractor
!
  pure subroutine TrjClear(this)
  class(trajectory),intent(inout)  :: this
    call MolinfoDestractor(this%top)
    if(allocated(this%xyz))deallocate(this%xyz)
    if(allocated(this%box))deallocate(this%box)
    if(allocated(this%selection))deallocate(this%selection)
    this%NATM = -1 ; this%NTRJ = -1 ; this%Stack = -1
    RETURN
  end subroutine TrjClear
!
  pure subroutine TrjDestractor(this)
  type(trajectory),intent(inout)  :: this
    call MolinfoDestractor(this%top)
    if(allocated(this%xyz))      deallocate(this%xyz)
    if(allocated(this%box))      deallocate(this%box)
    if(allocated(this%selection))deallocate(this%selection)
    this%NATM = -1 ; this%NTRJ = -1 ; this%Stack = -1
    RETURN
  end subroutine TrjDestractor
end module moltypes
