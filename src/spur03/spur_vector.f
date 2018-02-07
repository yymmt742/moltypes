!--------------------spur_vector.f------------------------!
!                       Ver.0.0.1                         !
!              std::vector like array module              !
!---------------------------------------------------------!
module spur_vector
  implicit none
  private
  public :: vector,vector_integer,vector_real,vector_character,&
           &vector_logical,vector_double,hashmap
  public :: assignment(=)
  public :: operator(+),operator(-),operator(*),operator(/),operator(.dot.)
  public :: operator(.eq.),operator(.ne.)
!
  character(4),parameter :: INT_FMT="(I0)"
  character(6),parameter :: REAL_FMT="(F8.3)"
  character(7),parameter :: DBLE_FMT="(F16.9)"
  character(3),parameter :: LOGC_FMT="(L)"
!
!  integer(4),parameter   :: FNV_OFFSET_32 = 2166136261
  integer(4),parameter   :: FNV_OFFSET_32 = 166136261
  integer(4),parameter   :: FNV_PRIME_32  = 16777619
!
  integer,parameter          :: IEmptyDummy = 0
  real,parameter             :: REmptyDummy = 0.0
  double precision,parameter :: DEmptyDummy = 0.d0
  logical,parameter          :: LEmptyDummy = .FALSE.
  character(0),parameter     :: CEmptyDummy = ''
!
  type vector
    private
    integer             :: length= 0, Stack = -1
  contains
    procedure           :: size        => VecSize
    procedure           :: capacity    => VecCapacity
    procedure           :: empty       => VecEmpty
    procedure           :: clear       => VecClear
    procedure           :: reserve     => VecReserve
    procedure           :: erace       => VecErace
    procedure           :: ShrinkToFit => VecShrinkToFit
  end type vector
!
  type,extends(vector) :: vector_integer
    private
    integer,allocatable,public :: at(:)
  contains
    procedure,private   :: Spush       => IPush
    procedure,private   :: VPush       => IVPush
    generic             :: push        => Spush,Vpush
    procedure           :: pop         => IPop
    procedure           :: insert      => IInsert
    procedure           :: lookup      => ILookUp
    procedure           :: uniq        => IUniq
    procedure           :: uniqself    => IUniqSelf
    final               :: IDestractor
  end type vector_integer
!
  type,extends(vector) :: vector_real
    private
    real,allocatable,public :: at(:)
  contains
    procedure,private   :: Spush       => RPush
    procedure,private   :: VPush       => RVPush
    generic             :: push        => Spush,Vpush
    procedure           :: pop         => RPop
    procedure           :: insert      => RInsert
    procedure           :: lookup      => RLookUp
    procedure           :: sort        => RSort
    final               :: RDestractor
  end type vector_real
!
  type,extends(vector) :: vector_double
    private
    double precision,allocatable,public :: at(:)
  contains
    procedure,private   :: DPush
    procedure,private   :: DVPush
    generic             :: push        => DPush,DVPush
    procedure           :: pop         => DPop
    procedure           :: insert      => DInsert
    procedure           :: lookup      => DLookUp
    final               :: DDestractor
  end type vector_double
!
  type,extends(vector) :: vector_logical
    private
    logical,allocatable,public :: at(:)
  contains
    procedure,private   :: Spush       => LPush
    procedure,private   :: VPush       => LVPush
    generic             :: push        => Spush,Vpush
    procedure           :: pop         => LPop
    procedure           :: insert      => LInsert
    procedure           :: lookup      => LLookUp
    final               :: LDestractor
  end type vector_logical
!
  type ChrNode
    private
    character(:),allocatable :: chr
  contains
    final     :: ChrNodeDestractor
  end type ChrNode
!
  type,extends(vector) :: vector_character
    private
    type(ChrNode),allocatable   :: var(:)
    character(0)                :: EmptyDummy = ""
    character(:),allocatable    :: delimiter
    integer                     :: MaxLen = 0, TotalLen=0
  contains
    procedure,private   :: Spush       => CPush
    procedure,private   :: VPush       => CVPush
    generic             :: push        => Spush,Vpush
    procedure           :: pop         => CPop
    procedure           :: insert      => CInsert
    procedure           :: at          => CAt
    procedure           :: toint       => CToInt
    procedure           :: toreal      => CToReal
    procedure           :: scope       => CScope
    procedure           :: lookup      => CLookUp
    procedure           :: large       => CLarge
    procedure           :: small       => CSmall
    procedure           :: find        => CFind
    procedure           :: join        => CJoin
    procedure           :: split       => CSplit
    procedure           :: textwrap    => CTextWrap
    procedure           :: maxlength   => CMaxlength
    procedure           :: uniq        => CUniq
    final               :: CDestractor
  end type vector_character
!
  type,extends(vector) :: hashmap
    private
    type(chrnode),allocatable   :: var(:),key(:)
    character(:),allocatable    :: delimiter
    character(0)                :: EmptyDummy = ""
    integer                     :: MaxLen = 0, TotalLen=0
  contains
    procedure           :: push        => HMPush
    procedure           :: at          => HMAt
    final               :: AADestractor
  end type hashmap
!
  interface assignment(=)
    module procedure IAssign, IAssign_Array,&
                   & RAssign, RAssign_Array,&
                   & DAssign, DAssign_Array,&
                   & LAssign, LAssign_Array,&
                   & ChrAssign, ChrAssign_Array,&
                   & VecAssign
  end interface assignment(=)
!
  interface operator(+)
    module procedure IIAdd, RRAdd, DDAdd
  end interface
  interface operator(-)
    module procedure IIsub, RRSub, DDSub
  end interface
  interface operator(*)
    module procedure IImul, RRmul, DDmul
  end interface
  interface operator(/)
    module procedure IIdiv, RRdiv, DDdiv
  end interface
  interface operator(.dot.)
    module procedure IIdot, RRdot, DDdot
  end interface
  interface operator(==)
    module procedure CVCeq, CCVeq, CVCVeq, CVCAeq, CACVeq
  end interface
  interface operator(/=)
    module procedure CVCne, CCVne, CVCVne, CVCAne, CACVne
  end interface
contains
!---------------------------------------------------------!
!       Routines for Stack controll.                      !
!---------------------------------------------------------!
  pure subroutine StackExtention(this)
  class(Vector),intent(inout)  :: this
  integer                      :: OldStack
  integer,         allocatable :: TmpInt(:)
  real,            allocatable :: TmpReal(:)
  double precision,allocatable :: TmpDouble(:)
  logical,         allocatable :: TmpLogical(:)
  type(ChrNode),   allocatable :: TmpChr(:)
!
    if(this%Stack>=this%length.and.this%Stack>0)RETURN
    if(this%Stack<=0)this%Stack = 1
    OldStack = this%Stack
    do while(this%Stack<this%length)
      this%Stack = this%Stack * 2
    enddo
!
    select type(this)
    type is(vector_integer)
      allocate(TmpInt(this%Stack))
      if(allocated(this%at))TmpInt(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpInt,to=this%at)
    type is(vector_real)
      allocate(TmpReal(this%Stack))
      if(allocated(this%at))TmpReal(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpReal,to=this%at)
    type is(vector_double)
      allocate(TmpDouble(this%Stack))
      if(allocated(this%at))TmpDouble(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpDouble,to=this%at)
    type is(vector_logical)
      allocate(TmpLogical(this%Stack))
      if(allocated(this%at))TmpLogical(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpLogical,to=this%at)
    type is(vector_character)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%var)
    type is(hashmap)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%var)
      allocate(TmpChr(this%Stack))
      if(allocated(this%key))call ChrNodeMove(this%key(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%key)
      call HMrehash(this,OldStack)
    class default
      this%length = 0
    end select
    RETURN
  end subroutine StackExtention
!
  pure subroutine VecReserve(this,siz)
  class(Vector),intent(inout)  :: this
  integer,intent(in)           :: siz
  integer                      :: OldStack
  integer,allocatable          :: TmpInt(:)
  real,allocatable             :: TmpReal(:)
  double precision,allocatable :: TmpDouble(:)
  logical,allocatable          :: TmpLogical(:)
  type(ChrNode),allocatable    :: TmpChr(:)
    if(siz<=this%Stack)RETURN
    if(this%Stack<=0)this%Stack = 1
!
    OldStack = this%Stack
    if(siz+1<=this%Stack*2)then
      this%Stack = this%Stack * 2
    else
      this%Stack = siz
    endif
!
    select type(this)
    type is(vector_integer)
      allocate(TmpInt(this%Stack))
      if(allocated(this%at))TmpInt(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpInt,to=this%at)
    type is(vector_real)
      allocate(TmpReal(this%Stack))
      if(allocated(this%at))TmpReal(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpReal,to=this%at)
    type is(vector_double)
      allocate(TmpDouble(this%Stack))
      if(allocated(this%at))TmpDouble(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpDouble,to=this%at)
    type is(vector_logical)
      allocate(TmpLogical(this%Stack))
      if(allocated(this%at))TmpLogical(1:OldStack) = this%at(1:OldStack)
      call move_alloc(from=TmpLogical,to=this%at)
    type is(vector_character)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%var)
    type is(hashmap)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%var)
      allocate(TmpChr(this%Stack))
      if(allocated(this%key))call ChrNodeMove(this%key(1:OldStack),TmpChr(1:OldStack))
      call move_alloc(from=TmpChr,to=this%key)
      call HMrehash(this,OldStack)
    end select
    RETURN
  end subroutine VecReserve
!
  elemental subroutine VecErace(this,idx)
  class(Vector),intent(inout)         :: this
  integer,intent(in),optional         :: idx
  integer                             :: i,j,at
    if(.not.present(idx))this%length = 0
    if(this%length<=0) RETURN
!
    at = PeriodicBound(this%length,idx)
!
    select type(this)
    type is(vector_integer)
      this%at(at:this%length-1) = this%at(at+1:this%length)
    type is(vector_real)
      this%at(at:this%length-1) = this%at(at+1:this%length)
    type is(vector_double)
      this%at(at:this%length-1) = this%at(at+1:this%length)
    type is(vector_logical)
      this%at(at:this%length-1) = this%at(at+1:this%length)
    type is(vector_character)
      j = at
      do i=at+1,this%length
        j = j + 1
        call ChrNodeMove(this%var(i),this%var(j))
      enddo
    type is(hashmap)
      RETURN
    end select
    this%length = this%length - 1
    RETURN
  end subroutine VecErace
!
  pure subroutine VecShrinkToFit(this)
  class(Vector),intent(inout)  :: this
  integer,allocatable          :: TmpInt(:)
  real,allocatable             :: TmpReal(:)
  double precision,allocatable :: TmpDouble(:)
  logical,allocatable          :: TmpLogical(:)
  type(ChrNode),allocatable    :: TmpChr(:)
    this%stack = this%length
    if(this%stack<=0)RETURN
    select type(this)
    type is(vector_integer)
      allocate(TmpInt(this%Stack))
      if(allocated(this%at))TmpInt(1:this%Stack) = this%at(1:this%Stack)
      call move_alloc(from=TmpInt,to=this%at)
    type is(vector_real)
      allocate(TmpReal(this%Stack))
      if(allocated(this%at))TmpReal(1:this%Stack) = this%at(1:this%Stack)
      call move_alloc(from=TmpReal,to=this%at)
    type is(vector_double)
      allocate(TmpDouble(this%Stack))
      if(allocated(this%at))TmpDouble(1:this%Stack) = this%at(1:this%Stack)
      call move_alloc(from=TmpDouble,to=this%at)
    type is(vector_logical)
      allocate(TmpLogical(this%Stack))
      if(allocated(this%at))TmpLogical(1:this%Stack) = this%at(1:this%Stack)
      call move_alloc(from=TmpLogical,to=this%at)
    type is(vector_character)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:this%Stack),TmpChr(1:this%Stack))
      call move_alloc(from=TmpChr,to=this%var)
    type is(hashmap)
      allocate(TmpChr(this%Stack))
      if(allocated(this%var))call ChrNodeMove(this%var(1:this%Stack),TmpChr(1:this%Stack))
      call move_alloc(from=TmpChr,to=this%var)
      allocate(TmpChr(this%Stack))
      if(allocated(this%key))call ChrNodeMove(this%key(1:this%Stack),TmpChr(1:this%Stack))
      call move_alloc(from=TmpChr,to=this%key)
    class default
      this%length = 0
    end select
    RETURN
  end subroutine VecShrinkToFit
!
  pure integer function VecCapacity(this) result(res)
  class(Vector),intent(in) :: this
    res = this%Stack ; RETURN
  end function VecCapacity
!
  pure integer function VecSize(this) result(res)
  class(Vector),intent(in) :: this
    res = this%length ; RETURN
  end function VecSize
!
  pure subroutine VecClear(this)
  class(Vector),intent(inout) :: this
    select type(this)
    type is(vector_integer)
      call IDestractor(this)
    type is(vector_real)
      call RDestractor(this)
    type is(vector_double)
      call DDestractor(this)
    type is(vector_logical)
      call LDestractor(this)
    type is(vector_character)
      call CDestractor(this)
    type is(hashmap)
      call AADestractor(this)
    class default
      this%length = 0 ; this%Stack = -1
    end select
    RETURN
  end subroutine VecClear
!
  pure function VecEmpty(this) result(res)
  class(Vector),intent(in) :: this
  logical                  :: res
    res = this%length==0
    RETURN
  end function VecEmpty
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine IAssign(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  integer,intent(in)          :: RHS
  integer                     :: VAL
  character(20)               :: ChrVar
    VAL = RHS
    LHS%length = 1 ; call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_integer)
      LHS%at(1) = VAL
    type is(vector_real)
      LHS%at(1) = real(VAL)
    type is(vector_double)
      LHS%at(1) = dble(VAL)
    type is(vector_character)
      write(ChrVar,INT_FMT)RHS
      call ChrNodeAssign(LHS,1,ChrVar)
    class default
      LHS%length = 0
    end select
    RETURN
  end subroutine IAssign
!
  pure subroutine IAssign_Array(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  integer,intent(in)          :: RHS(:)
  integer,allocatable         :: VAL(:)
  integer                     :: i,j,ub
  character(20)               :: ChrVar
    LHS%length = size(RHS)
    if(LHS%length==0)RETURN
    allocate(VAL(LHS%length)) ; VAL = RHS
    call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_integer)
      LHS%at(1:LHS%length) = VAL
    type is(vector_real)
      LHS%at(1:LHS%length) = real(VAL)
    type is(vector_double)
      LHS%at(1:LHS%length) = dble(VAL)
    type is(vector_character)
      j = 0
      do i=lbound(VAL,1),ubound(VAL,1)
        j = j + 1
        write(ChrVar,INT_FMT)VAL(i)
        call ChrNodeAssign(LHS,j,ChrVar)
      enddo
    class default
      LHS%length = 0
    end select
    deallocate(VAL)
    RETURN
  end subroutine IAssign_Array
!
  pure subroutine RAssign(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  real,intent(in)             :: RHS
  real                        :: VAL
  character(50),allocatable   :: ChrVar
    VAL = RHS
    LHS%length = 1 ; call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_integer)
      LHS%at(1) = nint(VAL)
    type is(vector_real)
      LHS%at(1) = VAL
    type is(vector_double)
      LHS%at(1) = dble(VAL)
    type is(vector_character)
      write(ChrVar,REAL_FMT)VAL
      call ChrNodeAssign(LHS,1,ChrVar)
    class default
      LHS%length = 0
    end select
    RETURN
  end subroutine RAssign
!
  pure subroutine RAssign_Array(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  real,intent(in)             :: RHS(:)
  real,allocatable            :: VAL(:)
  integer                     :: i,j,ub
  character(50)               :: ChrVar
    LHS%length = size(RHS)
    if(LHS%length==0)RETURN
    allocate(VAL(LHS%length)) ; VAL = RHS
    call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_integer)
      LHS%at(1:LHS%length) = anint(VAL(:))
    type is(vector_real)
      LHS%at(1:LHS%length) = VAL(:)
    type is(vector_double)
      LHS%at(1:LHS%length) = dble(VAL(:))
    type is(vector_character)
      j = 0
      do i=lbound(VAL,1),ubound(VAL,1)
        j = j + 1
        write(ChrVar,REAL_FMT)VAL(i)
        call ChrNodeAssign(LHS,j,ChrVar)
      enddo
    class default
      LHS%length = 0
    end select
    deallocate(VAL)
    RETURN
  end subroutine RAssign_Array
!
  pure subroutine DAssign(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  double precision,intent(in) :: RHS
  double precision            :: VAL
  character(50)               :: ChrVar
    VAL = RHS
    LHS%length = 1 ; call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_integer)
      LHS%at(1) = idnint(VAL)
    type is(vector_real)
      LHS%at(1) = real(VAL)
    type is(vector_double)
      LHS%at(1) = VAL
    type is(vector_character)
      write(ChrVar,DBLE_FMT)VAL
      call ChrNodeAssign(LHS,1,ChrVar)
    class default
      LHS%length = 0
    end select
    RETURN
  end subroutine DAssign
!
  pure subroutine DAssign_Array(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  double precision,intent(in) :: RHS(:)
  double precision,allocatable:: VAL(:)
  integer                     :: i,j,ub
  character(50)               :: ChrVar
    LHS%length = size(RHS)
    if(LHS%length==0)RETURN
    allocate(VAL(LHS%length)) ; VAL = RHS
    call StackExtention(LHS)
    select type(LHS)
    type is(vector_integer)
      LHS%at(1:LHS%length) = anint(VAL(:))
    type is(vector_real)
      LHS%at(1:LHS%length) = real(VAL(:))
    type is(vector_double)
      LHS%at(1:LHS%length) = VAL(:)
    type is(vector_character)
      j = 0
      do i=lbound(VAL,1),ubound(VAL,1)
        j = j + 1
        write(ChrVar,DBLE_FMT)VAL(i)
        call ChrNodeAssign(LHS,j,Chrvar)
      enddo
    class default
      LHS%length = 0
    end select
    deallocate(VAL)
    RETURN
  end subroutine DAssign_Array
!
  pure subroutine LAssign(LHS,RHS)
    class(Vector),intent(inout) :: LHS
    logical,intent(in)          :: RHS
    logical                     :: VAL
    character(10)               :: ChrVar
    VAL = RHS
    LHS%length = 1
    call StackExtention(LHS)
!
    select type(LHS)
    type is(vector_logical)
      LHS%at(1) = VAL
    type is(vector_character)
      write(ChrVar,LOGC_FMT)VAL
      call ChrNodeAssign(LHS,1,ChrVar)
    class default
      LHS%length = 0
    end select
    RETURN
  end subroutine LAssign
!
  pure subroutine LAssign_Array(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  logical,intent(in)          :: RHS(:)
  logical,allocatable         :: VAL(:)
  integer                     :: i,j,ub
  character(10)               :: ChrVar
    LHS%length = size(RHS)
    if(LHS%length==0)RETURN
    allocate(VAL(LHS%length)) ; VAL = RHS
    call StackExtention(LHS)
    select type(LHS)
    type is(vector_logical)
      LHS%at(1:LHS%length) = VAL(:)
    type is(vector_character)
      j = 0
      do i=lbound(VAL,1),ubound(VAL,1)
        j = j + 1
        write(ChrVar,LOGC_FMT)VAL(i)
        call ChrNodeAssign(LHS,j,Chrvar)
      enddo
    class default
      LHS%length = 0
    end select
    deallocate(VAL)
    RETURN
  end subroutine LAssign_Array
!
  pure subroutine ChrAssign(LHS,RHS)
  class(Vector),intent(inout) :: LHS
  character(*),intent(in)     :: RHS
  integer                     :: is,intN
  real                        :: realN
  double precision            :: dbleN
    LHS%length = 0
    select type(LHS)
    type is(vector_integer)
      read(RHS,*,iostat=is)intN
      if(is==0)then
        LHS%length = 1
        call StackExtention(LHS)
        LHS%at(1) = intN
      endif
    type is(vector_real)
      read(RHS,*,iostat=is)realN
      if(is==0)then
        LHS%length = 1
        call StackExtention(LHS)
        LHS%at(1) = realN
      endif
    type is(vector_double)
      read(RHS,*,iostat=is)dbleN
      if(is==0)then
        LHS%length = 1
        call StackExtention(LHS)
        LHS%at(1) = dbleN
      endif
    type is(vector_character)
      LHS%length = 1
      call StackExtention(LHS)
      call ChrNodeAssign(LHS,1,RHS)
    end select
    RETURN
  end subroutine ChrAssign
!
  pure subroutine ChrAssign_Array(LHS,RHS)
  class(Vector),intent(inout)  :: LHS
  character(*),intent(in)      :: RHS(:)
  integer                      :: i,j
  integer                      :: is
  integer,allocatable          :: intN(:)
  real,allocatable             :: realN(:)
  double precision,allocatable :: dbleN(:)
    LHS%length = 0 ; j = 1
    select type(LHS)
    type is(vector_integer)
      allocate(intN(size(RHS)))
      do i = lbound(RHS,1),ubound(RHS,1)
        read(RHS,*,iostat=is)intN(j)
        if(is==0)j = j + 1
      enddo
      LHS%length = j - 1 ; if(LHS%length==0)RETURN
      LHS%stack = size(RHS)
      call move_alloc(from=intN,to=LHS%at)
    type is(vector_real)
      allocate(realN(size(RHS)))
      do i = lbound(RHS,1),ubound(RHS,1)
        read(RHS,*,iostat=is)realN(j)
        if(is==0)j = j + 1
      enddo
      LHS%length = j - 1 ; if(LHS%length==0)RETURN
      LHS%stack = size(RHS)
      call move_alloc(from=realN,to=LHS%at)
    type is(vector_double)
      allocate(dbleN(size(RHS)))
      do i = lbound(RHS,1),ubound(RHS,1)
        read(RHS,*,iostat=is)dbleN(j)
        if(is==0)j = j + 1
      enddo
      LHS%length = j - 1 ; if(LHS%length==0)RETURN
      LHS%stack = size(RHS)
      call move_alloc(from=dbleN,to=LHS%at)
    type is(vector_character)
      LHS%length = size(RHS)
      if(LHS%length==0)RETURN
      call StackExtention(LHS)
      do i = lbound(RHS,1),ubound(RHS,1)
        call ChrNodeAssign(LHS,j,RHS(i))
        j = j + 1
      enddo
    end select
    RETURN
  end subroutine ChrAssign_Array
!
  pure subroutine VecAssign(LHS,RHS)
    class(Vector),intent(inout) :: LHS
    class(Vector),intent(in)    :: RHS
    integer                     :: i
    LHS%length = RHS%length
    call VecReserve(LHS,LHS%length)
!
    select type(LHS)
    type is(vector_integer)
      select type(RHS)
      type is(vector_integer)
        LHS%at(1:LHS%length) = RHS%at(1:LHS%length)
      end select
    type is(vector_real)
      select type(RHS)
      type is(vector_real)
        LHS%at(1:LHS%length) = RHS%at(1:LHS%length)
      end select
    type is(vector_double)
      select type(RHS)
      type is(vector_double)
        LHS%at(1:LHS%length) = RHS%at(1:LHS%length)
      end select
    type is(vector_logical)
      select type(RHS)
      type is(vector_logical)
        LHS%at(1:LHS%length) = RHS%at(1:LHS%length)
      end select
    type is(vector_character)
      select type(RHS)
      type is(vector_character)
        do i=1,LHS%length
          call ChrNodeAssign(LHS,i,RHS%var(i)%chr)
        enddo
      end select
    class default
      LHS%length = 0
    end select
    RETURN
  end subroutine VecAssign
!
!---------------------------------------------------------!
!       Routines for push.                                !
!---------------------------------------------------------!
!
  pure subroutine IPush(this,var)
  class(vector_integer),intent(inout) :: this
  integer,intent(in)                  :: var
    this%length = this%length + 1 ; call StackExtention(this) ; this%at(this%length) = var ; RETURN
  end subroutine IPush
!
  pure subroutine RPush(this,var)
  class(vector_real),intent(inout) :: this
  real,intent(in)                  :: var
    this%length = this%length + 1 ; call StackExtention(this) ; this%at(this%length) = var ; RETURN
  end subroutine RPush
!
  pure subroutine DPush(this,var)
  class(vector_double),intent(inout) :: this
  double precision,intent(in)        :: var
    this%length = this%length + 1 ; call StackExtention(this) ; this%at(this%length) = var ; RETURN
  end subroutine DPush
!
  pure subroutine LPush(this,var)
  class(vector_logical),intent(inout) :: this
  logical,intent(in)                  :: var
    this%length = this%length + 1 ; call StackExtention(this) ; this%at(this%length) = var ; RETURN
  end subroutine LPush
!
  pure subroutine CPush(this,var)
  class(vector_character),intent(inout) :: this
  character(*),intent(in)               :: var
  integer                               :: at
    this%length = this%length + 1 ; call StackExtention(this) ; call ChrNodeAssign(this,this%length,var) ; RETURN
  end subroutine CPush
!
  pure subroutine HMPush(this,key,var)
  class(hashmap),intent(inout)          :: this
  character(*),intent(in)               :: key,var
  integer                               :: at,hash,Varlen
    this%length = this%length + 1 ; call StackExtention(this)
    hash = HMhash32(this,key,this%stack)
    VarLen = len_trim(key)
    if(.not.allocated(this%key(hash)%chr))allocate(character(VarLen)::this%key(hash)%chr)
    this%key(hash)%chr(:) = trim(key)
    VarLen = len_trim(var)
    if(.not.allocated(this%var(hash)%chr))allocate(character(VarLen)::this%var(hash)%chr)
    this%var(hash)%chr(:) = trim(var)
    this%TotalLen = this%TotalLen + VarLen ; if(VarLen>this%MaxLen)this%MaxLen = VarLen
    RETURN
  end subroutine HMPush
!
  pure subroutine IVPush(this,var)
  class(vector_integer),intent(inout) :: this
  integer,intent(in)                  :: var(:)
  integer                             :: lb, ub
    if(size(var)==0) RETURN ; lb = this%length + 1 ; ub = this%length + size(var)
    this%length = ub ; call StackExtention(this) ; this%at(lb:ub) = var(:) ; RETURN
  end subroutine IVPush
!
  pure subroutine RVPush(this,var)
  class(vector_real),intent(inout) :: this
  real,intent(in)                  :: var(:)
  integer                          :: lb, ub
    if(size(var)==0) RETURN ; lb = this%length + 1 ; ub = this%length + size(var)
    this%length = ub ; call StackExtention(this) ; this%at(lb:ub) = var(:) ; RETURN
  end subroutine RVPush
!
  pure subroutine DVPush(this,var)
  class(vector_double),intent(inout) :: this
  double precision,intent(in)        :: var(:)
  integer                            :: lb, ub
    if(size(var)==0) RETURN ; lb = this%length + 1 ; ub = this%length + size(var)
    this%length = ub ; call StackExtention(this) ; this%at(lb:ub) = var(:) ; RETURN
  end subroutine DVPush
!
  pure subroutine LVPush(this,var)
  class(vector_logical),intent(inout) :: this
  logical,intent(in)                  :: var(:)
  integer                             :: lb, ub
    if(size(var)==0) RETURN ; lb = this%length + 1 ; ub = this%length + size(var)
    this%length = ub ; call StackExtention(this) ; this%at(lb:ub) = var(:) ; RETURN
  end subroutine LVPush
!
  pure subroutine CVPush(this,var)
  class(vector_character),intent(inout) :: this
  character(*),intent(in)               :: var(:)
  integer                               :: i, j, lb, ub
    lb = this%length + 1 ; ub = this%length + size(var)
    this%length = ub ; call StackExtention(this)
    j = lbound(var,1) - 1
    do  i=lb,ub
      j = j + 1 ; call ChrNodeAssign(this,i,var(j))
    enddo
    RETURN
  end subroutine CVPush
!
!---------------------------------------------------------!
!       Routines for Concat.                              !
!---------------------------------------------------------!
!
  pure subroutine IConcat(this,var)
  class(Vector_Integer),intent(inout) :: this
  class(Vector_Integer),intent(in)    :: var
  integer                             :: lb, ub
    if(var%length==0)RETURN
    lb = this%length + 1 ; ub = this%length + var%length ; this%length = ub
    call StackExtention(this) ; this%at(lb:ub) = var%at(1:var%length) ; RETURN
  end subroutine IConcat
!
  pure subroutine RConcat(this,var)
  class(vector_real),intent(inout) :: this
  class(Vector_real),intent(in)    :: var
  integer                          :: lb, ub
    if(var%length==0)RETURN
    lb = this%length + 1 ; ub = this%length + var%length ; this%length = ub
    call StackExtention(this) ; this%at(lb:ub) = var%at(1:var%length) ; RETURN
  end subroutine RConcat
!
  pure subroutine DConcat(this,var)
  class(vector_double),intent(inout) :: this
  class(Vector_double),intent(in)    :: var
  integer                            :: lb, ub
    if(var%length==0)RETURN
    lb = this%length + 1 ; ub = this%length + var%length ; this%length = ub
    call StackExtention(this) ; this%at(lb:ub) = var%at(1:var%length) ; RETURN
  end subroutine DConcat
!
  pure subroutine LConcat(this,var)
  class(vector_logical),intent(inout) :: this
  class(Vector_logical),intent(in)    :: var
  integer                             :: lb, ub
    if(var%length==0)RETURN
    lb = this%length + 1 ; ub = this%length + var%length ; this%length = ub
    call StackExtention(this) ; this%at(lb:ub) = var%at(1:var%length) ; RETURN
  end subroutine LConcat
!
  pure subroutine CConcat(this,var)
  class(vector_character),intent(inout) :: this
  type(vector_character),intent(in)     :: var
  integer                               :: lb,ub
    if(var%length==0)RETURN
    lb = this%length + 1 ; ub = this%length + var%length ; this%length = ub
    call StackExtention(this)
    call ChrNodeCopy(var%var(1:var%length),this%var(lb:ub))
    RETURN
  end subroutine CConcat
!
!---------------------------------------------------------!
!       Routines for pop.                                 !
!---------------------------------------------------------!
!
  function IPop(this) result (res)
  class(vector_integer),intent(inout) :: this
  integer                             :: res
    if(this%length==0)then
      res = IEmptyDummy
    else
      res = this%at(this%length)
      this%length = this%length - 1
    endif
  end function IPop
!
  function RPop(this) result (res)
  class(vector_real),intent(inout) :: this
  real                             :: res
    if(this%length==0)then
      res = REmptyDummy
    else
      res = this%at(this%length)
      this%length = this%length - 1
    endif
  end function RPop
!
  function DPop(this) result (res)
  class(vector_double),intent(inout) :: this
  double precision                   :: res
    if(this%length==0)then
      res = DEmptyDummy
    else
      res = this%at(this%length)
      this%length = this%length - 1
    endif
  end function DPop
!
  logical function LPop(this) result (res)
  class(vector_logical),intent(inout) :: this
    if(this%length==0)then
      res = LEmptyDummy
    else
      res = this%at(this%length)
      this%length = this%length - 1
    endif
  end function LPop
!
  pure subroutine CPop(this,res)
  class(vector_character),intent(inout) :: this
  character(*),intent(out),optional     :: res
    if(this%length==0)then
      if(present(res)) res = CEmptyDummy
    else
      this%length = this%length - 1
      this%TotalLen = this%TotalLen - len(this%var(this%length)%chr)
      if(present(res)) res = this%var(this%length)%chr
    endif
    RETURN
  end subroutine CPop
!
!---------------------------------------------------------!
!       Routines for insert.                              !
!---------------------------------------------------------!
!
  pure subroutine IInsert(this,idx,var)
  class(vector_integer),intent(inout) :: this
  integer,intent(in)                  :: idx,var
  integer                             :: at
    if(idx>this%length)then
      this%length = idx
      call StackExtention(this)
      this%at(idx) = var
    else
      at = periodicbound(this%length,idx)
      this%length = this%length + 1
      call StackExtention(this)
      this%at(at+1:this%length) = this%at(at:this%length-1)
      this%at(at) = var
    endif
    RETURN
  end subroutine IInsert
!
  pure subroutine RInsert(this,idx,var)
  class(vector_real),intent(inout) :: this
  integer,intent(in)               :: idx
  real,intent(in)                  :: var
  integer                          :: at
    if(idx>this%length)then
      this%length = idx
      call StackExtention(this)
      this%at(idx) = var
    else
      at = periodicbound(this%length,idx)
      this%length = this%length + 1
      call StackExtention(this)
      this%at(at+1:this%length) = this%at(at:this%length-1)
      this%at(at) = var
    endif
    RETURN
  end subroutine RInsert
!
  pure subroutine DInsert(this,idx,var)
  class(vector_double),intent(inout) :: this
  integer,intent(in)                 :: idx
  double precision,intent(in)        :: var
  integer                            :: at
    if(idx>this%length)then
      this%length = idx
      call StackExtention(this)
      this%at(idx) = var
    else
      at = periodicbound(this%length,idx)
      this%length = this%length + 1
      call StackExtention(this)
      this%at(at+1:this%length) = this%at(at:this%length-1)
      this%at(at) = var
    endif
    RETURN
  end subroutine DInsert
!
  pure subroutine LInsert(this,idx,var)
  class(vector_logical),intent(inout) :: this
  integer,intent(in)                  :: idx
  logical,intent(in)                  :: var
  integer                             :: at
    if(idx>this%length)then
      this%length = idx
      call StackExtention(this)
      this%at(idx) = var
    else
      at = periodicbound(this%length,idx)
      this%length = this%length + 1
      call StackExtention(this)
      this%at(at+1:this%length) = this%at(at:this%length-1)
      this%at(at) = var
    endif
    RETURN
  end subroutine LInsert
!
  pure subroutine CInsert(this,idx,var)
  class(vector_character),intent(inout) :: this
  integer,intent(in)                    :: idx
  character(*),intent(in)               :: var
  integer                               :: i,j,at
    if(idx>this%length)then
      this%length = idx
      call StackExtention(this)
      call ChrNodeAssign(this,idx,var)
    else
      at = periodicbound(this%length,idx)
      this%length = this%length + 1
      call StackExtention(this)
      j = this%length
      do i=this%length-1,at,-1
        call ChrNodeMove(this%var(i),this%var(j))
        j = j - 1
      enddo
      call ChrNodeAssign(this,at,var)
    endif
    RETURN
  end subroutine CInsert
!
!---------------------------------------------------------!
!       Routines for at.                                  !
!---------------------------------------------------------!
!
  pure function CAt(this,idx,lb,ub) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in)                 :: idx
  integer,intent(in),optional        :: lb,ub
  character(:),allocatable           :: res
  integer                            :: i,j,at
  integer                            :: llb,lub,linc,lenchr,lenres
    if(this%length==0)then
      allocate(character(0)::res) ; RETURN 
    endif
    at = periodicbound(this%length,idx)
    lenchr = len(this%var(at)%chr)
    if(lenchr==0)then
      allocate(character(0)::res) ; RETURN 
    endif
!
    if(present(lb))then ; llb = lb
    else                ; llb = 1           ; endif
    if(present(ub))then ; lub = ub
    else                ; lub = lenchr      ; endif
    linc = 1
!
    call ScopeInc(lenchr,llb,lub,linc,lenres)
    allocate(character(lenres)::res)
    j = 0
    do i = llb,lub,linc
      j = j + 1 ; res(j:j) = this%var(at)%chr(i:i)
    enddo
    RETURN
  end function CAt
!
  pure integer function CToint(this,idx) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in)                 :: idx
  integer                            :: at,io,tmp
    res = 0 ; if(this%length==0) RETURN 
    at = periodicbound(this%length,idx)
    if(len(this%var(at)%chr)==0) RETURN 
    read(this%var(at)%chr,'(i)',iostat=io) tmp
    if(io==0) res = tmp
  end function CToint
!
  pure real function CToReal(this,idx) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in)                 :: idx
  real                               :: tmp
  integer                            :: at,io
    res = 0.0 ; if(this%length==0) RETURN 
    at = periodicbound(this%length,idx)
    if(len(this%var(at)%chr)==0) RETURN 
    read(this%var(at)%chr,'(f)',iostat=io) tmp
    if(io==0) res = tmp
  end function CToReal
!
  function HMAt(this,key) result(res)
  class(hashmap),intent(in)          :: this
  character(*),intent(in)            :: key
  character(:),allocatable           :: res
  integer                            :: hash
    allocate(character(0)::res)
    if(this%length==0) RETURN 
    hash = HMhash32(this,key,this%stack) ; if(hash<=0)RETURN
    res = this%var(hash)%chr ; RETURN
  end function HMAt
!
!---------------------------------------------------------!
!       Routines for Scope.                               !
!---------------------------------------------------------!
!
  pure function CScope(this,lb,ub,inc) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in),optional        :: lb,ub,inc
  integer                            :: llb,lub,linc
  type(vector_Character)             :: res
  integer                            :: i,idx,lnum,tmplen
    if(this%length==0)RETURN
!
    if(present(lb))then ; llb = lb
    else                ; llb = 1           ; endif
    if(present(ub))then ; lub = ub
    else                ; lub = this%length ; endif
    if(present(inc))then; linc = inc
    else                ; linc = 1          ; endif
    call ScopeInc(this%length,llb,lub,linc,res%length)
    call VecReserve(res,res%length)
    i = 0
    do idx = llb,lub,linc
      i = i + 1 ; call ChrNodeAssign(res,i,this%var(idx)%chr)
    enddo
    RETURN
  end function CScope
!---------------------------------------------------------!
!       Routines for LookUp.                              !
!---------------------------------------------------------!
  pure function ILookUp(this) result(res)
  class(vector_integer),intent(in) :: this
  integer                          :: res(1:maxval([1,this%length],1))
    if(this%length==0)then
      res = IEmptyDummy
    else
      res(1:this%length) = this%at(1:this%length)
    endif
    RETURN
  end function ILookUp
!
  pure function RLookUp(this) result(res)
  class(vector_real),intent(in)    :: this
  real                             :: res(1:maxval([1,this%length],1))
    if(this%length==0)then
      res = REmptyDummy 
    else
      res(1:this%length) = this%at(1:this%length)
    endif
    RETURN
  end function RLookUp
!
  pure function DLookUp(this) result(res)
  class(vector_double),intent(in) :: this
  double precision                :: res(1:maxval([1,this%length],1))
    if(this%length==0)then
      res = DEmptyDummy 
    else
      res(1:this%length) = this%at(1:this%length)
    endif
    RETURN
  end function DLookUp
!
  pure function LLookUp(this) result(res)
  class(vector_logical),intent(in) :: this
  logical                          :: res(1:maxval([1,this%length],1))
    if(this%length==0)then
      res = LEmptyDummy 
    else
      res(1:this%length) = this%at(1:this%length)
    endif
    RETURN
  end function LLookUp
!
  pure function CLookUp(this) result(res)
  class(vector_character),intent(in) :: this
  character(this%MaxLen)             :: res(1:maxval([1,this%length],1))
  integer                            :: i,j
    if(this%length==0)then
      res = CEmptyDummy 
    else
      j = 0
      do i=1,this%length
        j = j + 1
        res(i) = this%var(j)%chr
      enddo
    endif
    RETURN
  end function CLookUp
!
!---------------------------------------------------------!
!       Routines for oprator overloading.                 !
!---------------------------------------------------------!
  pure function IIAdd(v1,v2) result(res)
  class(vector_integer),intent(in) :: v1,v2
  integer                          :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) + v2%at(1:siz)
  end function IIAdd
  pure function RRAdd(v1,v2) result(res)
  class(vector_real),intent(in)    :: v1,v2
  real                             :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) + v2%at(1:siz)
  end function RRAdd
  pure function DDAdd(v1,v2) result(res)
  class(vector_double),intent(in)  :: v1,v2
  double precision                 :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) + v2%at(1:siz)
  end function DDAdd
!
  pure function IISub(v1,v2) result(res)
  class(vector_integer),intent(in) :: v1,v2
  integer                          :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) - v2%at(1:siz)
  end function IISub
  pure function RRSub(v1,v2) result(res)
  class(vector_real),intent(in)    :: v1,v2
  real                             :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) - v2%at(1:siz)
  end function RRSub
  pure function DDSub(v1,v2) result(res)
  class(vector_double),intent(in)  :: v1,v2
  double precision                 :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) - v2%at(1:siz)
  end function DDSub
!
  pure function IIMul(v1,v2) result(res)
  class(vector_integer),intent(in) :: v1,v2
  integer                          :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function IIMul
  pure function RRMul(v1,v2) result(res)
  class(vector_real),intent(in)    :: v1,v2
  real                             :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function RRMul
  pure function DDMul(v1,v2) result(res)
  class(vector_double),intent(in)  :: v1,v2
  double precision                 :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function DDMul
!
  pure function IIdiv(v1,v2) result(res)
  class(vector_integer),intent(in) :: v1,v2
  integer                          :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function IIdiv
  pure function RRdiv(v1,v2) result(res)
  class(vector_real),intent(in)    :: v1,v2
  real                             :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function RRdiv
  pure function DDdiv(v1,v2) result(res)
  class(vector_double),intent(in)  :: v1,v2
  double precision                 :: res(minval([v1%size(),v2%size()],1))
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)RETURN ; res = v1%at(1:siz) * v2%at(1:siz)
  end function DDdiv
!
  pure integer function IIDot(v1,v2) result(res)
  class(vector_integer),intent(in) :: v1,v2
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)then ; res=0 ; RETURN ; endif
    res = dot_product(v1%at(1:siz),v2%at(1:siz)) ; RETURN
  end function IIDot
  pure real function RRDot(v1,v2) result(res)
  class(vector_real),intent(in)    :: v1,v2
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)then ; res=0 ; RETURN ; endif
    res = dot_product(v1%at(1:siz),v2%at(1:siz)) ; RETURN
  end function RRDot
  pure integer function DDDot(v1,v2) result(res)
  class(vector_double),intent(in)  :: v1,v2
  integer                          :: siz
    siz = minval([v1%size(),v2%size()],1) ; if(siz<=0)then ; res=0 ; RETURN ; endif
    res = dot_product(v1%at(1:siz),v2%at(1:siz)) ; RETURN
  end function DDDot
!
!---------------------------------------------------------!
!       Routines for Destractor.                          !
!---------------------------------------------------------!
!
  pure subroutine IDestractor(this)
    type(vector_integer),intent(inout) :: this
    if(allocated(this%at))deallocate(this%at)
    this%length = 0 ; this%Stack = -1 ; RETURN
  end subroutine IDestractor
!
  pure subroutine RDestractor(this)
    type(vector_real),intent(inout) :: this
    if(allocated(this%at))deallocate(this%at)
    this%length = 0 ; this%Stack = -1 ; RETURN
  end subroutine RDestractor
!
  pure subroutine DDestractor(this)
    type(vector_double),intent(inout) :: this
    if(allocated(this%at))deallocate(this%at)
    this%length = 0 ; this%Stack = -1 ; RETURN
  end subroutine DDestractor
!
  pure subroutine LDestractor(this)
    type(vector_logical),intent(inout) :: this
    if(allocated(this%at))deallocate(this%at)
    this%length = 0 ; this%Stack = -1 ; RETURN
  end subroutine LDestractor
!
  pure subroutine CDestractor(this)
    type(vector_character),intent(inout) :: this
    if(allocated(this%var))deallocate(this%var)
    this%length = 0 ; this%Stack = -1
    this%MaxLen = 0 ; this%TotalLen = 0
    RETURN
  end subroutine CDestractor
!
  pure subroutine AADestractor(this)
    type(hashmap),intent(inout) :: this
    if(allocated(this%var))deallocate(this%var)
    if(allocated(this%key))deallocate(this%key)
    this%length = 0 ; this%Stack = -1
    this%MaxLen = 0 ; this%TotalLen = 0
    RETURN
  end subroutine AADestractor
!
  pure subroutine ChrNodeDestractor(this)
    type(ChrNode),intent(inout) :: this
    if(allocated(this%chr))deallocate(this%chr)
  end subroutine ChrNodeDestractor
!
!---------------------------------------------------------!
!       Routines for treatment Vector String.             !
!---------------------------------------------------------!
!
  pure integer function CMaxlength(this) result(res)
  class(vector_character),intent(in) :: this
    res = this%Maxlen ; RETURN
  end function CMaxlength
!
  pure function CVCeq(c1,c2) result(res)
  class(vector_character),intent(in) :: c1
  character(*),intent(in)            :: c2
  logical                            :: res(minval([c1%length,1],1))
    if(c1%length==0)then ; res = .FALSE.
    else
      call Ceq(c1%length,CLookUp(c1),c2,res)
    endif
  end function CVCeq
!
  pure function CCVeq(c1,c2) result(res)
  character(*),intent(in)            :: c1
  class(vector_character),intent(in) :: c2
  logical                            :: res(minval([c2%length,1],1))
    if(c2%length==0)then ; res = .FALSE.
    else
      call Ceq(c2%length,CLookUp(c2),c1,res)
    endif
  end function CCVeq
!
  pure function CVCne(c1,c2) result(res)
  class(vector_character),intent(in) :: c1
  character(*),intent(in)            :: c2
  logical                            :: res(maxval([c1%length,1],1))
    if(c1%length==0)then ; res = .TRUE.
    else
      call Cne(c1%length,CLookUp(c1),c2,res)
    endif
  end function CVCne
!
  pure function CCVne(c1,c2) result(res)
  character(*),intent(in)            :: c1
  class(vector_character),intent(in) :: c2
  logical                            :: res(maxval([c2%length,1],1))
    if(c2%length==0)then ; res = .TRUE.
    else
      call Cne(c2%length,CLookUp(c2),c1,res)
    endif
  end function CCVne
!
  pure function CVCVeq(c1,c2) result(res)
  class(vector_character),intent(in) :: c1,c2
  logical                            :: res(maxval([c1%length,1],1),maxval([c2%length,1],1))
    if(c1%length<=0.or.c2%length<=0)then ; res = .FALSE.
    else
      call CCeq(c1%length,CLookUp(c1),c2%length,CLookUp(c2),res)
    endif
  end function CVCVeq
!
  pure function CVCAeq(c1,c2) result(res)
  class(vector_character),intent(in) :: c1
  character(*),intent(in)            :: c2(:)
  logical                            :: res(maxval([c1%length,1],1),maxval([size(c2),1],1))
    if(c1%length<=0.or.size(c2)==0)then ; res = .FALSE.
    else
      call CCeq(c1%length,CLookUp(c1),size(c2),c2,res)
    endif
  end function CVCAeq
!
  pure function CACVeq(c1,c2) result(res)
  character(*),intent(in)            :: c1(:)
  class(vector_character),intent(in) :: c2
  logical                            :: res(maxval([size(c1),1],1),maxval([c2%length,1],1))
    if(size(c1)<=0.or.c2%length==0)then ; res = .FALSE.
    else
      call CCeq(size(c1),c1,c2%length,CLookUp(c2),res)
    endif
    RETURN
  end function CACVeq
!
  pure function CVCVne(c1,c2) result(res)
  class(vector_character),intent(in) :: c1,c2
  logical                            :: res(maxval([c1%length,1],1),maxval([c2%length,1],1))
    if(c1%length<=0.or.c2%length<=0)then ; res = .TRUE.
    else
      call CCne(c1%length,CLookUp(c1),c2%length,CLookUp(c2),res)
    endif
  end function CVCVne
!
  pure function CVCAne(c1,c2) result(res)
  class(vector_character),intent(in) :: c1
  character(*),intent(in)            :: c2(:)
  logical                            :: res(maxval([c1%length,1],1),maxval([size(c2),1],1))
    if(c1%length<=0.or.size(c2)==0)then ; res = .TRUE.
    else
      call CCne(c1%length,CLookUp(c1),size(c2),c2,res)
    endif
  end function CVCAne
!
  pure function CACVne(c1,c2) result(res)
  character(*),intent(in)            :: c1(:)
  class(vector_character),intent(in) :: c2
  logical                            :: res(maxval([size(c1),1],1),maxval([c2%length,1],1))
    if(size(c1)<=0.or.c2%length==0)then ; res = .TRUE.
    else
      call CCne(size(c1),c1,c2%length,CLookUp(c2),res)
    endif
    RETURN
  end function CACVne
!
  pure subroutine Ceq(n,c1,c2,res)
  integer,intent(in)                 :: n
  character(*),intent(in)            :: c1(n),c2
  logical,intent(out)                :: res(n)
    res = c1==c2
  end subroutine Ceq
  pure subroutine Cne(n,c1,c2,res)
  integer,intent(in)                 :: n
  character(*),intent(in)            :: c1(n),c2
  logical,intent(out)                :: res(n)
    res = c1/=c2
  end subroutine Cne
!
  pure subroutine CCeq(n1,c1,n2,c2,res)
  integer,intent(in)                 :: n1,n2
  character(*),intent(in)            :: c1(n1),c2(n2)
  logical,intent(out)                :: res(n1,n2)
  integer                            :: i,j
    do j=1,n2
      do i=1,n1
        res(i,j) = c1(i)==c2(j)
      enddo
    enddo
  end subroutine CCeq
  pure subroutine CCne(n1,c1,n2,c2,res)
  integer,intent(in)                 :: n1,n2
  character(*),intent(in)            :: c1(n1),c2(n2)
  logical,intent(out)                :: res(n1,n2)
  integer                            :: i,j
    do j=1,n2
      do i=1,n1
        res(i,j) = c1(i)/=c2(j)
      enddo
    enddo
  end subroutine CCne
!
  pure function CLarge(this,idx) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in)                 :: idx
  character(:),allocatable           :: res
  integer                            :: at,i,ilen
    if(this%length==0)then
      allocate(character(0)::res)
      res = CEmptyDummy ; RETURN
    endif
!
    at = periodicbound(this%length,idx)
    ilen = len_trim(this%var(at)%chr)
    allocate(character(ilen)::res)
    do i = 1, ilen
      if(this%var(at)%chr(i:i)>='a'.and.this%var(at)%chr(i:i)<='z')then
        res(i:i) = char(ichar(this%var(at)%chr(i:i))-32)
      else
        res(i:i) = this%var(at)%chr(i:i)
      endif
    enddo
    RETURN
  end function CLarge
!
  pure function CSmall(this,idx) result(res)
  class(vector_character),intent(in) :: this
  integer,intent(in)                 :: idx
  character(:),allocatable           :: res
  integer                            :: at,i,ilen
    if(this%length==0)then
      allocate(character(0)::res)
      res = CEmptyDummy ; RETURN
    endif
!
    at = periodicbound(this%length,idx)
    ilen = len_trim(this%var(at)%chr)
    allocate(character(ilen)::res)
    do i = 1, ilen
      if(this%var(at)%chr(i:i)>='A'.and.this%var(at)%chr(i:i)<='Z')then
        res(i:i) = char(ichar(this%var(at)%chr(i:i))+32)
      else
        res(i:i) = this%var(at)%chr(i:i)
      endif
    enddo
    RETURN
  end function CSmall
!
  pure integer function CFind(this,String,back,lb,ub) result(res)
  class(vector_character),intent(in) :: this
  character(*),intent(in)            :: String
  logical,intent(in),optional        :: back
  integer,intent(in),optional        :: lb,ub
  integer                            :: llb,lub,inc,tmp,i
    llb = 1 ; lub = this%length ; inc = 1
    if(present(lb)) llb = maxval([lb,llb],1)
    if(present(ub)) lub = minval([ub,lub],1)
!
    if(present(back))then
      if(back)then ; tmp = llb ; llb = lub ; lub = tmp ; inc = -inc ; endif
    endif
!
    res = 0
    do i = llb,lub,inc
      if(string==this%var(i)%chr)then
        res = i ; RETURN
      endif
    enddo
  end function CFind
!
  pure function CJoin(this,lb,ub,inc,delimiter) result(res)
  class(vector_character),intent(in)      :: this
  character(*),intent(in),optional        :: delimiter
  integer,intent(in),optional             :: lb,ub,inc
  integer                                 :: llb,lub,linc,lnum
  character(:),allocatable                :: res,LocDelim
  integer                                 :: Head, Tail, LenDelim
  integer                                 :: at,i,JoinLen,nword,nword2
    if(allocated(res))deallocate(res)
    if(this%length<=0)then
      allocate(character(0)::res) ; RETURN
    endif
!
    if(present(delimiter))then ; LenDelim = Len(delimiter)
    else                       ; LenDelim = 1
    endif
!
    if(present(lb))then ; llb = lb
    else                ; llb = 1           ; endif
    if(present(ub))then ; lub = ub
    else                ; lub = this%length ; endif
    if(present(inc))then ; linc = inc
    else                ; linc = 1          ; endif
    call ScopeInc(this%length,llb,lub,linc,lnum)
!
    Joinlen = 0 ; nword = 0
    do i = llb,lub,linc
      nword = nword + 1
      JoinLen = JoinLen + len(this%var(i)%chr) + LenDelim
    enddo
    joinlen = maxval([Joinlen-LenDelim,0],1)
!
    allocate(character(JoinLen)::res)
    allocate(character(LenDelim)::LocDelim)
    if(present(delimiter))then ; LocDelim = delimiter
    else                       ; LocDelim = " "
    endif
!
    res(:) = CEmptyDummy ; if(this%length==0)RETURN
!
    Tail = 0 ; nword2 = 0
    do i=llb,lub,linc
      nword2 = nword2 + 1
      Head = Tail + 1
      Tail = Tail + len(this%var(i)%chr)
      if(Tail>=Head) res(Head:Tail) = this%var(i)%chr
      if(nword2==nword)EXIT
      Head = Tail + 1
      Tail = Tail + LenDelim
      if(Tail>=Head) res(Head:Tail) = LocDelim
    enddo
    RETURN
  end function CJoin
!
  pure subroutine CSplit(this,var,delimiter,pickup)
  class(vector_character),intent(inout) :: this
  character(*),intent(in)               :: var
  character(1),intent(in),optional      :: delimiter(:)
  character(1),intent(in),optional      :: pickup(:)
  character(1),allocatable              :: deli(:),pick(:)
  integer                               :: LenVar, i, j
  integer                               :: Head, Tail
  logical                               :: p
    if(present(delimiter))then
      if(size(delimiter)>0)then
        allocate(deli(size(delimiter))) ; deli = delimiter
      endif
    else
      allocate(deli(1))           ; deli = [" "]
    endif
!
    if(present(pickup))then
      allocate(pick(size(pickup))) ; pick = pickup
    endif
    p = allocated(pick)
!
    head = 0 ; tail = 0
    do i=1,len(var)
      if(any(var(i:i)==deli))then
        if(head/=0) call this%push(var(head:tail))
        head = 0 ; CYCLE
      endif
      if(p)then
        if(any(var(i:i)==pick))then
          if(head/=0) call this%push(var(head:tail))
          call this%push(var(i:i))
          head = 0 ; CYCLE
        endif
      endif
      if(head == 0) head = i
      tail = i
    enddo
    if(head/=0) call this%push(var(head:tail))
    RETURN
  end subroutine CSplit
!
  subroutine CTextWrap(this,text,width,delimiter)
  class(vector_character),intent(inout) :: this
  character(*),intent(in)               :: text
  integer,intent(in)                    :: width
  character(1),intent(in),optional      :: delimiter(:)
  type(vector_character)                :: words
  type(vector_integer)                  :: head,tail
  integer,allocatable                   :: minima(:),lengths(:),breaks(:)
  integer,allocatable                   :: slack(:,:)
  integer                               :: width2
  integer                               :: i, j, x
  integer                               :: NAN
    call this%clear()
    if(present(delimiter))then
      call CSplit(words,text,delimiter)
    else
      call CSplit(words,text)
    endif
!
    allocate(lengths(words%size()))
    allocate(slack(words%size(),words%size()))
    allocate(minima(words%size()+1),breaks(words%size()))
!
    width2 = width + 1 ; NAN = 10**9
    minima = NAN ; minima(1) = 0  ; breaks = 1
!
    do i = 1, words%size()
      lengths(i) = len(words%var(i)%chr)
    enddo
!
    slack = 0
    do j = 1,words%size()
      slack(j,j) = width2 - lengths(j)
      do i = j+1,words%size()
        slack(j,i) = slack(j,i-1) - lengths(i) - 1
      enddo
    enddo
!
    do j = 1,words%size()
      do i = j,1,-1
        if(slack(i,j)<0)then
          EXIT
        else
          x = minima(i) + slack(i,j)**2
        endif
        if(minima(j+1)>x)then
          minima(j+1) = x
          breaks(j) = i
        endif
      enddo
    enddo
!
    j = words%size() + 1
    do while(j>1)
      i = breaks(j-1)
      call head%push(i) ; call tail%push(j-1)
      j = i
    enddo
!
    call this%reserve(head%size())
    do i = head%size(),1,-1
      call this%push(CJoin(words,head%at(i),tail%at(i)))
    enddo
    deallocate(lengths,slack,minima,breaks)
    call words%clear()
    call head%clear() ; call tail%clear()
    RETURN
  end subroutine CTextwrap
!
! subroutine CTextWrap(this,text,width,delim)
! class(vector_character),intent(inout) :: this
! character(*),intent(in)               :: text
! integer,intent(in)                    :: width
! character(1),intent(in),optional      :: delim(:)
! type(vector_character)                :: words
! type(vector_integer)                  :: head,tail
! integer,allocatable                   :: offsets(:),minima(:),breaks(:)
! integer                               :: width2
! integer                               :: Offset, NLine
! integer                               :: i, j, k, x, y, N
! integer                               :: r, edge
! integer                               :: NAN
! logical                               :: NextLine(this%TotalLen)
!   if(len_trim(text)==0)RETURN
!   if(present(delim))then
!     call CSplit(words,text,delim)
!   else
!     call CSplit(words,text)
!   endif
! print*,words%lookup()
!
!   width2 = width + 1
!   NAN = 10**9
!
!   allocate(offsets(words%size()))
!   offsets(1) = 0
!   do i = 2, words%size()
!     offsets(i) = offsets(i-1) +  len(words%var(i)%chr)
!   enddo
!   print*,offsets
!
!   allocate(minima(words%size()+1),breaks(words%size()))
!   minima = NAN ; minima(1) = 0  ; breaks = 0
!   n = words%size() + 1
!   Offset = 1 ; i = 0
!   do
!     r = minval([n,2**(i+1)+1]) ; edge = 2**i + 1 + Offset
!print*,"off",offset,"edge",edge,"r+off",r+offset
!     call smawk((/(j,j=Offset,edge-1)/),edge-offset,(/(k,k=edge,r+offset)/),r+offset-edge+1)
!     x = minima(r-1+offset)
!     do j = 2**i,r-1
!       y = cost(j + offset,r-1+offset)
!    print*,"x ",x,"y ",y
!       if(y<=x)then
!         n = n - j
!         i = 0 ;  offset = offset + j
!         EXIT
!       endif
!     enddo
!     if(r==n)EXIT
!     i = i + 1
!   enddo
!   print*,"|wordsize", words%size()
!   print*,breaks
!   j = words%size()+1
!   do while(j>1)
!     i = breaks(j)
!     call head%push(i) ; call tail%push(j-1)
!     print*,i,j
!     j = i
!   enddo
!   do i = head%size(),1,-1
!     do j = head%at(i),tail%at(i)
!       write(*,'(a,x)',advance="no"),words%var(j)%chr
!     enddo
!     print*,""
!   enddo
!   return
!   deallocate(offsets,minima,breaks)
!   j = words%size() + 1
!   do while(j>1)
!     i = breaks(j-1)
!     call head%push(i) ; call tail%push(j-1)
!     print*,i,j
!     j = i
!   enddo
!   do i = head%size(),1,-1
!     do j = head%at(i),tail%at(i)
!       write(*,'(a,x)',advance="no"),words%var(j)%chr
!     enddo
!     print*,""
!   enddo
!   RETURN
!contains
! integer function cost(i,j) result(res)
! integer, intent(in) :: i,j
! integer             :: w
!   w = offsets(j) - offsets(i) + j - i - 1
!   if(w>width2)then ; res = 10**7 * (w - width2)
!   else            ; res = minima(i) + (width2 - w)**2
!   endif
!   RETURN
! end function cost
!!
!  recursive subroutine smawk(row,nrow,col,ncol)
!  integer,intent(in)  :: nrow, ncol
!  integer,intent(in)  :: row(nrow), col(ncol)
!  integer             :: stack(nrow)
!  integer             :: i,j,ssize,c,fin,sl
!    i = 1 ; sl = 0
!    do while(i<=nrow)
!      if(sl==0)then
!        sl = 1 ; stack(sl) = row(i) ; i = i + 1
!      else
!        c = col(sl)
!        if(cost(stack(sl),c)<cost(row(i),c))then
!          if(sl<=ncol)then
!            sl = sl + 1 ; stack(sl) = row(i)
!          endif
!          i = i + 1
!        else
!          sl = sl - 1
!        endif
!      endif
!    enddo
! 
!    if(ncol>1)call smawk(stack(1:sl),sl,col(2::2),size(col(2::2)))
! 
!    i = 1 ; j = 1
!    do while(j<=ncol)
!      if(j<ncol)then
!        fin = breaks(col(j+1))
!      else
!        fin = stack(sl)
!      endif
!      c = cost(stack(i),col(j))
!      if(c<minima(col(j)))then
!        minima(col(j)) = c
!        breaks(col(j)) = stack(i)
!      endif
!      if(stack(i)<fin)then
!        i = i + 1
!      else
!        j = j + 2
!      endif
!     enddo
!    print*,""
!   end subroutine smawk
!  end subroutine CTextWrap
!
  function CUniq(this) result(res)
  class(vector_character),intent(inout) :: this
  type(vector_character)                :: res
  character(this%maxlen)                :: A(this%length)
  logical                               :: isNew(this%length)
  integer                               :: i,j
    call CDestractor(res) ; if(this%length==0)RETURN
!
    do i = 1,this%length
      A(i) = this%var(i)%chr
    enddo
!
    call QS_Chr(A,1,this%length,this%maxlen)
!
    isNew(1:1) = .TRUE.
    do i=2,this%length
      isNew(i) = A(i-1).ne.A(i)
    enddo
!
    call res%reserve(count(isNew))
!
    do i=1,this%length
      if(isNew(i)) call res%push(A(i))
    enddo
    RETURN
  end function CUniq
!
  recursive subroutine QS_Chr(A,first,last,N)
  integer,intent(in)         :: first,last,N
  character(N),intent(inout) :: A(:)
  character(N)               :: X,T
  integer                    :: i,j
    X=A(int((first+last)*0.5))
    i=first
    j=last
    do
        do while (A(i)<X)
            i=i+1
        enddo
        do while (A(j)>X)
            j=j-1
        enddo
        if(i>=j)EXIT
        T=A(i) ; A(i)=A(j) ; A(j)=T
        i=i+1
        j=j-1
    enddo
    if(first<i-1)call QS_Chr(A,first,i-1,N)
    if(j+1<last)call QS_Chr(A,j+1,last,N)
    RETURN
  end subroutine QS_Chr
!
  recursive subroutine QS_Int(A,first,last)
  integer,intent(inout)      :: A(:)
  integer                    :: X,T
  integer,intent(in)         :: first,last
  integer                    :: i,j
    X=A(int((first+last)*0.5))
    i=first
    j=last
    do
      do while (A(i)<X)
          i=i+1
      enddo
      do while (A(j)>X)
          j=j-1
      enddo
      if(i>=j)EXIT
      T=A(i) ; A(i)=A(j) ; A(j)=T
      i=i+1
      j=j-1
    enddo
    if(first<i-1)call QS_Int(A,first,i-1)
    if(j+1<last)call QS_Int(A,j+1,last)
    RETURN
  end subroutine QS_Int
!
 function IUniq(this) result(res)
 class(vector_integer),intent(in)      :: this
 integer,allocatable                   :: res(:)
 integer                               :: A(this%length)
 logical                               :: isNew(this%length)
 integer                               :: i,j
   if(this%length==0)then
     allocate(res(0)) ; res=0 ; RETURN
   endif

   A = this%at
   call QS_Int(A,1,this%length)

   isNew(1:1) = .TRUE.
   do i=2,this%length
     isNew(i) = A(i-1).ne.A(i)
   enddo

   allocate(res(count(isNew)))
   j = 0
   do i=1,this%length
     if(isNew(i))then
       j=j+1 ; res(j) = A(i)
     endif
   enddo
   RETURN
 end function IUniq
!
 subroutine IUniqSelf(this)
 class(vector_integer)                 :: this
 integer,allocatable                   :: res(:)
 integer                               :: A(this%length)
 logical                               :: isNew(this%length)
 integer                               :: i,j
   if(this%length==0)RETURN

   A = this%at
   call QS_Int(A,1,this%length)

   isNew(1:1) = .TRUE.
   do i=2,this%length
     isNew(i) = A(i-1).ne.A(i)
   enddo

   allocate(res(count(isNew)))
   j = 0
   do i=1,this%length
     if(isNew(i))then
       j=j+1 ; res(j) = A(i)
     endif
   enddo
   this = res
   RETURN
 end subroutine IUniqSelf
!
 function RSort(this) result(res)
 class(vector_real),intent(in) :: this
 real                          :: res(this%length)
 logical                       :: isNew(this%length)
 integer                       :: i,j
   if(this%length==0)RETURN

   res = this%at
   call QS_Real(res,1,this%length)
 end function RSort
!
  recursive subroutine QS_Real(A,first,last)
  real,intent(inout)         :: A(:)
  integer                    :: X,T
  integer,intent(in)         :: first,last
  integer                    :: i,j
    X=A(int((first+last)*0.5))
    i=first
    j=last
    do
      do while (A(i)<X)
          i=i+1
      enddo
      do while (A(j)>X)
          j=j-1
      enddo
      if(i>=j)EXIT
      T=A(i) ; A(i)=A(j) ; A(j)=T
      i=i+1
      j=j-1
    enddo
    if(first<i-1)call QS_Real(A,first,i-1)
    if(j+1<last)call QS_Real(A,j+1,last)
    RETURN
  end subroutine QS_Real
!
  pure subroutine HMrehash(this,OldStack)
  class(hashmap),intent(inout) :: this
  integer,intent(in)           :: OldStack
  type(ChrNode),allocatable    :: Tmpkey(:),Tmp(:)
  integer                      :: i,hash
    call move_alloc(from=this%key,to=Tmpkey)
    allocate(Tmp(this%stack),this%key(this%stack))
    do i=1,minval([this%stack,OldStack],1)
      if(allocated(Tmpkey(i)%chr))then
        if(Tmpkey(i)%chr/='')then
          hash = HMhash32(this,Tmpkey(i)%chr,this%Stack)
          allocate(character(0)::This%key(hash)%chr,Tmp(hash)%chr)
          this%key(hash)%chr = Tmpkey(i)%chr
          Tmp(hash)%chr      = this%var(i)%chr
        endif
      endif
    enddo
    call move_alloc(from=Tmp,to=this%var) ; RETURN
  end subroutine HMrehash
!
  pure integer function HMhash32(this,key,stack) result (res)
  class(hashmap),intent(in)    :: this
  character(*),intent(in)      :: key
  integer,intent(in)           :: stack
  integer                      :: i
    res = FNV_OFFSET_32
    do i=1,len(key)
      res = ieor(res,ichar(key(i:i)))
      res = FNV_PRIME_32*res
    enddo
    res = mod(abs(res),stack)+1
    do i=1,this%stack
      if(.not.allocated(this%key(res)%chr))then ; RETURN
      elseif(this%key(res)%chr==key)then        ; RETURN
      endif
      res = mod(res,stack) + 1
    enddo
    res = 0 ; RETURN
  end function HMhash32
!---------------------------------------------------------!
!       Routines for periodic boundary.                   !
!---------------------------------------------------------!
!
  pure integer function PeriodicBound(length,idx) result(res)
  integer,intent(in)                  :: length,idx
    if(idx>length)then
      res = modulo(idx,length)
      if(res==0) res = length
    elseif(idx<=0)then
      res = modulo(idx,length) + 1
    else
      res = idx
    endif
  end function PeriodicBound
!
  pure subroutine ScopeInc(length,llb,lub,linc,lnum)
  integer,intent(in)                 :: length
  integer,intent(inout)              :: llb,lub,linc
  integer,intent(out)                :: lnum
    llb = periodicbound(length,llb)
    lub = periodicbound(length,lub)
    if(linc==0) linc=1
    if(llb>lub) linc = - abs(linc)
    lnum = int((abs(lub-llb))/abs(linc)) + 1
    RETURN
  end subroutine ScopeInc
!
!---------------------------------------------------------!
!       Routines for treatment ChrNode.                   !
!---------------------------------------------------------!
!
  elemental subroutine ChrNodeMove(from,to)
  type(ChrNode),intent(inout) :: from,to
    call move_alloc(from=from%chr,to=to%chr) ; RETURN
  end subroutine ChrNodeMove
!
  elemental subroutine ChrNodeCopy(from,to)
  type(ChrNode),intent(in)    :: from
  type(ChrNode),intent(inout) :: to
    if(allocated(to%chr))deallocate(to%chr)
    if(.not.allocated(from%chr))RETURN
    allocate(character(len(from%chr))::to%chr)
    to%chr(:) = from%chr
    RETURN
  end subroutine ChrNodeCopy
!
  pure subroutine ChrNodeAssign(this,idx,var)
  class(vector_character),intent(inout) :: this
  integer,intent(in)                    :: idx
  character(*),intent(in)               :: var
  integer                               :: VarLen
    VarLen = len_trim(adjustl(var))
    if(.not.allocated(this%var(idx)%chr))allocate(character(VarLen)::this%var(idx)%chr)
    this%var(idx)%chr = trim(adjustl(var))
    this%TotalLen = this%TotalLen + VarLen
    if(VarLen>this%MaxLen)this%MaxLen = VarLen
    RETURN
  end subroutine ChrNodeAssign
end module spur_vector
