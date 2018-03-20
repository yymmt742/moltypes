module spur_array_int4
  use spur_rangemanager
  implicit none
  private
  public :: array_int4
  public :: assignment(=)
!
  integer(4),parameter       :: I4Dummy = 0
!
  type,extends(rangemanager) :: array_int4
    private
    integer(4),allocatable,public :: at(:)
  contains
    procedure,private   :: I4ReserveLength
    procedure,private   :: I4ReserveRange
    generic             :: reserve     => I4ReserveLength,I4ReserveRange
    procedure,private   :: IPush
    procedure,private   :: I1Push
    procedure,private   :: RPush
    procedure,private   :: R1Push
    procedure,private   :: DPush
    procedure,private   :: D1Push
    procedure,private   :: LPush
    procedure,private   :: L1Push
    procedure,private   :: CPush
    procedure,private   :: C1Push
    generic             :: push        => Ipush,I1push, &
                        &                 Rpush,R1push, &
                        &                 Dpush,D1push, &
                        &                 Lpush,L1push, &
                        &                 Cpush,C1push
    procedure           :: pop         => I4Pop
    procedure           :: lookup      => I4LookUp
    procedure           :: ShrinkToFit => I4ShrinkToFit
    final               :: I1dDestractor
  end type array_int4
!
  interface assignment(=)
    module procedure IAssign, I1Assign,&
                   & RAssign, R1Assign,&
                   & DAssign, D1Assign,&
                   & LAssign, L1Assign,&
                   & CAssign, C1Assign,&
                   & ArrayAssign
  end interface assignment(=)
!
contains
  pure subroutine I4Expand(this)
  class(array_int4),intent(inout) :: this
  integer(4),allocatable          :: tmp(:)
  integer(4)                      :: old(2)
    if(this%capacity()<=0) RETURN
    old = [lbound(this%at,1),ubound(this%at,1)]
    if(old(1)<=this%head.and.old(2)>=this%tail) RETURN
    allocate(tmp(this%lb:this%ub))
    if(allocated(this%at)) tmp(old(1):old(2)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine I4Expand
!
  pure subroutine I4ReserveLength(this,length)
  class(array_int4),intent(inout) :: this
  integer(4),intent(in)        :: length
    this%tail = length - this%head + 1
    this%ub   = this%tail
    call I4Expand(this)
  end subroutine I4ReserveLength
!
  pure subroutine I4ReserveRange(this,head,tail)
  class(array_int4),intent(inout) :: this
  integer(4),intent(in)        :: head,tail
    call range_reserve(this,head,tail)
    call I4Expand(this)
  end subroutine I4ReserveRange
!
  pure subroutine I4ShrinkToFit(this)
  class(array_int4),intent(inout) :: this
  integer(4),allocatable          :: tmp(:)
  integer(4)                      :: old(2)
    if(this%capacity()<=0) RETURN
    old = [lbound(this%at,1),ubound(this%at,1)]
    if(old(1)==this%head.and.old(2)==this%tail) RETURN
    call range_shrinktofit(this)
    allocate(tmp(this%lb:this%ub))
    if(allocated(this%at)) tmp(old(1):old(2)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine I4ShrinkToFit
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine IAssign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  integer(4),intent(in)        :: RHS
    call I1dDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = RHS
  end subroutine IAssign
!
  pure subroutine I1Assign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  integer(4),intent(in)        :: RHS(:)
    call I1dDestractor(LHS) ; call LHS%reserve(lbound(RHS,1),ubound(RHS,1)) ; LHS%at = RHS
  end subroutine I1Assign
!
  pure subroutine RAssign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  real,intent(in)              :: RHS
    call I1dDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = nint(RHS)
  end subroutine RAssign
!
  pure subroutine R1Assign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  real,intent(in)              :: RHS(:)
    call I1dDestractor(LHS) ; call LHS%reserve(lbound(RHS,1),ubound(RHS,1)) ; LHS%at = nint(RHS)
  end subroutine R1Assign
!
  pure subroutine DAssign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  double precision,intent(in)  :: RHS
    call I1dDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = nint(RHS)
  end subroutine DAssign
!
  pure subroutine D1Assign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  double precision,intent(in)  :: RHS(:)
    call I1dDestractor(LHS) ; call LHS%reserve(lbound(RHS,1),ubound(RHS,1)) ; LHS%at = nint(RHS)
  end subroutine D1Assign
!
  pure subroutine LAssign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  logical,intent(in)           :: RHS
    call I1dDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = RHS
  end subroutine LAssign
!
  pure subroutine L1Assign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  logical,intent(in)           :: RHS(:)
    call I1dDestractor(LHS) ; call LHS%reserve(lbound(RHS,1),ubound(RHS,1)) ; LHS%at = RHS
  end subroutine L1Assign
!
  pure subroutine CAssign(LHS,RHS)
  use spur_string
  class(array_int4),intent(inout) :: LHS
  character(*),intent(in)         :: RHS
    call I1dDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = tonum(RHS,0)
  end subroutine CAssign
!
  pure subroutine C1Assign(LHS,RHS)
  use spur_string
  class(array_int4),intent(inout) :: LHS
  character(*),intent(in)         :: RHS(:)
  integer                         :: i
    call I1dDestractor(LHS) ; call LHS%reserve(lbound(RHS,1),ubound(RHS,1))
    LHS%at = tonum(RHS,[(0,i=1,size(RHS))])
  end subroutine C1Assign
!
  pure subroutine ArrayAssign(LHS,RHS)
  class(array_int4),intent(inout) :: LHS
  class(array_int4),intent(in)    :: RHS
    call I1dDestractor(LHS)
    LHS%head = RHS%head ; LHS%tail = RHS%tail
    LHS%lb   = RHS%lb   ; LHS%ub   = RHS%ub
    call I4Expand(LHS)
    if(allocated(RHS%at)) LHS%at = RHS%at
  end subroutine ArrayAssign
!
  pure subroutine IPush(this,var)
  class(array_int4),intent(inout) :: this
  integer(4),intent(in)        :: var
    call range_push(this) ; call I4Expand(this) ; this%at(this%tail) = var
  end subroutine IPush
!
  pure subroutine I1Push(this,var)
  class(array_int4),intent(inout) :: this
  integer(4),intent(in)        :: var(:)
  integer(4)                   :: tmp
    tmp = this%tail + 1 ; call range_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%tail) = var
  end subroutine I1Push
!
  pure subroutine RPush(this,var)
  class(array_int4),intent(inout) :: this
  real,intent(in)              :: var
    call range_push(this) ; call I4Expand(this) ; this%at(this%tail) = nint(var)
  end subroutine RPush
!
  pure subroutine R1Push(this,var)
  class(array_int4),intent(inout) :: this
  real,intent(in)              :: var(:)
  integer(4)                   :: tmp
    tmp = this%tail + 1 ; call range_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%tail) = nint(var)
  end subroutine R1Push
!
  pure subroutine DPush(this,var)
  class(array_int4),intent(inout) :: this
  double precision,intent(in)  :: var
    call range_push(this) ; call I4Expand(this) ; this%at(this%tail) = nint(var)
  end subroutine DPush
!
  pure subroutine D1Push(this,var)
  class(array_int4),intent(inout) :: this
  double precision,intent(in)  :: var(:)
  integer(4)                   :: tmp
    tmp = this%tail + 1 ; call range_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%tail) = nint(var)
  end subroutine D1Push
!
  pure subroutine LPush(this,var)
  class(array_int4),intent(inout) :: this
  logical,intent(in)           :: var
    call range_push(this) ; call I4Expand(this) ; this%at(this%tail) = var
  end subroutine LPush
!
  pure subroutine L1Push(this,var)
  class(array_int4),intent(inout) :: this
  logical,intent(in)           :: var(:)
  integer(4)                   :: tmp
    tmp = this%tail + 1 ; call range_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%tail) = var
  end subroutine L1Push
!
  pure subroutine CPush(this,var)
  use spur_string
  class(array_int4),intent(inout) :: this
  character(*),intent(in)         :: var
    call range_push(this) ; call I4Expand(this) ; this%at(this%tail) = tonum(var,0)
  end subroutine CPush
!
  pure subroutine C1Push(this,var)
  use spur_string
  class(array_int4),intent(inout) :: this
  character(*),intent(in)         :: var(:)
  integer(4)                      :: i,tmp
    tmp = this%tail + 1 ; call range_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%tail) = tonum(var,[(I4Dummy,i=1,size(var))])
  end subroutine C1Push
!
  function I4Pop(this) result (res)
  class(array_int4),intent(inout) :: this
  integer(4)                      :: res
    if(this%size()==0)then
      res = I4Dummy
    else
      res = this%at(this%tail)
      call range_pop(this)
    endif
  end function I4Pop
!
  pure function I4LookUp(this) result(res)
  class(array_int4),intent(in) :: this
  integer(4)                   :: res(this%head:maxval([this%tail,this%head],1))
    if(this%size()==0)then
      res = 0
    else
      res(this%head:this%tail) = this%at(this%head:this%tail)
    endif
  end function I4LookUp
!
  pure subroutine I1dDestractor(this)
  type(array_int4),intent(inout) :: this
    call this%clear()
    if(allocated(this%at))deallocate(this%at)
  end subroutine I1dDestractor
end module spur_array_int4
