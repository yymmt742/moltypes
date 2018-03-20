module spur_vector_int4
  use spur_stackmanager
  implicit none
  private
  public :: vector_int4
  public :: assignment(=)
!
  integer(4),parameter       :: I4Dummy = 0
!
  type,extends(stackmanager) :: vector_int4
    private
    integer(4),allocatable,public :: at(:)
  contains
    procedure           :: reserve     => I4Reserve
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
    procedure           :: sort        => Isort
    procedure           :: uniq        => Iuniq
    procedure           :: ShrinkToFit => I4ShrinkToFit
    procedure           :: clear       => I4Clear
    final               :: I4Destractor
  end type vector_int4
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
  class(vector_int4),intent(inout) :: this
  integer(4),allocatable           :: tmp(:)
  integer                          :: s
    s = size(this%at) ; if(.not.allocated(this%at)) s=0
    if(this%capacity()==0.or.s>=this%size()) RETURN
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:s) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine I4Expand
!
  pure subroutine I4Reserve(this,length)
  class(vector_int4),intent(inout) :: this
  integer(4),intent(in)            :: length
  integer(4),allocatable           :: tmp(:)
    call stack_reserve(this,length)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine I4Reserve
!
  pure subroutine I4ShrinkToFit(this)
  class(vector_int4),intent(inout) :: this
  integer(4),allocatable           :: tmp(:)
    if(this%capacity()==0.or.this%capacity()==this%size()) RETURN
    call stack_shrinktofit(this)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine I4ShrinkToFit
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine IAssign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS
    call I4Destractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = RHS
  end subroutine IAssign
!
  pure subroutine I1Assign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS(:)
    call I4Destractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = RHS
  end subroutine I1Assign
!
  pure subroutine RAssign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  real,intent(in)                  :: RHS
    call I4Destractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = nint(RHS)
  end subroutine RAssign
!
  pure subroutine R1Assign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  real,intent(in)                  :: RHS(:)
    call I4Destractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = nint(RHS)
  end subroutine R1Assign
!
  pure subroutine DAssign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  double precision,intent(in)      :: RHS
    call I4Destractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = nint(RHS)
  end subroutine DAssign
!
  pure subroutine D1Assign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  double precision,intent(in)      :: RHS(:)
    call I4Destractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = nint(RHS)
  end subroutine D1Assign
!
  pure subroutine LAssign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  logical,intent(in)               :: RHS
    call I4Destractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = RHS
  end subroutine LAssign
!
  pure subroutine L1Assign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  logical,intent(in)               :: RHS(:)
    call I4Destractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = RHS
  end subroutine L1Assign
!
  pure subroutine CAssign(LHS,RHS)
  use spur_string
  class(vector_int4),intent(inout) :: LHS
  character(*),intent(in)          :: RHS
    call I4Destractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = tonum(RHS,I4Dummy)
  end subroutine CAssign
!
  pure subroutine C1Assign(LHS,RHS)
  use spur_string
  class(vector_int4),intent(inout) :: LHS
  character(*),intent(in)         :: RHS(:)
  integer                         :: i
    call I4Destractor(LHS) ; call LHS%reserve(size(RHS))
    LHS%at = tonum(RHS,[(I4Dummy,i=1,size(RHS))])
  end subroutine C1Assign
!
  pure subroutine ArrayAssign(LHS,RHS)
  class(vector_int4),intent(inout) :: LHS
  class(vector_int4),intent(in)    :: RHS
    call I4Destractor(LHS)
    call LHS%reserve(RHS%size())
    call I4Expand(LHS)
    if(allocated(RHS%at)) LHS%at = RHS%at
  end subroutine ArrayAssign
!
  pure subroutine IPush(this,var)
  class(vector_int4),intent(inout) :: this
  integer(4),intent(in)            :: var
    call stack_push(this,1) ; call I4Expand(this) ; this%at(this%size()) = var
  end subroutine IPush
!
  pure subroutine I1Push(this,var)
  class(vector_int4),intent(inout) :: this
  integer(4),intent(in)            :: var(:)
  integer(4)                       :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%size()) = var
  end subroutine I1Push
!
  pure subroutine RPush(this,var)
  class(vector_int4),intent(inout) :: this
  real,intent(in)              :: var
    call stack_push(this,1) ; call I4Expand(this) ; this%at(this%size()) = nint(var)
  end subroutine RPush
!
  pure subroutine R1Push(this,var)
  class(vector_int4),intent(inout) :: this
  real,intent(in)              :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%size()) = nint(var)
  end subroutine R1Push
!
  pure subroutine DPush(this,var)
  class(vector_int4),intent(inout) :: this
  double precision,intent(in)  :: var
    call stack_push(this,1) ; call I4Expand(this) ; this%at(this%size()) = nint(var)
  end subroutine DPush
!
  pure subroutine D1Push(this,var)
  class(vector_int4),intent(inout) :: this
  double precision,intent(in)  :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%size()) = nint(var)
  end subroutine D1Push
!
  pure subroutine LPush(this,var)
  class(vector_int4),intent(inout) :: this
  logical,intent(in)           :: var
    call stack_push(this,1) ; call I4Expand(this) ; this%at(this%size()) = var
  end subroutine LPush
!
  pure subroutine L1Push(this,var)
  class(vector_int4),intent(inout) :: this
  logical,intent(in)           :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%size()) = var
  end subroutine L1Push
!
  pure subroutine CPush(this,var)
  use spur_string
  class(vector_int4),intent(inout) :: this
  character(*),intent(in)         :: var
    call stack_push(this,1) ; call I4Expand(this) ; this%at(this%size()) = tonum(var,I4Dummy)
  end subroutine CPush
!
  pure subroutine C1Push(this,var)
  use spur_string
  class(vector_int4),intent(inout) :: this
  character(*),intent(in)         :: var(:)
  integer(4)                      :: i,tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call I4Expand(this) ; this%at(tmp:this%size()) = tonum(var,[(I4Dummy,i=1,size(var))])
  end subroutine C1Push
!
  function I4Pop(this) result (res)
  class(vector_int4),intent(inout) :: this
  integer(4)                       :: res
    if(this%size()==0)then
      res = I4Dummy
    else
      res = this%at(this%size()) ; call stack_pop(this,1)
    endif
  end function I4Pop
!
  pure function I4LookUp(this) result(res)
  class(vector_int4),intent(in) :: this
  integer(4)                   :: res(this%size())
    if(this%size()==0)then
      res = I4Dummy
    else
      res = this%at(:this%size())
    endif
  end function I4LookUp
!
  pure subroutine IUniq(this,reverse)
  class(vector_int4),intent(inout) :: this
  logical,intent(in),optional      :: reverse
  logical                          :: isNew(this%size())
  integer                          :: i,j
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
      else
        call qs_up(this,1,this%size())
      endif
    else
      call qs_up(this,1,this%size())
    endif
!
    isNew(1:1) = .TRUE.
    do i=2,this%size()
      isNew(i) = this%at(i-1)/=this%at(i)
    enddo
!
    j = 0
    do i=1,this%size()
      if(isNew(i))then
        j = j + 1 ; this%at(j) = this%at(i)
      endif
    enddo
    call stack_reserve(this,count(isNew))
  end subroutine IUniq
!
  pure subroutine ISort(this,reverse)
  class(vector_int4),intent(inout) :: this
  logical,intent(in),optional      :: reverse
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
        return
      endif
    endif
    call qs_up(this,1,this%size())
  end subroutine ISort
!
  pure recursive subroutine qs_up(this,first,last)
  class(vector_int4),intent(inout) :: this
  integer,intent(in)              :: first,last
  integer(4)                      :: x,t
  integer                         :: i,j
    x=this%at(int((first+last)/2))
    i=first
    j=last
    do
      do while (this%at(i)<x)
        i=i+1
      enddo
      do while (this%at(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=this%at(i) ; this%at(i)=this%at(j) ; this%at(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_up(this,first,i-1)
    if(j+1<last)  call qs_up(this,j+1,last)
  end subroutine qs_up
!
  pure recursive subroutine qs_down(this,first,last)
  class(vector_int4),intent(inout) :: this
  integer,intent(in)               :: first,last
  integer(4)                       :: x,t
  integer                          :: i,j
    x=this%at(int((first+last)/2))
    i=first
    j=last
    do
      do while (this%at(i)>x)
        i=i+1
      enddo
      do while (this%at(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=this%at(i) ; this%at(i)=this%at(j) ; this%at(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_down(this,first,i-1)
    if(j+1<last)  call qs_down(this,j+1,last)
  end subroutine qs_down
!
  pure elemental subroutine I4Clear(this)
  class(vector_int4),intent(inout) :: this
    call stack_clear(this)
    if(allocated(this%at)) deallocate(this%at)
  end subroutine I4Clear
!
  pure subroutine I4Destractor(this)
  type(vector_int4),intent(inout) :: this
    call this%clear()
  end subroutine I4Destractor
end module spur_vector_int4
