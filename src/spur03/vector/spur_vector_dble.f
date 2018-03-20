module spur_vector_dble
  use spur_stackmanager
  implicit none
  private
  public :: vector_dble
  public :: assignment(=)
!
  double precision,parameter       :: DDummy = 0.d0
!
  type,extends(stackmanager) :: vector_dble
    private
    double precision,allocatable,public :: at(:)
  contains
    procedure           :: reserve     => DReserve
    procedure,private   :: IPush
    procedure,private   :: I1Push
    procedure,private   :: RPush
    procedure,private   :: R1Push
    procedure,private   :: DPush
    procedure,private   :: D1Push
    procedure,private   :: CPush
    procedure,private   :: C1Push
    generic             :: push        => Ipush,I1push, &
                        &                 Rpush,R1push, &
                        &                 Dpush,D1push, &
                        &                 Cpush,C1push
    procedure           :: pop         => DPop
    procedure           :: lookup      => DLookUp
    procedure           :: sort        => Dsort
    procedure           :: ShrinkToFit => DShrinkToFit
    procedure           :: clear       => DClear
    final               :: DDestractor
  end type vector_dble
!
  interface assignment(=)
    module procedure IAssign, I1Assign,&
                   & RAssign, R1Assign,&
                   & DAssign, D1Assign,&
                   & CAssign, C1Assign,&
                   & ArrayAssign
  end interface assignment(=)
!
contains
  pure subroutine DExpand(this)
  class(vector_dble),intent(inout) :: this
  double precision,allocatable     :: tmp(:)
  integer                          :: s
    s = size(this%at) ; if(.not.allocated(this%at)) s=0
    if(this%capacity()==0.or.s>=this%size()) RETURN
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:s) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine DExpand
!
  pure subroutine DReserve(this,length)
  class(vector_dble),intent(inout) :: this
  integer(4),intent(in)            :: length
  double precision,allocatable     :: tmp(:)
    call stack_reserve(this,length)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine DReserve
!
  pure subroutine DShrinkToFit(this)
  class(vector_dble),intent(inout) :: this
  double precision,allocatable     :: tmp(:)
    if(this%capacity()==0.or.this%capacity()==this%size()) RETURN
    call stack_shrinktofit(this)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine DShrinkToFit
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine IAssign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS
    call DDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = dble(RHS)
  end subroutine IAssign
!
  pure subroutine I1Assign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS(:)
    call DDestractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = dble(RHS)
  end subroutine I1Assign
!
  pure subroutine RAssign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  real,intent(in)                  :: RHS
    call DDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = dble(RHS)
  end subroutine RAssign
!
  pure subroutine R1Assign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  real,intent(in)                  :: RHS(:)
    call DDestractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = dble(RHS)
  end subroutine R1Assign
!
  pure subroutine DAssign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  double precision,intent(in)      :: RHS
    call DDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = RHS
  end subroutine DAssign
!
  pure subroutine D1Assign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  double precision,intent(in)      :: RHS(:)
    call DDestractor(LHS) ; call LHS%reserve(size(RHS)) ; LHS%at = RHS
  end subroutine D1Assign
!
  pure subroutine CAssign(LHS,RHS)
  use spur_string
  class(vector_dble),intent(inout) :: LHS
  character(*),intent(in)          :: RHS
    call DDestractor(LHS) ; call LHS%reserve(1) ; LHS%at(1) = tonum(RHS,DDummy)
  end subroutine CAssign
!
  pure subroutine C1Assign(LHS,RHS)
  use spur_string
  class(vector_dble),intent(inout) :: LHS
  character(*),intent(in)         :: RHS(:)
  integer                         :: i
    call DDestractor(LHS) ; call LHS%reserve(size(RHS))
    LHS%at = tonum(RHS,[(DDummy,i=1,size(RHS))])
  end subroutine C1Assign
!
  pure subroutine ArrayAssign(LHS,RHS)
  class(vector_dble),intent(inout) :: LHS
  class(vector_dble),intent(in)    :: RHS
    call DDestractor(LHS)
    call LHS%reserve(RHS%size())
    call DExpand(LHS)
    if(allocated(RHS%at)) LHS%at = RHS%at
  end subroutine ArrayAssign
!
  pure subroutine IPush(this,var)
  class(vector_dble),intent(inout) :: this
  integer(4),intent(in)            :: var
    call stack_push(this,1) ; call DExpand(this) ; this%at(this%size()) = dble(var)
  end subroutine IPush
!
  pure subroutine I1Push(this,var)
  class(vector_dble),intent(inout) :: this
  integer(4),intent(in)        :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call DExpand(this) ; this%at(tmp:this%size()) = dble(var)
  end subroutine I1Push
!
  pure subroutine RPush(this,var)
  class(vector_dble),intent(inout) :: this
  real,intent(in)              :: var
    call stack_push(this,1) ; call DExpand(this) ; this%at(this%size()) = dble(var)
  end subroutine RPush
!
  pure subroutine R1Push(this,var)
  class(vector_dble),intent(inout) :: this
  real,intent(in)              :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call DExpand(this) ; this%at(tmp:this%size()) = dble(var)
  end subroutine R1Push
!
  pure subroutine DPush(this,var)
  class(vector_dble),intent(inout) :: this
  double precision,intent(in)  :: var
    call stack_push(this,1) ; call DExpand(this) ; this%at(this%size()) = var
  end subroutine DPush
!
  pure subroutine D1Push(this,var)
  class(vector_dble),intent(inout) :: this
  double precision,intent(in)  :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call DExpand(this) ; this%at(tmp:this%size()) = var
  end subroutine D1Push
!
  pure subroutine CPush(this,var)
  use spur_string
  class(vector_dble),intent(inout) :: this
  character(*),intent(in)         :: var
    call stack_push(this,1) ; call DExpand(this) ; this%at(this%size()) = tonum(var,DDummy)
  end subroutine CPush
!
  pure subroutine C1Push(this,var)
  use spur_string
  class(vector_dble),intent(inout) :: this
  character(*),intent(in)         :: var(:)
  integer(4)                      :: i,tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call DExpand(this) ; this%at(tmp:this%size()) = tonum(var,[(DDummy,i=1,size(var))])
  end subroutine C1Push
!
  function DPop(this) result (res)
  class(vector_dble),intent(inout) :: this
  double precision                 :: res
    if(this%size()==0)then
      res = DDummy
    else
      res = this%at(this%size()) ; call stack_pop(this,1)
    endif
  end function DPop
!
  pure function DLookUp(this) result(res)
  class(vector_dble),intent(in) :: this
  double precision              :: res(this%size())
    if(this%size()==0)then
      res = DDummy
    else
      res = this%at(:this%size())
    endif
  end function DLookUp
!
  pure subroutine DSort(this,reverse)
  class(vector_dble),intent(inout) :: this
  logical,intent(in),optional      :: reverse
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
        return
      endif
    endif
    call qs_up(this,1,this%size())
  end subroutine DSort
!
  pure recursive subroutine qs_up(this,first,last)
  class(vector_dble),intent(inout) :: this
  integer,intent(in)               :: first,last
  double precision                 :: x,t
  integer                          :: i,j
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
  class(vector_dble),intent(inout) :: this
  integer,intent(in)               :: first,last
  double precision                 :: x,t
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
  pure elemental subroutine DClear(this)
  class(vector_dble),intent(inout) :: this
    call stack_clear(this)
    if(allocated(this%at))deallocate(this%at)
  end subroutine DClear
!
  pure subroutine DDestractor(this)
  type(vector_dble),intent(inout) :: this
    call this%clear()
  end subroutine DDestractor
end module spur_vector_dble
