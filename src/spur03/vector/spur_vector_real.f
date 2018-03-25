module spur_vector_real
  use spur_stackmanager
  implicit none
  private
  public :: vector_real
  public :: assignment(=)
!
  real,parameter       :: RDummy = 0.0
!
  type,extends(stackmanager) :: vector_real
    private
    real,allocatable,public :: at(:)
  contains
    procedure           :: reserve     => RReserve
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
    procedure           :: pop         => RPop
    procedure           :: lookup      => RLookUp
    procedure           :: sort        => Rsort
    procedure           :: ShrinkToFit => RShrinkToFit
    procedure           :: clear       => RClear
    final               :: RDestractor
  end type vector_real
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
  pure subroutine RExpand(this)
  class(vector_real),intent(inout) :: this
  real,allocatable                 :: tmp(:)
  integer                          :: s
    s = size(this%at) ; if(.not.allocated(this%at)) s=0
    if(this%capacity()==0.or.s>=this%size()) RETURN
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:s) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine RExpand
!
  pure subroutine RReserve(this,length)
  class(vector_real),intent(inout) :: this
  integer(4),intent(in)            :: length
  real,allocatable                 :: tmp(:)
    call stack_reserve(this,length)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine RReserve
!
  pure subroutine RShrinkToFit(this)
  class(vector_real),intent(inout) :: this
  real,allocatable                 :: tmp(:)
    if(this%capacity()==0.or.this%capacity()==this%size()) RETURN
    call stack_shrinktofit(this)
    allocate(tmp(this%capacity()))
    if(allocated(this%at)) tmp(:size(this%at)) = this%at
    call move_alloc(from=tmp,to=this%at)
  end subroutine RShrinkToFit
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine IAssign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS
    call RDestractor(LHS) ; call LHS%push(real(RHS))
  end subroutine IAssign
!
  pure subroutine I1Assign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  integer(4),intent(in)            :: RHS(:)
    call RDestractor(LHS) ; call LHS%push(real(RHS))
  end subroutine I1Assign
!
  pure subroutine RAssign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  real,intent(in)                  :: RHS
    call RDestractor(LHS) ; call LHS%push(RHS)
  end subroutine RAssign
!
  pure subroutine R1Assign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  real,intent(in)                  :: RHS(:)
    call RDestractor(LHS) ; call LHS%push(RHS)
  end subroutine R1Assign
!
  pure subroutine DAssign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  double precision,intent(in)      :: RHS
    call RDestractor(LHS) ; call LHS%push(RHS)
  end subroutine DAssign
!
  pure subroutine D1Assign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  double precision,intent(in)      :: RHS(:)
    call RDestractor(LHS) ; call LHS%push(RHS)
  end subroutine D1Assign
!
  pure subroutine CAssign(LHS,RHS)
  use spur_string
  class(vector_real),intent(inout) :: LHS
  character(*),intent(in)          :: RHS
    call RDestractor(LHS) ; call LHS%push(tonum(RHS,RDummy))
  end subroutine CAssign
!
  pure subroutine C1Assign(LHS,RHS)
  use spur_string
  class(vector_real),intent(inout) :: LHS
  character(*),intent(in)         :: RHS(:)
  integer                         :: i
    call RDestractor(LHS) ; call LHS%push(tonum(RHS,[(RDummy,i=1,size(RHS))]))
  end subroutine C1Assign
!
  pure subroutine ArrayAssign(LHS,RHS)
  class(vector_real),intent(inout) :: LHS
  class(vector_real),intent(in)    :: RHS
    call RDestractor(LHS) ; call LHS%push(RHS%lookup())
  end subroutine ArrayAssign
!
  pure subroutine IPush(this,var)
  class(vector_real),intent(inout) :: this
  integer(4),intent(in)            :: var
    call stack_push(this,1) ; call RExpand(this) ; this%at(this%size()) = real(var)
  end subroutine IPush
!
  pure subroutine I1Push(this,var)
  class(vector_real),intent(inout) :: this
  integer(4),intent(in)        :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call RExpand(this) ; this%at(tmp:this%size()) = real(var)
  end subroutine I1Push
!
  pure subroutine RPush(this,var)
  class(vector_real),intent(inout) :: this
  real,intent(in)              :: var
    call stack_push(this,1) ; call RExpand(this) ; this%at(this%size()) = var
  end subroutine RPush
!
  pure subroutine R1Push(this,var)
  class(vector_real),intent(inout) :: this
  real,intent(in)              :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call RExpand(this) ; this%at(tmp:this%size()) = var
  end subroutine R1Push
!
  pure subroutine DPush(this,var)
  class(vector_real),intent(inout) :: this
  double precision,intent(in)  :: var
    call stack_push(this,1) ; call RExpand(this) ; this%at(this%size()) = real(var)
  end subroutine DPush
!
  pure subroutine D1Push(this,var)
  class(vector_real),intent(inout) :: this
  double precision,intent(in)  :: var(:)
  integer(4)                   :: tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call RExpand(this) ; this%at(tmp:this%size()) = real(var)
  end subroutine D1Push
!
  pure subroutine CPush(this,var)
  use spur_string
  class(vector_real),intent(inout) :: this
  character(*),intent(in)         :: var
    call stack_push(this,1) ; call RExpand(this) ; this%at(this%size()) = tonum(var,RDummy)
  end subroutine CPush
!
  pure subroutine C1Push(this,var)
  use spur_string
  class(vector_real),intent(inout) :: this
  character(*),intent(in)         :: var(:)
  integer(4)                      :: i,tmp
    tmp = this%size() + 1 ; call stack_push(this,size(var))
    call RExpand(this) ; this%at(tmp:this%size()) = tonum(var,[(RDummy,i=1,size(var))])
  end subroutine C1Push
!
  function RPop(this) result (res)
  class(vector_real),intent(inout) :: this
  real                             :: res
    if(this%size()==0)then
      res = RDummy
    else
      res = this%at(this%size()) ; call stack_pop(this,1)
    endif
  end function RPop
!
  pure function RLookUp(this) result(res)
  class(vector_real),intent(in) :: this
  real                          :: res(this%size())
    if(this%size()==0)then
      res = RDummy
    else
      res = this%at(:this%size())
    endif
  end function RLookUp
!
  pure subroutine RSort(this,reverse)
  class(vector_real),intent(inout) :: this
  logical,intent(in),optional      :: reverse
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
        return
      endif
    endif
    call qs_up(this,1,this%size())
  end subroutine RSort
!
  pure recursive subroutine qs_up(this,first,last)
  class(vector_real),intent(inout) :: this
  integer,intent(in)               :: first,last
  real                             :: x,t
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
  class(vector_real),intent(inout) :: this
  integer,intent(in)               :: first,last
  real                             :: x,t
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
  pure elemental subroutine RClear(this)
  class(vector_real),intent(inout) :: this
    call stack_clear(this)
    if(allocated(this%at))deallocate(this%at)
  end subroutine RClear
!
  pure subroutine RDestractor(this)
  type(vector_real),intent(inout) :: this
    call this%clear()
  end subroutine RDestractor
end module spur_vector_real
