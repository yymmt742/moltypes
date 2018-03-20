module spur_rangemanager
  implicit none
  private
  public :: rangemanager
  public :: range_push,range_pop,range_reserve,range_shrinktofit
!
! default values
  integer(4),parameter  :: sh_def    =  1            ! head index
  integer(4),parameter  :: st_def    =  0            ! tail index
  integer(4),parameter  :: sc        =  2            ! multiple incriment
  integer(4),parameter  :: sm_def    =  HUGE(sc) ! max size
! class def
  type rangemanager
    integer(4)          :: head   = sh_def
    integer(4)          :: tail   = st_def
    integer(4)          :: lb     = sh_def
    integer(4)          :: ub     = st_def
  contains
    procedure           :: size           => SSize
    procedure           :: max_size       => SMaxsize
    procedure           :: capacity       => SCapacity
    procedure           :: empty          => SEmpty
    procedure           :: clear          => SClear
  end type rangemanager
!
contains
  pure integer(4) function SSize(this) result(res)
  class(rangemanager),intent(in) :: this
    res = maxval([this%tail-this%head,-1],1) + 1
  end function SSize
!
  pure integer(4) function SCapacity(this) result(res)
  class(rangemanager),intent(in) :: this
    res = maxval([this%ub-this%lb,-1],1) + 1
  end function SCapacity
!
  pure integer(4) function SMaxsize(this) result(res)
  class(rangemanager),intent(in) :: this
    res = sm_def
  end function SMaxsize
!
  pure logical function SEmpty(this) result(res)
  class(rangemanager),intent(in) :: this
    res = this%size()==st_def
  end function SEmpty
!
  pure elemental subroutine SClear(this)
  class(rangemanager),intent(inout) :: this
    this%head   = sh_def ; this%tail   = st_def
    this%lb     = sh_def ; this%ub     = st_def
  end subroutine SClear
!
  pure subroutine range_extension(this)
  class(rangemanager),intent(inout) :: this
    if(this%lb<=this%head.and.this%ub>=this%tail) RETURN
    if(this%capacity()==0)then
      this%lb = this%head
      this%ub = this%tail
      RETURN
    endif
!
    do while(this%lb>this%head)
      this%lb = this%ub - this%capacity() * sc
    enddo
    do while(this%ub<this%tail)
      this%ub = this%lb + this%capacity() * sc
    enddo
  end subroutine range_extension
!
  pure elemental subroutine range_push(this,num)
  class(rangemanager),intent(inout) :: this
  integer(4),intent(in),optional    :: num
    if(present(num))then
      if(num>0)then
        this%tail = this%tail + num
      elseif(num<0)then
        this%head = this%head + num
      endif
    else
      this%tail = this%tail + 1
    endif
    call range_extension(this)
  end subroutine range_push
!
  pure elemental subroutine range_pop(this,num)
  class(rangemanager),intent(inout) :: this
  integer(4),intent(in),optional    :: num
    if(present(num))then
      if(num>0)then
        this%tail = maxval([this%tail-num,this%head],1)
      elseif(num<0)then
        this%head = minval([this%head-num,this%tail],1)
      endif
    else
      this%tail = maxval([this%tail-1,this%head],1)
    endif
    call range_extension(this)
  end subroutine range_pop
!
  pure elemental subroutine range_reserve(this,head,tail)
  class(rangemanager),intent(inout) :: this
  integer(4),intent(in)             :: head,tail
    this%head   = head
    this%lb     = head
    this%tail   = tail
    this%ub     = tail
  end subroutine range_reserve
!
  pure elemental subroutine range_shrinktofit(this)
  class(rangemanager),intent(inout) :: this
    this%lb     = this%head
    this%ub     = this%tail
  end subroutine range_shrinktofit
end module spur_rangemanager
