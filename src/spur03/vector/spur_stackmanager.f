module spur_stackmanager
  implicit none
  private
  public :: stackmanager
  public :: stack_push,stack_pop,stack_reserve,stack_shrinktofit,stack_clear
!
! default values
  integer(4),parameter  :: sl_def    =  0            ! head index
  integer(4),parameter  :: ss_def    = -1            ! tail index
  integer(4),parameter  :: sc        =  2            ! multiple incriment
  integer(4),parameter  :: sm_def    =  HUGE(sc)     ! max size
! class def
  type stackmanager
    private
    integer(4)          :: length = sl_def
    integer(4)          :: stack  = ss_def
  contains
    procedure           :: size           => SSize
    procedure           :: max_size       => SMaxsize
    procedure           :: capacity       => SCapacity
    procedure           :: empty          => SEmpty
  end type stackmanager
!
contains
  pure integer(4) function SSize(this) result(res)
  class(stackmanager),intent(in) :: this
    res = this%length
  end function SSize
!
  pure integer(4) function SCapacity(this) result(res)
  class(stackmanager),intent(in) :: this
    res = maxval([this%stack,0],1)
  end function SCapacity
!
  pure integer(4) function SMaxsize(this) result(res)
  class(stackmanager),intent(in) :: this
    res = sm_def
  end function SMaxsize
!
  pure logical function SEmpty(this) result(res)
  class(stackmanager),intent(in) :: this
    res = this%length==sl_def
  end function SEmpty
!
  pure subroutine stack_extension(this)
  class(stackmanager),intent(inout) :: this
    if(this%stack>=this%length) RETURN
    if(this%capacity()==0) this%stack = maxval([this%length,1],1)
    do while(this%stack<this%length)
      this%stack = this%stack * sc
    enddo
  end subroutine stack_extension
!
  pure elemental subroutine stack_push(this,num)
  class(stackmanager),intent(inout) :: this
  integer(4),intent(in)             :: num
    this%length = maxval([this%length+num,sl_def],1)
    call stack_extension(this)
  end subroutine stack_push
!
  pure elemental subroutine stack_pop(this,num)
  class(stackmanager),intent(inout) :: this
  integer(4),intent(in)             :: num
    this%length = maxval([this%length-num,sl_def],1)
    call stack_extension(this)
  end subroutine stack_pop
!
  pure elemental subroutine stack_reserve(this,length)
  class(stackmanager),intent(inout) :: this
  integer(4),intent(in)             :: length
    this%length = maxval([length,sl_def],1)
    this%stack  = maxval([length,ss_def],1)
  end subroutine stack_reserve
!
  pure elemental subroutine stack_shrinktofit(this)
  class(stackmanager),intent(inout) :: this
    this%stack  = this%length
  end subroutine stack_shrinktofit
!
  pure elemental subroutine stack_clear(this)
  class(stackmanager),intent(inout) :: this
    this%length = sl_def ; this%stack  = ss_def
  end subroutine stack_clear
end module spur_stackmanager
