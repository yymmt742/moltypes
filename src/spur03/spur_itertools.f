module spur_itertools
  implicit none
  private
  public :: IterScope,PlusMinus,PeriodicBound
contains
  pure integer function PlusMinus(n) result(res)
  integer,intent(in) :: n
    res = n/abs(n)
  end function PlusMinus
!
  pure subroutine IterScope(length,lb,ub,inc,num)
  integer,intent(in)                 :: length
  integer,intent(inout)              :: lb,ub,inc
  integer,intent(out)                :: num
  integer                            :: swp
    if(length<=0.or.inc==0)then
      num = 0 ; RETURN
    endif
    lb  = periodicbound(length,lb)
    ub  = periodicbound(length,ub)
    if(lb>ub) inc = - abs(inc)
    num = int((abs(ub-lb))/abs(inc)) + 1
  end subroutine IterScope
!
  pure integer function PeriodicBound(length,idx) result(res)
  integer,intent(in)                  :: length,idx
    if(length<=0)then
      res = 0 ; RETURN
    endif
    if(idx>length)then
      res = modulo(idx,length) ; if(res==0) res = length
    elseif(idx<=0)then
      res = modulo(idx,length) + 1
    else
      res = idx
    endif
  end function PeriodicBound
end module spur_itertools
