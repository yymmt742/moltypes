module spur_itertools
  implicit none
  private
  public :: sgn,IterScope,PeriodicBound
  integer(4),parameter     :: int4 = 4
  integer(4),parameter     :: bsi4 = bit_size(int4) - 1
contains
  pure elemental function sgn(n) result(res)
  integer(int4),intent(in) :: n
  integer(int4)            :: res
    res = n ; if(res==0) RETURN
    res = ishft(btest(res,bsi4),1)+1
  end function sgn
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
