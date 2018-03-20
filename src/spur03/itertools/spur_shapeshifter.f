module spur_shapeshifter
implicit none
private
public  :: CompleteMask,SpreadMask,LT_shape
contains
  pure function CompleteMask(mask,num) result(res)
  logical,intent(in)            :: mask(:)
  integer,intent(in)            :: num
  logical                       :: res(num)
  integer                       :: l,u,s
    l = lbound(mask,1) ; u = minval([ubound(mask,1),l+num-1],1)
    s = u - l + 1
    res(:s) = mask(l:u) ; if(s<num) res(s+1:) = .FALSE.
  end function CompleteMask
!
  pure function SpreadMask(mask,num,d) result(res)
  logical,intent(in)            :: mask(:)
  integer,intent(in)            :: num,d
  logical                       :: res(d,num)
  integer                       :: l,u,i,j
    l = lbound(mask,1) ; u = minval([ubound(mask,1),l+num-1],1)
    res = .FALSE.
    do i=l,u
      j = j + 1 ; res(:,j) = mask(i)
    enddo
  end function SpreadMask
!
  pure logical function LT_shape(L,R) result(res)
  integer,intent(in) :: L(:),R(:)
  integer            :: i,N
    N = minval([size(L),size(R)],1)
    do i=1,N-1
      res = L(i) /= R(i) ; if(res) RETURN
    enddo
    res = L(N) < R(N)
  end function LT_shape
end module spur_shapeshifter
