module dbscan_tool
implicit none
  private
  public dbscan
contains
  pure subroutine dbscan(d,n,x,eps,npts,label)
  integer,intent(in)           :: d,n
  double precision,intent(in)  :: x(d,n)
  double precision,intent(in)  :: eps
  integer,intent(in)           :: npts
  integer,intent(inout)        :: label(n)
  double precision             :: eps2,v(d)
  logical                      :: neighbor(n),visited(n),c(n)
  integer                      :: i,j,k,nlabel
    eps2 = eps * eps ; label = 0
!
    nlabel = 0
    c = .FALSE. ; visited = .FALSE.
!
    do j=1,n
      if(visited(j))CYCLE ; visited(j) = .TRUE.
      do i=1,n
        v = x(:,i) - x(:,j)
        neighbor(i) = dot_product(v,v) <= eps2
      enddo
      if(count(neighbor)<=npts) CYCLE
      nlabel = nlabel + 1
      i = 0 ; c = neighbor
      do while(i<=n)
        i = i + 1
        if(.not.c(i).or.visited(i))CYCLE
        visited(i) = .TRUE.
        do k=1,n
          v = x(:,i) - x(:,k)
          neighbor(k) = dot_product(v,v) <= eps2
        enddo
        if(count(neighbor)>npts)then
          c = neighbor.or.c ; i = 0
        endif
      enddo
      do i=1,n
        if(c(i)) label(i) = nlabel
      enddo
    enddo
  end subroutine dbscan
end module dbscan_tool
