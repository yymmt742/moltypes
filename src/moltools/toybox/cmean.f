program cmean
implicit none
real,allocatable    :: xyz(:,:),mass(:),cent(:,:)
real                :: lb(3),ub(3)
integer,allocatable :: id(:)
integer             :: n,k
integer             :: i,j
character(2)        :: dum(3)
  call loaddata(n,xyz,mass,lb,ub)
  k = 3
  allocate(id(n),cent(3,k))
  call init_id(n,k,lb,ub,id,cent)
  dum = ["O ","N ","Cl"]
  do j = 1,10
    call nearest_id(n,k,xyz,cent,id)
    print'(i0,/)',n+k
    do i=1,n
      print'(a,3f9.3)',dum(id(i)),[1.0,1.0,10.0]*xyz(:,i)
    enddo
    do i=1,k
      print'(a,i0,3f9.3)',"C",i,[1.0,1.0,10.0]*cent(:,i)
    enddo
    call calc_centroid(n,k,xyz,mass,id,cent)
  enddo
  stop
contains
  subroutine loaddata(n,xyz,mass,lb,ub)
  use spur_optparse
  use spur_io
  use spur_vector
  integer,intent(out)          :: n
  real,allocatable,intent(out) :: xyz(:,:),mass(:)
  real,intent(out)             :: lb(3),ub(3)
  type(optparse)               :: op
  type(fileio)                 :: inp
  type(vector_real)            :: xx,yy,zz,mm
  real                         :: tmp(4)
  integer                      :: i,dev,is
    call op%add_description('Cluster analysis by X-means method.')
    call op%parser()
    if(op%narg()==0) call op%call_usage()
    n = 0
    lb = HUGE(0.0) ; ub = -HUGE(0.0)
    do i = 1,op%narg()
      call inp%fetch(op%args(i))
      if(.not.inp%isExist().or.inp%isAbnormal())CYCLE
      dev = inp%devn()
      do
        read(dev,*,iostat=is)tmp
        if(is>0)CYCLE ; if(is<0)EXIT
        n = n + 1
        call xx%push(tmp(1))
        call yy%push(tmp(2))
        call zz%push(tmp(3))
        call mm%push(tmp(4))
      enddo
    enddo
!
    if(xx%size()==0) STOP 1
    allocate(xyz(3,n),mass(n))
    do i = 1,n
      xyz(:,i) = [xx%at(i),yy%at(i),zz%at(i)]
    enddo
    mass = mm%lookup()
!
    lb(1) = lbound(xx%lookup(),1)
    ub(1) = ubound(xx%lookup(),1)
    lb(2) = lbound(yy%lookup(),1)
    ub(2) = ubound(yy%lookup(),1)
    lb(3) = lbound(zz%lookup(),1)
    ub(3) = ubound(zz%lookup(),1)
    RETURN
  end subroutine loaddata
!
  subroutine init_id(n,k,lb,ub,id,cent)
  integer,intent(in)              :: n,k
  real,intent(in)                 :: lb(3),ub(3)
  integer,intent(out)             :: id(n)
  real,intent(out)                :: cent(3,k)
  real                            :: rnd(n+3*k)
  integer                         :: i
    call random_number(rnd)
    id = int(rnd(1:n)*real(k)) + 1
    do i=1,k
      cent(:,i) = rnd(n+i*3-2:n+i*3)*(ub-lb) + lb
    enddo
  end subroutine init_id
!
  subroutine nearest_id(n,k,xyz,cent,id)
  integer,intent(in)              :: n,k
  real,intent(in)                 :: xyz(3,n)
  real,intent(in)                 :: cent(3,k)
  integer,intent(out)             :: id(n)
  real                            :: v(3),r,rold
  integer                         :: i,j
    do j=1,n
      rold = HUGE(0.0)
      do i=1,k
        v = cent(:,i) - xyz(:,j)
        r = dot_product(v,v)
        if(r>rold)CYCLE
        rold = r
        id(j) = i
      enddo
    enddo
  end subroutine nearest_id
!
  subroutine calc_centroid(n,k,xyz,mass,id,cent)
  integer,intent(in)              :: n,k
  real,intent(in)                 :: xyz(3,n),mass(n)
  integer,intent(in)              :: id(n)
  real,intent(out)                :: cent(3,k)
  real                            :: cnt(n)
  integer                         :: i
    cent = 0.0 ; cnt = 0.0
    do i=1,n
      cent(:,id(i)) = cent(:,id(i)) + xyz(:,i) * mass(i)
      cnt(id(i))    = cnt(id(i)) + mass(i)
    enddo
    do i=1,k
      cent(:,i) = cent(:,i) / cnt(i)
    enddo
  end subroutine calc_centroid
end program cmean
