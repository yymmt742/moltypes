module xmeans_tool
implicit none
  private
  public xmeans
  double precision,parameter   :: Pi = 3.1415926535897932d0
  integer,parameter            :: MAX_ITER = 1000000
  double precision             :: rpi
  integer                      :: nlabel
  character(4)                 :: dm(18)
contains
  subroutine xmeans(d,n,x,label)
  integer,intent(in)           :: d,n
  double precision,intent(in)  :: x(d,n)
  integer,intent(out)          :: label(n)
  double precision             :: nx(d,n),c(d)
  integer                      :: i,j
  integer                      :: k
    dm = ["C_","N_","O_","F_","Si","P_","S_","Cl","Na",&
         &"Br","Pb","Au","Ag","Kr","Mg","Al","I_","Ce"]
    rpi = sqrt((0.5d0/Pi)**d)
    nlabel = 0
    label = 0 ; k = 2
    call normalize(d,n,x,nx)
    print'(i0,/)',n
    c = 0.d0
    call kmeans(d,n,nx,c,0.d0)
    !call kmeans(d,n,k,nx,label)
  end subroutine xmeans
!
  subroutine normalize(d,n,x,nx)
  integer,intent(in)           :: d,n
  double precision,intent(in)  :: x(d,n)
  double precision,intent(out) :: nx(d,n)
  double precision             :: mean(d),sd,revn
  integer                      :: i
    mean = 0.d0
    revn = 1.d0 / dble(n)
    do i=1,n
      mean = mean + x(:,i)
    enddo
    mean = mean * revn
!
    sd = 0.d0
    do i=1,n
      nx(:,i) = x(:,i) - mean
      sd = sd + dot_product(nx(:,i),nx(:,i))
    enddo
    sd = sqrt( dble(n) / sd )
    nx = nx * sd
  end subroutine normalize
!
  recursive subroutine kmeans(d,n,x,c,b)
  integer,intent(in)           :: d,n
  double precision,intent(in)  :: x(d,n),c(d),b
  integer                      :: label(n)
  double precision             :: cent(d,2),v(d,2),th
  double precision             :: b1,b2
  double precision             :: rnd(d*2)
  logical                      :: l12(n),mask(d,n)
  integer                      :: i,j,n1,n2
    call random_number(rnd)
    rnd = rnd*2.d0 - 1.d0
    cent(:,1) = c + rnd(1:d)
    cent(:,2) = c + rnd(d+1:2*d)
    v = 0.d0
    do i=1,MAX_ITER
      call nearest_id(d,n,2,x,cent,label)
      call calc_centroid(d,n,2,x,label,cent)
      v = cent - v
      th = dot_product(v(:,1),v(:,1)) + dot_product(v(:,2),v(:,2))
      if(th<1.0E-10)EXIT
      v = cent
    enddo
!
    l12 = label == 1
    do i = 1,n
      mask(:,i) = l12(i)
    enddo
    n1  = count(l12)    ;  n2  = n - n1
!
    b1 = BIC(d,n1,reshape(pack(x,mask),[d,n1]))
    b2 = BIC(d,n2,reshape(pack(x,.not.mask),[d,n2]))
!
    if(b<b1+b2)then
      if(n1>0) call kmeans(d,n1,reshape(pack(x,mask),[d,n1]),cent(:,1),b1)
      if(n2>0) call kmeans(d,n2,reshape(pack(x,.not.mask),[d,n2]),cent(:,2),b2)
    else
      nlabel = nlabel + 1
      do i=1,n
        print'(a2,i0,3f8.3)',dm(mod(nlabel,18)+1),nlabel,10.d0*x(:,i)
      enddo
    endif
!
  end subroutine kmeans
!
  subroutine nearest_id(d,n,k,x,cent,label)
  integer,intent(in)              :: d,n,k
  double precision,intent(in)     :: x(d,n)
  double precision,intent(in)     :: cent(d,k)
  integer,intent(out)             :: label(n)
  double precision                :: v(d),r,rold
  integer                         :: i,j
    do j=1,n
      rold = HUGE(0.d0)
      do i=1,k
        v = cent(:,i) - x(:,j)
        r = dot_product(v,v)
        if(r>rold)CYCLE
        rold = r
        label(j) = i
      enddo
    enddo
  end subroutine nearest_id
!
  subroutine calc_centroid(d,n,k,x,label,cent)
  integer,intent(in)              :: d,n,k
  double precision,intent(in)     :: x(d,n)
  integer,intent(in)              :: label(n)
  double precision,intent(out)    :: cent(d,k)
  double precision                :: revn(k)
  integer                         :: i
    cent = 0.d0 ; revn = 0.d0
    do i=1,n
      cent(:,label(i)) = cent(:,label(i)) + x(:,i)
      revn(label(i)) = revn(label(i))+ 1.d0
    enddo
    do i=1,k
      if(revn(i)<1.0E-10)CYCLE
      cent(:,i) = cent(:,i) / revn(i)
    enddo
  end subroutine calc_centroid
!
  function BIC(d,n,x) result (res)
  integer,intent(in)              :: d,n
  double precision,intent(in)     :: x(d,n)
  double precision                :: res
  double precision                :: cent(d),disp,sd,v(d),vv(n)
  integer                         :: i
    cent = 0.d0 ; disp = 0.d0
    res  = 1.d0
    do i=1,n
      cent = cent + x(:,i)
    enddo
    if(n>0)cent = cent / dble(n)
    do i=1,n
      v  = x(:,i) - cent
      vv(i) = dot_product(v,v)
      disp = disp + vv(i)
    enddo
!
    if(n>0)then
      disp = dble(n) / disp
    else
      disp = 1.d0
    endif
    sd = sqrt(disp)
    disp = -0.5d0*disp
!
    do i=1,n
      res = res * exp(vv(i)*disp) * sd
    enddo
  end function BIC
end module xmeans_tool
