module spur_histgram
  implicit none
  private 
  public histgram_1d,histgram_2d,histgram_3d
!
! histgram ranges(4)           :: [hmax,hmin,hgrid,hgrid_reverse]
  double precision,parameter   :: hrange_def(4)  = [-10.d0, 10.d0, 1.d0, 1.d0]
  double precision,parameter   :: nrange_max     = 10000
  double precision,parameter   :: lb_def = -10.5d0, ub_def = 10.5d0
  double precision,parameter   :: val_def =  1.d0
!
  type histgram_1d
  private
  double precision             :: hrange(4) = hrange_def
  integer                      :: nrange(3) = [0, 0, 0]
  double precision             :: lb = lb_def
  double precision             :: ub = ub_def
  double precision,allocatable :: Hist(:)
  contains
    procedure :: setup        => H1dSetup
    procedure,private ::         H1dStack
    procedure,private ::         H1dStackArray
    generic   :: stack        => H1dStack,H1dStackArray
    procedure :: Scale        => H1dScale
    procedure :: normal       => H1dNormal
    procedure :: size         => H1dSize
    procedure :: range        => H1dRange
    procedure :: hvalue       => H1dHvalue
    procedure :: export       => H1dExport
    final     :: h1dDestractor
  end type histgram_1d
!
  type histgram_2d
  private
  double precision             :: hrange(2,4) = reshape([hrange_def(1),hrange_def(1),&
                                                        &hrange_def(2),hrange_def(2),&
                                                        &hrange_def(3),hrange_def(3),&
                                                        &hrange_def(4),hrange_def(4)],&
                                                        &[2,4])
  integer                      :: nrange(2,3) = reshape([0, 0, 0, 0, 0, 0],[2,3])
  double precision             :: lb(2) = [lb_def,lb_def]
  double precision             :: ub(2) = [ub_def,ub_def]
  double precision,allocatable :: Hist(:,:)
  contains
    procedure :: setup        => H2dSetup
    procedure,private ::         H2dStack
    procedure,private ::         H2dStackArray
    generic   :: stack        => H2dStack,H2dStackArray
    procedure :: scale        => H2dScale
    procedure :: maxval       => H2dMaxval
    procedure :: maxloc       => H2dMaxloc
    procedure :: export       => H2dExport
    final     :: h2dDestractor
  end type histgram_2d
!
  type histgram_3d
  private
  double precision             :: hrange(3,4) = reshape([hrange_def(1),hrange_def(1),hrange_def(1),&
                                                        &hrange_def(2),hrange_def(2),hrange_def(2),&
                                                        &hrange_def(3),hrange_def(3),hrange_def(3),&
                                                        &hrange_def(4),hrange_def(4),hrange_def(4)],&
                                                        &[3,4])
  integer                      :: nrange(3,3) = reshape([0, 0, 0, 0, 0, 0, 0, 0, 0],[3,3])
  double precision             :: lb(3) = [lb_def,lb_def,lb_def]
  double precision             :: ub(3) = [ub_def,ub_def,ub_def]
  double precision,allocatable :: Hist(:,:,:)
  contains
    procedure :: setup        => H3dSetup
    procedure,private ::         H3dStack
    procedure,private ::         H3dStackArray
    generic   :: stack        => H3dStack,H3dStackArray
    procedure :: scale        => h3dscale
    procedure :: symmetry     => h3dSymmetry
    procedure :: maxval       => H3dMaxval
    procedure :: maxloc       => H3dMaxloc
    procedure :: export       => H3dExport
    procedure :: export_dx    => DXExport
    final     :: h3dDestractor
  end type histgram_3d
!
contains
  pure subroutine H1dSetup(this,hmin,hmax,grid,axis)
  class(histgram_1d),intent(inout)     :: this
  double precision,intent(in),optional :: hmin,hmax,grid
  integer,intent(in),optional          :: axis
    if(present(hmin)) this%hrange(1) = hmin
    if(present(hmax)) this%hrange(2) = hmax
    if(present(grid)) this%hrange(3) = grid
!
    call gridCheck(this%hrange)
    call gridNumRange(this%hrange,this%nrange,this%lb,this%ub)
    if(allocated(this%Hist))deallocate(this%Hist)
    allocate(this%hist(this%nrange(1):this%nrange(2)))
    this%hist = 0.d0
  end subroutine H1dSetup
!
  pure subroutine H2dSetup(this,hmin,hmax,grid,hmin1,hmin2,hmax1,hmax2,grid1,grid2)
  class(histgram_2d),intent(inout)     :: this
  double precision,intent(in),optional :: hmin,hmax,grid
  double precision,intent(in),optional :: hmin1,hmin2
  double precision,intent(in),optional :: hmax1,hmax2
  double precision,intent(in),optional :: grid1,grid2
    if(present(hmin)) this%hrange(:,1)  = hmin
    if(present(hmax)) this%hrange(:,2)  = hmax
    if(present(grid)) this%hrange(:,3)  = grid
    if(present(hmin1)) this%hrange(1,1) = hmin1
    if(present(hmin2)) this%hrange(2,1) = hmin2
    if(present(hmax1)) this%hrange(1,2) = hmax1
    if(present(hmax2)) this%hrange(2,2) = hmax2
    if(present(grid1)) this%hrange(1,3) = grid1
    if(present(grid2)) this%hrange(2,3) = grid2
!
    call gridCheck(this%hrange(1,:))
    call gridNumRange(this%hrange(1,:),this%nrange(1,:),this%lb(1),this%ub(1))
    call gridCheck(this%hrange(2,:))
    call gridNumRange(this%hrange(2,:),this%nrange(2,:),this%lb(2),this%ub(2))
    if(allocated(this%Hist))deallocate(this%Hist)
    allocate(this%hist(this%nrange(1,1):this%nrange(1,2),&
                      &this%nrange(2,1):this%nrange(2,2)))
    this%hist = 0.d0
  end subroutine H2dSetup
!
  pure subroutine H3dSetup(this,hmin,hmax,grid,hmin1,hmin2,hmin3,hmax1,hmax2,hmax3,grid1,grid2,grid3)
  class(histgram_3d),intent(inout)     :: this
  double precision,intent(in),optional :: hmin,hmax,grid
  double precision,intent(in),optional :: hmin1,hmin2,hmin3
  double precision,intent(in),optional :: hmax1,hmax2,hmax3
  double precision,intent(in),optional :: grid1,grid2,grid3
    if(present(hmin)) this%hrange(:,1)  = hmin
    if(present(hmax)) this%hrange(:,2)  = hmax
    if(present(grid)) this%hrange(:,3)  = grid
    if(present(hmin1)) this%hrange(1,1) = hmin1
    if(present(hmin2)) this%hrange(2,1) = hmin2
    if(present(hmin3)) this%hrange(3,1) = hmin3
    if(present(hmax1)) this%hrange(1,2) = hmax1
    if(present(hmax2)) this%hrange(2,2) = hmax2
    if(present(hmax3)) this%hrange(3,2) = hmax3
    if(present(grid1)) this%hrange(1,3) = grid1
    if(present(grid2)) this%hrange(2,3) = grid2
    if(present(grid3)) this%hrange(3,3) = grid3
!
    call gridCheck(this%hrange(1,:))
    call gridNumRange(this%hrange(1,:),this%nrange(1,:),this%lb(1),this%ub(1))
    call gridCheck(this%hrange(2,:))
    call gridNumRange(this%hrange(2,:),this%nrange(2,:),this%lb(2),this%ub(2))
    call gridCheck(this%hrange(3,:))
    call gridNumRange(this%hrange(3,:),this%nrange(3,:),this%lb(3),this%ub(3))
    if(allocated(this%Hist))deallocate(this%Hist)
    allocate(this%hist(this%nrange(1,1):this%nrange(1,2),&
                      &this%nrange(2,1):this%nrange(2,2),&
                      &this%nrange(3,1):this%nrange(3,2)))
    this%hist = 0.d0
  end subroutine H3dSetup
!
  pure subroutine gridCheck(hrange)
  double precision,intent(inout) :: hrange(4)
  double precision               :: tmp(4)
    tmp = hrange ; hrange = hrange_def
    if(tmp(3)<=0.d0)RETURN
    if(anint((tmp(2)-tmp(1))/tmp(3))>nrange_max) RETURN
    hrange = tmp           ; RETURN
  end subroutine gridCheck
!
  pure subroutine gridNumRange(hrange,nrange,lb,ub)
  double precision,intent(inout) :: hrange(4)
  double precision,intent(inout) :: lb,ub
  integer,intent(inout)          :: nrange(3)
    hrange(4) = 1.d0 / hrange(3)
    nrange(1) = anint((hrange(1)) * hrange(4))
    nrange(2) = anint((hrange(2)) * hrange(4))
    nrange(3) = nrange(2) - nrange(1) + 1
    lb = hrange(1) - hrange(3) * 0.5d0
    ub = hrange(2) + hrange(3) * 0.5d0
  end subroutine gridNumRange
!
  pure subroutine H1dStack(this,x,val)
  class(histgram_1d),intent(inout)     :: this
  double precision,intent(in)          :: x
  double precision,intent(in),optional :: val
  double precision                     :: lval
  integer                              :: intx
    if(x<this%lb.or.x>this%ub)RETURN
    if(present(val))then
      lval = val
    else
      lval = val_def
    endif
    intx = idnint(x * this%hrange(4))
    this%Hist(intx) = this%Hist(intx) + lval
  end subroutine H1dStack
!
  pure subroutine H1dStackArray(this,x,val)
  class(histgram_1d),intent(inout)     :: this
  double precision,intent(in)          :: x(:)
  double precision,intent(in),optional :: val
  double precision                     :: lval
  double precision                     :: lb,ub
  integer                              :: i,intx
    if(present(val))then
      lval = val
    else
      lval = val_def
    endif
    do i = lbound(x,1),ubound(x,1)
      if(x(i)<this%lb.or.x(i)>this%ub)CYCLE
      intx = idnint(x(i) * this%hrange(4))
      this%Hist(intx) = this%Hist(intx) + lval
    enddo
  end subroutine H1dStackArray
!
  pure subroutine H2dStack(this,x,val)
  class(histgram_2d),intent(inout)     :: this
  double precision,intent(in)          :: x(2)
  double precision,intent(in),optional :: val
  double precision                     :: lval
  integer                              :: intx(2)
    if(any(x<this%lb).or.any(x>this%ub))RETURN
    if(present(val))then
      lval = val
    else
      lval = val_def
    endif
    intx = idnint(x * this%hrange(:,4))
    this%Hist(intx(1),intx(2)) = this%Hist(intx(1),intx(2)) + lval
  end subroutine H2dStack
!
  pure subroutine H2dStackArray(this,x,val)
  class(histgram_2d),intent(inout)     :: this
  double precision,intent(in)          :: x(:,:)
  double precision,intent(in),optional :: val
  double precision                     :: lval
  integer                              :: i,intx(2),l,u
    if(size(x(:,lbound(x,2)))<2)RETURN
    if(present(val))then
      lval = val
    else
      lval = val_def
    endif
    l = lbound(x,1) ; u = l + 1
    do i = lbound(x,2),ubound(x,2)
      if(any(x(l:u,i)<this%lb).or.any(x(l:u,i)>this%ub))RETURN
      intx = idnint(x(l:u,i) * this%hrange(:,4))
      this%Hist(intx(1),intx(2)) = this%Hist(intx(1),intx(2)) + lval
    enddo
  end subroutine H2dStackArray
!
  pure subroutine H3dStack(this,x,val)
  class(histgram_3d),intent(inout)     :: this
  double precision,intent(in)          :: x(3)
  double precision,intent(in),optional :: val
  double precision                     :: lval
  integer                              :: intx(3)
    if(any(x<this%lb).or.any(x>this%ub))RETURN
    intx = idnint(x * this%hrange(:,4))
    this%Hist(intx(1),intx(2),intx(3)) = this%Hist(intx(1),intx(2),intx(3)) + val
  end subroutine H3dStack
!
  pure subroutine H3dStackArray(this,x,val)
  class(histgram_3d),intent(inout)     :: this
  double precision,intent(in)          :: x(:,:)
  double precision,intent(in),optional :: val
  double precision                     :: lval
  integer                              :: i,intx(3),l,u
    if(size(x(:,lbound(x,2)))<3)RETURN
    if(present(val))then
      lval = val
    else
      lval = val_def
    endif
    l = lbound(x,1) ; u = l + 2
    do i = lbound(x,2),ubound(x,2)
      if(any(x(l:u,i)<this%lb).or.any(x(l:u,i)>this%ub))RETURN
      intx = idnint(x(l:u,i) * this%hrange(:,4))
      this%Hist(intx(1),intx(2),intx(3)) = this%Hist(intx(1),intx(2),intx(3)) + lval
    enddo
  end subroutine H3dStackArray
!
  pure subroutine H1dScale(this,factor,expornent)
  class(histgram_1d),intent(inout)     :: this
  double precision,intent(in),optional :: factor
  double precision,intent(in),optional :: expornent
  double precision                     :: f(0:maxval([this%nrange(1:2)],1))
  integer                              :: i
    if(.not.allocated(this%Hist))RETURN
    if(present(factor))then
      f(0) = factor
    else
      f(0) = maxval(this%hist)
      if(f(0)<1.0E-15) f(0)=1.d0
      f(0) = 1.d0 / f(0)
    endif
    if(present(expornent))then
      f(0) = f(0) * this%hrange(3) ** expornent
      do i = 1,size(f)
        f(i) = f(0) * dble(i)**expornent
      enddo
      do i = this%nrange(1),this%nrange(2)
        this%hist(i) = this%hist(i) * f(abs(i))
      enddo
    else
      this%hist = this%hist * f(0)
    endif
  end subroutine H1dScale
!
  pure subroutine H2dScale(this,factor,expornent)
  class(histgram_2d),intent(inout)     :: this
  double precision,intent(in),optional :: factor
  double precision,intent(in),optional :: expornent
  double precision                     :: f(0:maxval([this%nrange(1,1:2)],1),&
                                           &0:maxval([this%nrange(2,1:2)],1))
  integer                              :: i,j
    if(.not.allocated(this%Hist))RETURN
    if(present(factor))then
      f(0,0) = factor
    else
      f(0,0) = maxval(this%hist)
      if(f(0,0)<1.0E-15) f(0,0)=1.d0
      f(0,0) = 1.d0 / f(0,0)
    endif
    if(present(expornent))then
      do j = 1,size(f(0,:))
        do i = 1,size(f(:,0))
          f(i,j) = f(0,0) * (dble(i)**expornent+dble(j)**expornent)
        enddo
      enddo
      do j = this%nrange(2,1),this%nrange(2,2)
        do i = this%nrange(1,1),this%nrange(1,2)
          this%hist(i,j) = this%hist(i,j) * f(abs(i),abs(j))
        enddo
      enddo
    else
      this%hist = this%hist * f(0,0)
    endif
  end subroutine H2dScale
!
  pure subroutine H3dScale(this,factor,expornent)
  class(histgram_3d),intent(inout)     :: this
  double precision,intent(in),optional :: factor
  double precision,intent(in),optional :: expornent
  double precision                     :: f(0:maxval([this%nrange(1,1:2)],1),&
                                           &0:maxval([this%nrange(2,1:2)],1),&
                                           &0:maxval([this%nrange(3,1:2)],1))
  integer                              :: i,j,k
    if(.not.allocated(this%Hist))RETURN
    if(present(factor))then
      f(0,0,0) = factor
    else
      f(0,0,0) = maxval(this%hist)
      if(f(0,0,0)<1.0E-15) f(0,0,0)=1.d0
      f(0,0,0) = 1.d0 / f(0,0,0)
    endif
    if(present(expornent))then
      do k = 1,size(f(0,0,:))
        do j = 1,size(f(0,:,0))
          do i = 1,size(f(:,0,0))
            f(i,j,k) = f(0,0,0) * (dble(i)**expornent+dble(j)**expornent+dble(k)**expornent)
          enddo
        enddo
      enddo
      do k = this%nrange(3,1),this%nrange(3,2)
        do j = this%nrange(2,1),this%nrange(2,2)
          do i = this%nrange(1,1),this%nrange(1,2)
            this%hist(i,j,k) = this%hist(i,j,k) * f(abs(i),abs(j),abs(k))
          enddo
        enddo
      enddo
    else
      this%hist = this%hist * f(0,0,0)
    endif
  end subroutine H3dScale
!
  pure subroutine H1dNormal(this)
  class(histgram_1d),intent(inout) :: this
  double precision                 :: sumup
    if(.not.allocated(this%Hist))RETURN
    sumup = sum(this%Hist)
    if(sumup<1.0E-9)RETURN
    this%Hist = this%Hist / sumup
  end subroutine H1dNormal
!
  subroutine H3dSymmetry(this,Symmetry,origin)
!$ use omp_lib
  use spur_string_neaten
  class(histgram_3d),intent(inout)     :: this
  character(*),intent(in)              :: Symmetry
  double precision,intent(in),optional :: origin(3)
  integer                              :: lb(3),ub(3)
  integer                              :: i,j,k
  integer                              :: im,jm,km
    if(.not.allocated(this%Hist))RETURN
    if(present(origin))then
      lb = anint(origin*this%hrange(:,4))
    else
      lb = 0
    endif
!
    do i=1,3
      ub(i) = minval([lb(i)-this%nrange(i,1),this%nrange(i,2)-lb(i)],1) + lb(i)
      if(ub(i)<lb(i)) lb(i) = ub(i)
    enddo
!
    select case(small(Symmetry))
    case('cs','csxy')
      call sigma2xy()
    case('csyz')
      call sigma2yz()
    case('cszx','csxz')
      call sigma2zx()
    case('c2x')
      call C2xs()
    case('c2y')
      call C2ys()
    case("c2",'c2z')
      call C2zs()
    case('c2hx')
      call C2xs() ; call sigma2yz()
    case('c2hy')
      call C2ys() ; call sigma2zx()
    case('c2h','c2hz')
      call C2zs() ; call sigma2xy()
    case('c2vx')
      call C2xs() ; call sigma2xy() ; call sigma2zx()
    case('c2vy')
      call C2ys() ; call sigma2xy() ; call sigma2yz()
    case("c2v",'c2vz')
      call C2zs() ; call sigma2yz() ; call sigma2zx()
    case("d2")
      call c2xs() ; call c2ys() ; call c2zs()
    case("d2v")
      call c2xs() ; call c2ys() ; call c2zs()
      call sigma2xy() ; call sigma2yz() ; call sigma2zx()
    end select
  contains
    subroutine c2xs
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(i ,jm,km))
            this%hist(i ,jm,km) = this%hist(i ,j ,k )
            this%hist(i ,jm,k ) = 0.5d0 * (this%hist(i ,jm,k ) + this%hist(i ,j ,km))
            this%hist(i ,j ,km) = this%hist(i ,jm,k )
            this%hist(im,j ,k ) = 0.5d0 * (this%hist(im,j ,k ) + this%hist(im,jm,km))
            this%hist(im,jm,km) = this%hist(im,j ,k )
            this%hist(im,jm,k ) = 0.5d0 * (this%hist(im,jm,k ) + this%hist(im,j ,km))
            this%hist(im,j ,km) = this%hist(im,jm,k )
          enddo
        enddo
      enddo
    end subroutine c2xs
!
    subroutine c2ys
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(im,j ,km))
            this%hist(im,j ,km) = this%hist(i ,j ,k )
            this%hist(im,j ,k ) = 0.5d0 * (this%hist(im,j ,k ) + this%hist(i ,j ,km))
            this%hist(i ,j ,km) = this%hist(im,j ,k )
            this%hist(i ,jm,k ) = 0.5d0 * (this%hist(i ,jm,k ) + this%hist(im,jm,km))
            this%hist(im,jm,km) = this%hist(i ,jm,k )
            this%hist(im,jm,k ) = 0.5d0 * (this%hist(im,jm,k ) + this%hist(i ,jm,km))
            this%hist(i ,jm,km) = this%hist(im,jm,k )
          enddo
        enddo
      enddo
    end subroutine c2ys
!
    subroutine c2zs
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(im,jm,k ))
            this%hist(im,jm,k ) = this%hist(i ,j ,k )
            this%hist(im,j ,k ) = 0.5d0 * (this%hist(im,j ,k ) + this%hist(i ,jm,k ))
            this%hist(i ,jm,k ) = this%hist(im,j ,k )
            this%hist(i ,j ,km) = 0.5d0 * (this%hist(i ,j ,km) + this%hist(im,jm,km))
            this%hist(im,jm,km) = this%hist(i ,j ,km)
            this%hist(i ,jm,km) = 0.5d0 * (this%hist(i ,jm,km) + this%hist(im,j ,km))
            this%hist(im,j ,km) = this%hist(i ,jm,km)
          enddo
        enddo
      enddo
    end subroutine c2zs
!
    subroutine sigma2xy
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(i ,j ,km))
            this%hist(i ,j ,km) = this%hist(i ,j ,k )
            this%hist(im,j ,k ) = 0.5d0 * (this%hist(im,j ,k ) + this%hist(im,j ,km))
            this%hist(im,j ,km) = this%hist(im,j ,k )
            this%hist(i ,jm,km) = 0.5d0 * (this%hist(i ,jm,km) + this%hist(i ,jm,k ))
            this%hist(i ,jm,k ) = this%hist(i ,jm,km)
            this%hist(im,jm,km) = 0.5d0 * (this%hist(im,jm,km) + this%hist(im,jm,k ))
            this%hist(im,jm,k ) = this%hist(im,jm,km)
          enddo
        enddo
      enddo
    end subroutine sigma2xy
!
    subroutine sigma2yz
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(im,j ,k ))
            this%hist(im,j ,k ) = this%hist(i ,j ,k )
            this%hist(i ,jm,k ) = 0.5d0 * (this%hist(i ,jm,k ) + this%hist(im,jm,k ))
            this%hist(im,jm,k ) = this%hist(i ,jm,k )
            this%hist(i ,j ,km) = 0.5d0 * (this%hist(i ,j ,km) + this%hist(im,j ,km))
            this%hist(im,j ,km) = this%hist(i ,j ,km)
            this%hist(i ,jm,km) = 0.5d0 * (this%hist(i ,jm,km) + this%hist(im,jm,km))
            this%hist(im,jm,km) = this%hist(i ,jm,km)
          enddo
        enddo
      enddo
    end subroutine sigma2yz
!
    subroutine sigma2zx
      km = lb(3) + 1
      do k=lb(3),ub(3)
        km = km - 1 ; jm = lb(2) + 1
        do j=lb(2),ub(2)
          jm = jm - 1 ; im = lb(1) + 1
          do i=lb(1),ub(1)
            im = im - 1
            this%hist(i ,j ,k ) = 0.5d0 * (this%hist(i ,j ,k ) + this%hist(i ,jm,k ))
            this%hist(i ,jm,k ) = this%hist(i ,j ,k )
            this%hist(im,j ,k ) = 0.5d0 * (this%hist(im,j ,k ) + this%hist(im,jm,k ))
            this%hist(im,jm,k ) = this%hist(im,j ,k )
            this%hist(i ,j ,km) = 0.5d0 * (this%hist(i ,j ,km) + this%hist(i ,jm,km))
            this%hist(i ,jm,km) = this%hist(i ,j ,km)
            this%hist(im,j ,km) = 0.5d0 * (this%hist(im,j ,km) + this%hist(im,jm,km))
            this%hist(im,jm,km) = this%hist(im,j ,km)
          enddo
        enddo
      enddo
    end subroutine sigma2zx
  end subroutine H3dSymmetry
!
  pure function H1dMaxval(this,idx) result(res)
  class(histgram_1d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res
  integer                          :: i
    res = 0.d0
    if(.not.allocated(this%Hist))RETURN
    res = maxval(this%Hist)
    if(present(idx))then
      do i=1,idx-1
        res = maxval(this%Hist,mask=this%hist<res)
      enddo
    endif
  end function H1dMaxval
!
  pure function H2dMaxval(this,idx) result(res)
  class(histgram_2d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res
  integer                          :: i
    res = 0.d0
    if(.not.allocated(this%Hist))RETURN
    res = maxval(this%Hist)
    if(present(idx))then
      do i=1,idx-1
        res = maxval(this%Hist,mask=this%hist<res)
      enddo
    endif
  end function H2dMaxval
!
  pure function H3dMaxval(this,idx) result(res)
  class(histgram_3d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res
  integer                          :: i
    res = 0.d0
    if(.not.allocated(this%Hist))RETURN
    res = maxval(this%Hist)
    if(present(idx))then
      do i=1,idx-1
        res = maxval(this%Hist,mask=this%hist<res)
      enddo
    endif
  end function H3dMaxval
!
  pure function H1dMaxloc(this,idx) result(res)
  class(histgram_1d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res
  logical,allocatable              :: mask(:)
  integer                          :: i,mloc,ofs
    res = 0.d0
    if(.not.allocated(this%Hist))RETURN
    ofs  = lbound(this%Hist,1)-1
    mloc = maxloc(this%Hist,1) + ofs
!
    if(present(idx))then
      allocate(mask(lbound(this%Hist,1):ubound(this%hist,1)))
      mask = .TRUE.
      do i=1,idx-1
        mask(mloc) = .FALSE.
        mloc = maxloc(this%Hist,1,mask=mask) + ofs
      enddo
      deallocate(mask)
    endif
    res = dble(mloc) * this%hrange(3)
  end function H1dMaxloc
!
  pure function H2dMaxloc(this,idx) result(res)
  class(histgram_2d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res(2)
  logical,allocatable              :: mask(:,:)
  integer                          :: i,mloc(2),ofs(2)
    res = [0.d0,0.d0]
    if(.not.allocated(this%Hist))RETURN
    ofs  = [lbound(this%Hist,1)-1,lbound(this%Hist,2)-1]
    mloc = maxloc(this%Hist) + ofs
!
    if(present(idx))then
      allocate(mask(lbound(this%Hist,1):ubound(this%hist,1),lbound(this%hist,2):ubound(this%hist,2)))
      mask = .TRUE.
      do i=1,idx-1
        mask(mloc(1),mloc(2)) = .FALSE.
        mloc = maxloc(this%Hist,mask=mask) + ofs
      enddo
      deallocate(mask)
    endif
    res = dble(mloc) * this%hrange(:,3)
  end function H2dMaxloc
!
  pure function H3dMaxloc(this,idx) result(res)
  class(histgram_3d),intent(in)    :: this
  integer,intent(in),optional      :: idx
  double precision                 :: res(3)
  logical,allocatable              :: mask(:,:,:)
  integer                          :: i,mloc(3),ofs(3)
    res = [0.d0,0.d0,0.d0]
    if(.not.allocated(this%Hist))RETURN
    ofs  = [lbound(this%Hist,1)-1,lbound(this%Hist,2)-1,lbound(this%Hist,3)-1]
    mloc = maxloc(this%Hist) + ofs
!
    if(present(idx))then
      allocate(mask(lbound(this%Hist,1):ubound(this%hist,1),&
                   &lbound(this%hist,2):ubound(this%hist,2),&
                   &lbound(this%hist,3):ubound(this%hist,3)))
      mask = .TRUE.
      do i=1,idx-1
        mask(mloc(1),mloc(2),mloc(3)) = .FALSE.
        mloc = maxloc(this%Hist,mask=mask) + ofs
      enddo
      deallocate(mask)
    endif
    res = dble(mloc) * this%hrange(:,3)
  end function H3dMaxloc
!
  pure function H1dSize(this) result(res)
  class(histgram_1d),intent(in)    :: this
  integer                          :: res
  integer                          :: i
    res = this%nrange(3)
  end function H1dSize
!
  pure function H1dRange(this) result(res)
  class(histgram_1d),intent(in)    :: this
  double precision                 :: res(this%nrange(3))
  integer                          :: i
    res = [(dble(i)*this%hrange(3),i=this%nrange(1),this%nrange(2))]
  end function H1dRange
!
  pure function H1dHvalue(this) result(res)
  class(histgram_1d),intent(in)    :: this
  double precision                 :: res(this%nrange(3))
  integer                          :: i
    res = [(this%hist(i),i=this%nrange(1),this%nrange(2))]
  end function H1dHvalue
!
  subroutine H1dExport(this,path,title)
  use spur_stdio
  class(histgram_1d),intent(inout) :: this
  character(*),intent(in)          :: path
  character(*),intent(in),optional :: title
  type(stdio)                      :: fdata
  integer                          :: i
    if(.not.allocated(this%Hist))RETURN
    call fdata%fetch(path) ; call fdata%generate()
    if(present(title)) call fdata%puts("#"//title)
!
    do i = this%nrange(1),this%nrange(2)
      write(fdata%DevN(),'(f9.3,f16.9)') dble(i)*this%hrange(3),this%hist(i)
    enddo
    call fdata%quit()
  end subroutine H1dExport
!
  subroutine H2dExport(this,path,title)
  use spur_stdio
  class(histgram_2d),intent(inout) :: this
  character(*),intent(in)          :: path
  character(*),intent(in),optional :: title
  type(stdio)                      :: fdata
  integer                          :: i,j
    if(.not.allocated(this%Hist))RETURN
    call fdata%fetch(path) ; call fdata%generate()
!
    if(present(title)) call fdata%puts("#"//title)
    do j=this%nrange(2,1),this%nrange(2,2)
      do i=this%nrange(1,1),this%nrange(1,2)
        write(fdata%devn(),'(2f9.3,f16.9)')dble(j)*this%hrange(2,3),dble(i)*this%hrange(1,3),this%Hist( i, j )
      enddo
      call fdata%break()
    enddo
    call fdata%quit()
  end subroutine H2dExport
!
  subroutine H3dExport(this,path,title,threshold)
  use spur_stdio
  class(histgram_3d),intent(in)        :: this
  character(*),intent(in)              :: path
  character(*),intent(in),optional     :: title
  double precision,intent(in),optional :: threshold
  type(stdio)                          :: fdata
  double precision                     :: t
  integer                              :: i,j,k
  logical                              :: new
    if(.not.allocated(this%Hist))RETURN
    call fdata%fetch(path) ; call fdata%generate()
    if(present(title)) call fdata%puts("#"//title)
    t = 0.d0
    if(present(threshold)) t = maxval([threshold,t],1)
!
    do i=this%nrange(1,1),this%nrange(1,2)
      new = .FALSE.
      do j=this%nrange(2,1),this%nrange(2,2)
        do k=this%nrange(3,1),this%nrange(3,2)
          if(abs(this%Hist( i, j, k ))<t)CYCLE
          write(fdata%devn(),'(3f9.3,f16.9)') dble(k)*this%hrange(3,3),dble(j)*this%hrange(2,3),&
                                            & dble(i)*this%hrange(1,3),this%Hist( i, j, k )
          new = .TRUE.
        enddo
      enddo
      if(new) call fdata%break()
    enddo
  end subroutine H3dExport
!
  subroutine DXexport(this,path,title,cutoff,cutval)
  use spur_stdio
  class(histgram_3d),intent(in)        :: this
  character(*),intent(in)              :: path
  character(*),intent(in),optional     :: title
  double precision,intent(in),optional :: cutoff,cutval
  type(stdio)                          :: fdata
  double precision                     :: coff,cval,cpnt(3)
  integer                              :: i,j,k
    if(.not.allocated(this%Hist))RETURN
    coff = huge(0.d0) ; cval = 0.d0
    if(present(cutoff)) coff = cutoff * cutoff
    if(present(cutval)) cval = cutval
!
    call fdata%fetch(path) ; call fdata%generate()
    if(present(title)) call fdata%puts("#"//title)
    write(fdata%devn(),'(a,i0,x,i0,x,i0)')"object 1 class gridpositions counts ", this%nrange(:,3)
    write(fdata%devn(),'(a,3f16.3)')"origin", this%hrange(:,1)
    write(fdata%devn(),'(a,f16.3,a)')"delta            0.000           0.000",this%hrange(3,3)
    write(fdata%devn(),'(a,f16.3,a)')"delta            0.000",this%hrange(2,3),"           0.000"
    write(fdata%devn(),'(a,f16.3,a)')"delta ",this%hrange(1,3),"           0.000           0.000"
    write(fdata%devn(),'(a,i0,x,i0,x,i0)')"object 2 class gridconections counts ",this%nrange(:,3)
    write(fdata%devn(),'(a,i0,a)')"object 3 class array type double rank 0 items ",&
                        &this%nrange(1,3)*this%nrange(2,3)*this%nrange(3,3)," data follows"
    cpnt = this%hrange(:,1)
    do k=this%nrange(3,1),this%nrange(3,2)
      cpnt(3) = k * this%hrange(3,3)
      do j=this%nrange(2,1),this%nrange(2,2)
        cpnt(2) = j * this%hrange(2,3)
        do i=this%nrange(1,1),this%nrange(1,2)
          cpnt(1) = i * this%hrange(1,3)
          if(dot_product(cpnt,cpnt)>coff)then
            write(fdata%devn(),'(f16.9)')cval
          else
            write(fdata%devn(),'(f16.9)')this%Hist( i, j, k )
          endif
        enddo
      enddo
    enddo
  end subroutine DXExport
!
  pure subroutine H1dDestractor(this)
  type(histgram_1d),intent(inout) :: this
    if(allocated(this%Hist))deallocate(this%Hist)
    this%hrange = hrange_def
    this%nrange = [0,0,0]
  end subroutine H1dDestractor
!
  pure subroutine H2dDestractor(this)
  type(histgram_2d),intent(inout) :: this
    if(allocated(this%Hist))deallocate(this%Hist)
    this%hrange = reshape([hrange_def(1),hrange_def(1),&
                          &hrange_def(2),hrange_def(2),&
                          &hrange_def(3),hrange_def(3),&
                          &hrange_def(4),hrange_def(4)],&
                          &[2,4])
    this%nrange = reshape([0,0,0,0,0,0],[2,3])
  end subroutine H2dDestractor
!
  pure subroutine H3dDestractor(this)
  type(histgram_3d),intent(inout) :: this
    if(allocated(this%Hist))deallocate(this%Hist)
    this%hrange = reshape([hrange_def(1),hrange_def(1),hrange_def(1),&
                          &hrange_def(2),hrange_def(2),hrange_def(2),&
                          &hrange_def(3),hrange_def(3),hrange_def(3),&
                          &hrange_def(4),hrange_def(4),hrange_def(4)],&
                          &[3,4])
    this%nrange = reshape([0,0,0,0,0,0,0,0,0],[3,3])
  end subroutine H3dDestractor
end module spur_histgram
