program ofs
  use molio ; use vfunc
  implicit none
  character(5),allocatable :: Aname(:)
  double precision,allocatable :: xyz(:,:,:)
  double precision,allocatable::xyz_ofs1(:,:),xyz_ofs2(:,:),xyz_ofs3(:,:)
  integer :: Natm,Ntrj
!
  character(132),allocatable :: arg(:)
  integer :: i,j,io,Narg
    Narg=iargc()
    if(Narg==0)call usage
!
    allocate(arg(Narg))
    do i=1,Narg
        call getarg(i,arg(i))
    enddo
!
    call molio_load(arg(1),FILE=arg(1))
!
    call molio_pack(arg(1),NATM=NATM,NTRJ=NTRJ,ATMNAM=aname,CRD=xyz)
    if(Narg>1)then
        call molio_pack(arg(1),COMALL=xyz_ofs1,MASK=arg(2))
    else
        allocate(xyz_ofs1(3,NTRJ)) ; xyz_ofs1=0.d0
    endif
    if(Narg>2)then
        call molio_pack(arg(1),COMALL=xyz_ofs2,MASK=arg(3))
    else
        allocate(xyz_ofs2(3,NTRJ)) ; xyz_ofs2=0.d0
    endif
    if(Narg>3)then
        call molio_pack(arg(1),COMALL=xyz_ofs3,MASK=arg(4))
    else
        allocate(xyz_ofs3(3,NTRJ)) ; xyz_ofs3=0.d0
    endif
!
    call ofset
!
    deallocate(xyz_ofs1,xyz_ofs2,xyz_ofs3)
!
    do j=1,NTRJ
        print'(i0,/,i0)',Natm,j
        do i=1,Natm
            print'(a5,3f12.3)',Aname(i),xyz(:,i,j)
        enddo
    enddo
    STOP
contains
  subroutine ofset
  double precision :: ofs2_norm,ofs3_norm
  double precision :: sin_o3,cos_o3,xyz_t(1:3)
  double precision :: ofs2_ang,ofs2_crs(1:3)
  double precision :: x_norm(1:3), y_norm(1:3), z_norm(1:3)
    if(Narg==1)RETURN
!
    x_norm=(/1.d0,0.d0,0.d0/)
    y_norm=(/0.d0,1.d0,0.d0/)
    z_norm=(/0.d0,0.d0,1.d0/)
!
    do i=1,NTRJ
        do j=1,Natm
            xyz(:,j,i)=xyz(:,j,i)-xyz_ofs1(:,i)
        enddo
        xyz_ofs2(:,i)=xyz_ofs2(:,i)-xyz_ofs1(:,i)
        xyz_ofs3(:,i)=xyz_ofs3(:,i)-xyz_ofs1(:,i)
    enddo
!
    if(Narg==2)RETURN
!
    do i=1,NTRJ
        ofs2_norm=norm(xyz_ofs2(:,i))
        if(ofs2_norm>1.0E-10)then
            xyz_ofs2(:,i)=xyz_ofs2(:,i)/ofs2_norm
            ofs2_ang=VecAng(xyz_ofs2(:,i),x_norm)
            ofs2_crs=Cross(xyz_ofs2(:,i),x_norm)
            if(ofs2_ang<1.0E-5)CYCLE
!
            call Rotation_Set(RotAng=ofs2_ang,RotN=ofs2_crs)
!
            do j=1,Natm
                xyz(:,j,i)=Rotation(V=xyz(:,j,i))
            enddo
!
            xyz_ofs3(:,i)=Rotation(V=xyz_ofs3(:,i))
        endif
    enddo
!
    if(Narg==3)RETURN
!
    do i=1,NTRJ
        ofs3_norm=sqrt(xyz_ofs3(2,i)**2.d0+xyz_ofs3(3,i)**2.d0)
!
        if(ofs3_norm>1.0E-10)then
            sin_o3=xyz_ofs3(3,i)/ofs3_norm
            cos_o3=xyz_ofs3(2,i)/ofs3_norm
!
            do j=1,Natm
                xyz_t(1)=xyz(1,j,i)
                xyz_t(2)=xyz(2,j,i)*cos_o3+xyz(3,j,i)*sin_o3
                xyz_t(3)=-xyz(2,j,i)*sin_o3+xyz(3,j,i)*cos_o3
                xyz(:,j,i)=xyz_t
            enddo
        endif
    enddo
!
    RETURN
  end subroutine ofset

  function rmsrot(natm,c1,c0) result(res)
  integer,intent(in)             :: natm
  double precision,intent(in)    :: c1(3,natm),c0(3,natm)
  double precision               :: res(3,3)
  double precision               :: cov(3,3),s(3),u(3,3),vt(3,3),d(3,3),det
    cov = covariance(3,natm,c1,c0)
    call DSVD(cov,3,s,u,vt)
    cov = matmul(u,vt)
    det = dnint(det3d(cov))
    res = matmul(u,reshape([1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,det],[3,3]))
    res = matmul(res,vt)
    res = transpose(res)
  end function rmsrot
!
  pure function covariance(d,n,X,Y) result(res)
  integer,intent(in)          :: d,n
  double precision,intent(in) :: X(d,n),Y(d,n)
  double precision            :: res(d,d)
  integer                     :: i,j,k
    res = 0.d0
    do k=1,n
      do j=1,d
        do i=1,d
          res(i,j) = res(i,j) + X(i,k)*Y(j,k)
        enddo
      enddo
    enddo
  end function covariance
!
  pure function det3d(X) result(res)
  double precision,intent(in) :: X(3,3)
  double precision            :: res
    res = X(1,1)*X(2,2)*X(3,3)+X(1,2)*X(2,3)*X(3,1)+X(1,3)*X(2,1)*X(3,2)&
        &-X(1,3)*X(2,2)*X(3,1)-X(1,2)*X(2,1)*X(3,3)-X(1,1)*X(2,3)*X(3,2)
  end function det3d
!
  subroutine DSVD(A,N,S,U,VT)
  integer,intent(in)             :: N
  double precision,intent(inout) :: A(N,N)
  double precision,intent(out)   :: S(N),U(N,N),VT(N,N)
  double precision,allocatable   :: WorkSpace(:)
  integer                        :: lwork,liwork
  integer                        :: info
    LWork=-1
    allocate(WorkSpace(lWork))
    WorkSpace=0
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
  end subroutine DSVD
!
  subroutine usage
    write(6,'(a)')"usage :: ofs <filename> [mask1] [mask2] [mask3]"
    STOP
  end subroutine usage
end program ofs
