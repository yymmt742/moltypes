include "mkl_vsl.fi"
module spur_matcalc
!$ use omp_lib
implicit none
  private
  public :: cov,idemat,refmat,det,inverse,svd,sye
!
  interface cov
    module procedure rcov,dcov,rcovc,dcovc
  end interface cov
!
  interface idemat
    module procedure ridm,didm
  end interface idemat
!
  interface refmat
    module procedure rrfm
  end interface refmat
!
  interface det
    module procedure  ddet3,rdet3
  end interface det
!
  interface inverse
    module procedure  SINVERSE,DINVERSE
  end interface inverse
!
  interface svd
    module procedure ssvd,dsvd
  end interface svd
!
  interface sye
    module procedure ssye,dsye
  end interface sye
contains
  pure function rcov(d1,d2,n,X,Y) result(res)
  integer,intent(in) :: d1,d2,n
  real,intent(in)    :: X(d1,n),Y(d2,n)
  real               :: res(d1,d2)
  integer            :: i,j,k
    res = 0.0
    do k=1,n
      do j=1,d2
        do i=1,d1
          res(i,j) = res(i,j) + X(i,k)*Y(j,k)
        enddo
      enddo
    enddo
  end function rcov
!
  pure function rcovc(d1,d2,n,X,Y,XC,YC) result(res)
  integer,intent(in) :: d1,d2,n
  real,intent(in)    :: X(d1,n),Y(d2,n)
  real,intent(in)    :: XC(d1),YC(d2)
  real               :: res(d1,d2)
  integer            :: i,j,k
    res = 0.0
    do k=1,n
      do j=1,d2
        do i=1,d1
          res(i,j) = res(i,j) + (X(i,k)-XC(i))*(Y(j,k)-YC(j))
        enddo
      enddo
    enddo
  end function rcovc
!
  pure function dcov(d1,d2,n,X,Y) result(res)
  integer,intent(in)          :: d1,d2,n
  double precision,intent(in) :: X(d1,n),Y(d2,n)
  double precision            :: res(d1,d2)
  integer                     :: i,j,k
    res = 0.d0
    do k=1,n
      do j=1,d2
        do i=1,d1
          res(i,j) = res(i,j) + X(i,k)*Y(j,k)
        enddo
      enddo
    enddo
  end function dcov
!
  pure function dcovc(d1,d2,n,X,Y,XC,YC) result(res)
  integer,intent(in)          :: d1,d2,n
  double precision,intent(in) :: X(d1,n),Y(d2,n)
  double precision,intent(in) :: XC(d1),YC(d2)
  double precision            :: res(d1,d2)
  integer                     :: i,j,k
    res = 0.d0
    do k=1,n
      do j=1,d2
        do i=1,d1
          res(i,j) = res(i,j) + (X(i,k)-XC(i))*(Y(j,k)-YC(j))
        enddo
      enddo
    enddo
  end function dcovc
!
  pure function ridm(d,lambda) result(res)
  integer,intent(in) :: d
  real,intent(in)    :: lambda
  real               :: res(d,d)
  integer            :: i
    do i=1,d
      res(:i-1,i) = 0.0
      res(i,i)    = lambda
      res(i+1:,i) = 0.0
    enddo
  end function ridm
!
  pure function didm(d,lambda) result(res)
  integer,intent(in)          :: d
  double precision,intent(in) :: lambda
  double precision            :: res(d,d)
  integer                     :: i
    do i=1,d
      res(:i-1,i) = 0.d0
      res(i,i)    = lambda
      res(i+1:,i) = 0.d0
    enddo
  end function didm
!
  function rrfm(d) result(res)
  integer,intent(in) :: d
  real               :: res(d,d)
    res = ridm(d,1.0) ; res(d,d) = -1.0
  end function rrfm
!
  pure function rdet3(X) result(res)
  real,intent(in) :: X(3,3)
  real            :: res
    res = X(1,1)*X(2,2)*X(3,3)+X(1,2)*X(2,3)*X(3,1)+X(1,3)*X(2,1)*X(3,2)&
        &-X(1,3)*X(2,2)*X(3,1)-X(1,2)*X(2,1)*X(3,3)-X(1,1)*X(2,3)*X(3,2)
  end function rdet3
!
  pure function ddet3(X) result(res)
  double precision,intent(in) :: X(3,3)
  double precision            :: res
    res = X(1,1)*X(2,2)*X(3,3)+X(1,2)*X(2,3)*X(3,1)+X(1,3)*X(2,1)*X(3,2)&
        &-X(1,3)*X(2,2)*X(3,1)-X(1,2)*X(2,1)*X(3,3)-X(1,1)*X(2,3)*X(3,2)
  end function ddet3
!
  function SINVERSE(A) result(res)
  real,intent(in)     :: A(:,:)
  real                :: res(size(A,1),size(A,1))
  real                :: WorkSpace(64*size(A,1))
  integer             :: ipiv(size(A,1))
  integer             :: info,lwork,n,lda
    n = size(A,1) ; lda = n
    lwork = 64*n
    res = A(1:n,1:n)
    CALL SGETRF(n,n,res,lda,ipiv,info)
    CALL SGETRI(n,res,lda,ipiv,WorkSpace,LWork,Info)
  end function SINVERSE
!
  function DINVERSE(A) result(res)
  double precision,intent(in)  :: A(:,:)
  double precision             :: res(size(A,1),size(A,1))
  double precision             :: WorkSpace(64*size(A,1))
  integer                      :: ipiv(size(A,1))
  integer                      :: info,lwork,n,lda
    n = size(A,1) ; lda = n
    lwork = 64*n
    res = A(1:n,1:n)
    CALL DGETRF(n,n,res,lda,ipiv,info)
    CALL DGETRI(n,res,lda,ipiv,WorkSpace,LWork,Info)
  end function DINVERSE
!
  subroutine ssvd(A,N,S,U,VT)
  integer,intent(in) :: N
  real,intent(inout) :: A(N,N)
  real,intent(out)   :: S(N),U(N,N),VT(N,N)
  real,allocatable   :: WorkSpace(:)
  integer            :: info,lwork
    LWork=-1 ; allocate(WorkSpace(lWork)) ; WorkSpace=0
    CALL SGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL SGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
  end subroutine ssvd
!
  subroutine dsvd(a,n,s,u,vt)
  integer,intent(in)             :: N
  double precision,intent(inout) :: A(N,N)
  double precision,intent(out)   :: S(N),U(N,N),VT(N,N)
  double precision,allocatable   :: WorkSpace(:)
  integer                        :: info,lwork
    LWork=-1 ; allocate(WorkSpace(lWork)) ; WorkSpace=0
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
  end subroutine dsvd
!
  subroutine SSYE(A,EV,N)
  real,intent(inout)             :: A(N,N)
  real,intent(out)               :: EV(N)
  integer,intent(in)             :: N
  real,allocatable               :: WorkSpace(:)
  integer                        :: lda,lwork,liwork
  integer                        :: info
    LWork=-1
    allocate(WorkSpace(1))
    WorkSpace=0
    CALL SSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
!
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL SSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
  end subroutine SSYE
!
  subroutine DSYE(A,EV,N)
  double precision,intent(inout) :: A(N,N)
  double precision,intent(out)   :: EV(N)
  integer,intent(in)             :: N
  double precision,allocatable   :: WorkSpace(:)
  integer                        :: lda,lwork,liwork
  integer                        :: info
    LWork=-1
    allocate(WorkSpace(1))
    WorkSpace=0
    CALL DSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
!
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL DSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
  end subroutine DSYE
end module spur_matcalc
