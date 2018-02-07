include "mkl_vsl.fi"
module spur_matcalc
!$ use omp_lib
implicit none
  private
  public :: cov,idemat,refmat,det,svd
!
  interface cov
    module procedure rcov
  end interface cov
!
  interface idemat
    module procedure ridm
  end interface idemat
!
  interface refmat
    module procedure rrfm
  end interface refmat
!
  interface det
    module procedure ddet3,rdet3
  end interface det
!
  interface svd
    module procedure rsvd,dsvd
  end interface svd
contains
  function rcov(d1,d2,n,X,Y) result(res)
  integer,intent(in) :: d1,d2,n
  real,intent(in)    :: X(d1,n),Y(d2,n)
  real               :: res(d1,d2)
  integer            :: i,j,k
    do k=1,n
      do j=1,d2
        do i=1,d1
          res(i,j) = res(i,j) + X(i,k)*Y(j,k)
        enddo
      enddo
    enddo
  end function rcov
!
  function ridm(d) result(res)
  integer,intent(in) :: d
  real               :: res(d,d)
  integer            :: i
!$omp parallel do private(i)
    do i=1,d
      res(:i-1,i) = 0.0
      res(i,i)    = 1.0
      res(i+1:,i) = 0.0
    enddo
!$omp end parallel do
  end function ridm
!
  function rrfm(d) result(res)
  integer,intent(in) :: d
  real               :: res(d,d)
    res = ridm(d) ; res(d,d) = -1.0
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
  subroutine rsvd(A,N,S,U,VT)
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
  end subroutine rsvd
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
end module spur_matcalc
