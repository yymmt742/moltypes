program rcorr
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
  call compute()
contains
  subroutine compute()
  use spur_optparse
  use moltypes_perser
  use spur_vector_real
  use spur_stdio
  type(optparse)       :: arg
  type(molperser)      :: trj0,trj1
  type(stdio)          :: fio
  real,allocatable     :: vx0(:,:,:),vy0(:,:,:),vz0(:,:,:)
  real,allocatable     :: vx1(:,:,:),vy1(:,:,:),vz1(:,:,:)
  integer              :: i,j
  integer              :: nstep,nmax,nsum,nframe,natm,nres
  integer              :: nend
    call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string.')
    call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
    call arg%add_option("-x",narg=1,metavar='MaskString',help='vector x mask string.')
    call arg%add_option("-y",narg=1,metavar='MaskString',help='vector y mask string.')
    call arg%add_option("-c",narg=1,metavar='MaskString',def=['all'],help='vector c mask string.')
    call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
        &               help='trajectry time steps(ps). default 1.0')
    call arg%add_option("-u",narg=1,metavar='value',def=['-1'],help='upper bound of sumup')
    call arg%add_option("-i",narg=1,metavar='value',def=['0.1'],help='increment factor')
    call arg%add_option("-e",narg=1,metavar='value',def=['-1'],help='end')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
!
    call trj0%fetch(arg%args())
    call trj0%atomselect(mask=arg%optargs('-m',1))
    if(trj0%nfetchatoms()<=0.or.trj0%nfetchframes()<=0) STOP "this trajectory is empty!"
!
    call trj1%fetch(arg%args())
    call trj1%atomselect(mask=arg%optargs('-m',1))
!
    nmax = trj0%nfetchframes()
    nres = trj0%nresidues()
    natm = trj0%nfetchatoms()
!
    nsum   = minval([arg%optargi('-u',1),nmax]) ; if(nsum<1)   nsum = nmax
    nend   = minval([arg%optargi('-e',1),nmax]) ; if(nend<1)   nend = nmax
!
    call fio%fetch(arg%optargs('-o',1))
    if(fio%is()/='') call fio%generate()
!
    i = nmax
    j = 2
!
    allocate(vx0(3,nres,nsum),vy0(3,nres,nsum),vz0(3,nres,nsum))
    allocate(vx1(3,nres,nsum),vy1(3,nres,nsum),vz1(3,nres,nsum))
!
    do while(i>=nsum)
      i = nmax - j + 1
      nstep = i/nsum + 1
!
      call trj0%load(ub=i,stride=nstep)
      call trj1%load(lb=j,stride=nstep)
      nframe = trj0%nframes()
      vx0(:,:,:nframe) = trj0%center_of_residue(arg%optargs('-x',1))
      vy0(:,:,:nframe) = trj0%center_of_residue(arg%optargs('-y',1))
      vz0(:,:,:nframe) = trj0%center_of_residue(arg%optargs('-c',1))
      call set_vxyz(nres,nframe,vx0,vy0,vz0)
      vx1(:,:,:nframe) = trj1%center_of_residue(arg%optargs('-x',1))
      vy1(:,:,:nframe) = trj1%center_of_residue(arg%optargs('-y',1))
      vz1(:,:,:nframe) = trj1%center_of_residue(arg%optargs('-c',1))
      call set_vxyz(nres,nframe,vx1,vy1,vz1)
!
      write(fio%devn(),'(f10.4,3f16.9,a,i0,a)') real(j-1)*arg%optargf('-s',1),&
&                                              ccorr(nres,nframe,vx0(:,:,:nframe),vx1(:,:,:nframe)),&
&                                              ccorr(nres,nframe,vy0(:,:,:nframe),vy1(:,:,:nframe)),&
&                                              ccorr(nres,nframe,vz0(:,:,:nframe),vz1(:,:,:nframe)),&
&                                              ' (',nframe,' frames)'
      j = j + maxval([int(10.0**(int(log10(real(j))))*arg%optargf('-i',1)),1],1)
      if(j>nend) EXIT
    enddo
  end subroutine compute
!
  pure subroutine set_vxyz(nres,nframe,vx,vy,vz)
  integer,intent(in)                :: nres,nframe
  real,intent(inout)                :: vx(3,nres,nframe),vy(3,nres,nframe),vz(3,nres,nframe)
  integer                           :: i,j
    do j=1,nframe
      do i=1,nres
        vx(:,i,j) = vx(:,i,j) - vz(:,i,j)
        vx(:,i,j) = vx(:,i,j) / sqrt(dot_product(vx(:,i,j),vx(:,i,j)))
      enddo
    enddo
    do j=1,nframe
      do i=1,nres
        vy(:,i,j) = vy(:,i,j) - vz(:,i,j)
        vy(:,i,j) = vy(:,i,j) / sqrt(dot_product(vy(:,i,j),vy(:,i,j)))
      enddo
    enddo
    do j=1,nframe
      do i=1,nres
        vz(1,i,j) = vx(2,i,j)*vy(3,i,j)-vx(3,i,j)*vy(2,i,j)
        vz(2,i,j) = vx(3,i,j)*vy(1,i,j)-vx(1,i,j)*vy(3,i,j)
        vz(3,i,j) = vx(1,i,j)*vy(2,i,j)-vx(2,i,j)*vy(1,i,j)
        vz(:,i,j) = vz(:,i,j) / sqrt(dot_product(vz(:,i,j),vz(:,i,j)))
        vy(1,i,j) = vx(2,i,j)*vz(3,i,j)-vx(3,i,j)*vz(2,i,j)
        vy(2,i,j) = vx(3,i,j)*vz(1,i,j)-vx(1,i,j)*vz(3,i,j)
        vy(3,i,j) = vx(1,i,j)*vz(2,i,j)-vx(2,i,j)*vz(1,i,j)
      enddo
    enddo
  end subroutine set_vxyz
!
  pure function ccorr(nres,nframe,v0,v1) result(res)
  integer,intent(in)                :: nres,nframe
  real,intent(in)                   :: v0(3,nres,nframe),v1(3,nres,nframe)
  real                              :: res
  integer                           :: i,j
    res = 0.0
    do j=1,nframe
      do i = 1,nres
        res  = res + dot_product(v0(:,i,j),v1(:,i,j))
      enddo
    enddo
    res = res/real(nres*nframe)
  end function ccorr
end program rcorr
