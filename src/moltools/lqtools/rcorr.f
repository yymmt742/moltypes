program rcorr
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
  call compute()
contains
  subroutine compute()
  use spur_optparse
  use moltypes
  use moltypes_process
  use spur_vector_real
  use spur_stdio
  type(optparse)       :: arg
  type(moltype)        :: trj0,trj1
  type(stdio)          :: fio
  real,allocatable     :: v0(:,:,:),v1(:,:,:)
  integer              :: i,j
  integer              :: nframe,natm,nres
  integer              :: nstep,nmax,nsum,stride
  integer              :: progres,nwork,work
    call arg%add_option("-c",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
    call arg%add_option("-x",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
    call arg%add_option("-y",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
    call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
    call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
        &               help='trajectry time steps(ps). default 1.0')
    call arg%add_option("-u",narg=1,metavar='value',def=['-1'],help='upper bound of sumup')
    call arg%add_option("-i",narg=1,metavar='value',def=['-1'],help='stride')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
!
    call trj0%fetch(arg%args())
    if(trj0%natoms()<=0.or.trj0%nframes()<=0) STOP "this trajectory is empty!"
    call trj1%fetch(arg%args())
!
    natm   = trj0%natoms()
    nframe = trj0%nframes()
    nres   = trj0%nresidues()
!
    nmax   = minval([arg%optargi('-u',1),nframe])
    if(nmax<1)   nmax = nframe
    stride = arg%optargi('-i',1)
    if(stride<1) stride = 1
!
    progres = 0
    work    = 0
    nwork   = nmax*(nmax-1)/2 + (nframe-nmax) * nmax
!
    allocate(v0(9,nres,nmax),v1(9,nres,nmax))
!
    call fio%fetch(arg%optargs('-o',1))
    if(fio%is()/='') call fio%generate()
    write(fio%devn(),'(4f16.9)') 0.0,0.0,0.0,0.0
!
    do i=2,nframe,stride
      j = nframe - i + 1
      nstep = j/minval([i,nmax])+1
!
      call trj0%load(lb=1,ub=j,stride=nstep)
      call trj1%load(lb=i,ub=nframe,stride=nstep)
      nsum = trj0%nframes()
print*,nsum,trj1%nframes()
!
      v0(1:3,:,1:nsum) = center_of_residue(trj0,arg%optargs('-x',1))
      v0(4:6,:,1:nsum) = center_of_residue(trj0,arg%optargs('-y',1))
      v0(7:9,:,1:nsum) = center_of_residue(trj0,arg%optargs('-c',1))
cycle
      v1(1:3,:,1:nsum) = center_of_residue(trj1,arg%optargs('-x',1))
      v1(4:6,:,1:nsum) = center_of_residue(trj1,arg%optargs('-y',1))
      v1(7:9,:,1:nsum) = center_of_residue(trj1,arg%optargs('-c',1))
!
      write(fio%devn(),'(4f16.9)') real(i-1)*arg%optargf('-s',1),corr_calc(nres,nsum,v0(:,:,1:nsum),v1(:,:,1:nsum))
    enddo
  end subroutine compute
!
  function corr_calc(natm,nframe,v0,v1) result(res)
  integer,intent(in)                :: natm,nframe
  real,intent(inout)                :: v0(9,natm,nframe),v1(9,natm,nframe)
  real                              :: res(3)
  real                              :: disp(3)
  integer                           :: i,j
    do j=1,nframe
      do i=1,natm
        v0(1:3,i,j) = v0(1:3,i,j) - v0(7:9,i,j)
        v0(1:3,i,j) = v0(1:3,i,j) / sqrt(dot_product(v0(1:3,i,j),v0(1:3,i,j)))
        v0(4:6,i,j) = v0(4:6,i,j) - v0(7:9,i,j)
        v0(4:6,i,j) = v0(4:6,i,j) / sqrt(dot_product(v0(4:6,i,j),v0(4:6,i,j)))
        v0(7,i,j)   = v0(2,i,j)*v0(6,i,j)-v0(3,i,j)*v0(5,i,j)
        v0(8,i,j)   = v0(3,i,j)*v0(4,i,j)-v0(1,i,j)*v0(6,i,j)
        v0(9,i,j)   = v0(1,i,j)*v0(5,i,j)-v0(2,i,j)*v0(4,i,j)
        v0(7:9,i,j) = v0(7:9,i,j) / sqrt(dot_product(v0(7:9,i,j),v0(7:9,i,j)))
        v0(4,i,j)   = v0(2,i,j)*v0(9,i,j)-v0(3,i,j)*v0(8,i,j)
        v0(5,i,j)   = v0(3,i,j)*v0(7,i,j)-v0(1,i,j)*v0(9,i,j)
        v0(6,i,j)   = v0(1,i,j)*v0(8,i,j)-v0(2,i,j)*v0(7,i,j)
      enddo
    enddo
    do j=1,nframe
      do i=1,natm
        v1(1:3,i,j) = v1(1:3,i,j) - v1(7:9,i,j)
        v1(1:3,i,j) = v1(1:3,i,j) / sqrt(dot_product(v1(1:3,i,j),v1(1:3,i,j)))
        v1(4:6,i,j) = v1(4:6,i,j) - v1(7:9,i,j)
        v1(4:6,i,j) = v1(4:6,i,j) / sqrt(dot_product(v1(4:6,i,j),v1(4:6,i,j)))
        v1(7,i,j)   = v1(2,i,j)*v1(6,i,j)-v1(3,i,j)*v1(5,i,j)
        v1(8,i,j)   = v1(3,i,j)*v1(4,i,j)-v1(1,i,j)*v1(6,i,j)
        v1(9,i,j)   = v1(1,i,j)*v1(5,i,j)-v1(2,i,j)*v1(4,i,j)
        v1(7:9,i,j) = v1(7:9,i,j) / sqrt(dot_product(v1(7:9,i,j),v1(7:9,i,j)))
        v1(4,i,j)   = v1(2,i,j)*v1(9,i,j)-v1(3,i,j)*v1(8,i,j)
        v1(5,i,j)   = v1(3,i,j)*v1(7,i,j)-v1(1,i,j)*v1(9,i,j)
        v1(6,i,j)   = v1(1,i,j)*v1(8,i,j)-v1(2,i,j)*v1(7,i,j)
      enddo
    enddo
!
    res = 0.0
    do j=1,nframe
      do i = 1,natm
        res(1)  = res(1) + dot_product(v0(1:3,i,j),v1(1:3,i,j))
        res(2)  = res(2) + dot_product(v0(4:6,i,j),v1(4:6,i,j))
        res(3)  = res(3) + dot_product(v0(7:9,i,j),v1(7:9,i,j))
      enddo
    enddo
    res = res/real(natm*nframe)
  end function corr_calc
end program rcorr
