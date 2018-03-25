program msd
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
  integer              :: i,j
  integer              :: nstep,nmax,nsum,nframe,natm,stride
  integer              :: progres,nwork,work
    call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
    call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
    call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
        &               help='trajectry time steps(ps). default 1.0')
    call arg%add_option("-u",narg=1,metavar='value',def=['-1'],help='upper bound of sumup')
    call arg%add_option("-i",narg=1,metavar='value',def=['-1'],help='stride')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
!
    call trj0%fetch(arg%args())
    call trj0%atomselect(mask=arg%optargs('-m',1))
    if(trj0%natoms()<=0.or.trj0%nframes()<=0) STOP "this trajectory is empty!"
!
    call trj1%fetch(arg%args())
    call trj1%atomselect(mask=arg%optargs('-m',1))
!
    natm   = trj0%natoms()
    nframe = trj0%nframes()
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
    call fio%fetch(arg%optargs('-o',1))
    if(fio%is()/='') call fio%generate()
    write(fio%devn(),'(2f16.9)') 0.0,0.0
!
    j = nframe
!
    do i=2,nframe,stride
      j = nframe - i + 1
!
      nsum  = minval([i,nmax])
      nstep = j/nsum + 1
      work  = work + nsum
!
      call trj0%load(lb=1,ub=j,stride=nstep)
      call centering_coordinates(trj0)
      call trj1%load(lb=i,ub=nframe,stride=nstep)
      call centering_coordinates(trj1)
!
      write(fio%devn(),'(2f16.9)') real(i-1)*arg%optargf('-s',1),msd_calc(natm,trj1%nframes(),trj0%xyz(),trj1%xyz())
    enddo
  end subroutine compute
!
  pure function msd_calc(natm,nframe,xyz0,xyz1) result(res)
  integer,intent(in)                :: natm,nframe
  real,intent(in)                   :: xyz0(3,natm,nframe),xyz1(3,natm,nframe)
  real                              :: res
  real                              :: disp(3)
  integer                           :: i,j
    res = 0.0
    do j=1,nframe
      do i = 1,natm
        disp = xyz1(:,i,j) - xyz0(:,i,j)
        res  = res + dot_product(disp,disp)
      enddo
    enddo
    res = res/real(natm*nframe)
  end function msd_calc
end program msd
