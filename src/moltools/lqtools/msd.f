program msd
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
  integer              :: i,j
  integer              :: nstep,nmax,nsum,nframe,natm
    call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
    call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
    call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
        &               help='trajectry time steps(ps). default 1.0')
    call arg%add_option("-u",narg=1,metavar='value',def=['-1'],help='upper bound of sumup')
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
    nframe = trj0%nfetchframes()
!
    nmax   = minval([arg%optargi('-u',1),nframe])
    if(nmax<1)   nmax = nframe
!
    call fio%fetch(arg%optargs('-o',1))
    if(fio%is()/='') call fio%generate()
    write(fio%devn(),'(2f16.9)') 0.0,0.0
!
    i = 2
!
    do while(i<=nframe)
      j = nframe - i + 1
!
      nsum  = minval([i,nmax])
      nstep = j/nsum + 1
!
      call trj0%load(lb=1,ub=j,stride=nstep)
      call trj0%centering_coordinates()
      call trj1%load(lb=i,ub=nframe,stride=nstep)
      call trj1%centering_coordinates()
!
      write(fio%devn(),'(2f16.9)') real(i-1)*arg%optargf('-s',1),&
&           msd_calc(trj1%natoms(),trj1%nframes(),trj0%xyz(:,:,:trj0%nframes()),trj1%xyz(:,:,:trj1%nframes()))
      i = i + 10**(int(log10(real(i))))
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
