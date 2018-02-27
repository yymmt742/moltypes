program msd
use spur_optparse
use moltypes
use spur_vector
use spur_io
implicit none
type(optparse)       :: arg
type(trajectory)     :: trj
type(fileio)         :: fio
integer              :: i,j
integer              :: nstep,nmax,nsum
integer              :: progres,nwork,work
type(vector_double)  :: msd_x,msd_y,msd_z,msd_r,rms_r

  call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
  call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
  call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
                     &help='trajectry time steps(ps). default 1.0')
  call arg%add_option("-u",narg=1,metavar='outputfile',def=['-1'],help='upper bound of sumup')
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
!
  write(STDOUT,'(a,i0,a)') '>>Loading coordinates...'
  call trj%load(arg%args(),mask=arg%optargs('-m',1))
  if(trj%n_atoms()<=0.or.trj%n_frames()<=0)STOP "this trajectory is empty!"
!
  call msd_x%reserve(trj%n_frames())
  call msd_y%reserve(trj%n_frames())
  call msd_z%reserve(trj%n_frames())
  call msd_r%reserve(trj%n_frames())
  call rms_r%reserve(trj%n_frames())
!
  nmax    = minval([arg%optargi('-u',1),trj%n_frames()])
  if(nmax<=0) nmax = trj%n_frames()
!
  progres = 0
  work    = 0
  nwork   = nmax*(nmax-1)/2 + (trj%n_frames()-nmax) * nmax
  call PrintProgresBar(nwork)
!
  j = trj%n_frames() + 1
!
  do i=1,trj%n_frames()-1
    j = j - 1
    nsum  = minval([i,nmax])
    nstep = (i/nsum)
    work  = work + nsum
    call msd_calc(trj%n_atoms(),i,nsum,nstep,trj%xyz(:,:,1:i),trj%xyz(:,:,j:trj%n_frames()),msd_x,msd_y,msd_z,msd_r,rms_r)
    call PrintCounter(nwork,work,progres)
  enddo
  call msd_x%push(0.d0)
  call msd_y%push(0.d0)
  call msd_z%push(0.d0)
  call msd_r%push(0.d0)
  call rms_r%push(0.d0)
  call PrintCounter(nwork,nwork,progres)
!
  fio = arg%optargs('-o',1)
  call fio%generate()
  do i=0,trj%n_frames()-1
    write(fio%devn(),'(f9.3,5f16.9)') real(i)*arg%optargf('-s',1),msd_x%pop(),msd_y%pop(),msd_z%pop(),msd_r%pop(),rms_r%pop()
  enddo
contains
  subroutine msd_calc(natm,nframe,nsum,nstep,xyz0,xyz1,msd_x,msd_y,msd_z,msd_r,rms_r)
  integer,intent(in)                :: natm,nframe,nsum,nstep
  double precision,intent(in)       :: xyz0(3,natm,nframe),xyz1(3,natm,nframe)
  type(vector_double),intent(inout) :: msd_x,msd_y,msd_z,msd_r,rms_r
  double precision                  :: xsum,ysum,zsum
  double precision,allocatable      :: rsum(:)
  double precision                  :: msd,rms
  double precision                  :: rpat,rpfr,rpaf
  double precision                  :: disp(3),dif
  integer                           :: i,j,k
    allocate(rsum(nsum))
    xsum = 0.d0 ; ysum = 0.d0 ; zsum = 0.d0 ; rsum = 0.d0
    rpat = 1.d0 / dble(natm)
    rpfr = 1.d0 / dble(nsum)
    rpaf = rpat * rpfr
!
    j = 1 - nstep ; k = 0
    do while (k<nsum)
      j = j + nstep
      k = k + 1
      do i = 1,natm
        disp = xyz1(:,i,j) - xyz0(:,i,j)
        disp = disp * disp
        xsum = xsum + disp(1)
        ysum = ysum + disp(2)
        zsum = zsum + disp(3)
        rsum(k) = rsum(k) + sum(disp)
      enddo
      rsum(k) = rsum(k) * rpat
    enddo
!
    xsum = xsum * rpaf
    ysum = ysum * rpaf
    zsum = zsum * rpaf
    msd  = sum(rsum) * rpfr
!
    i = 1 - nstep ; j = 0
    rms = 0.d0
    do j=1,nsum
      dif = rsum(j) - msd
      rms = rms + dif * dif
    enddo
    rms = sqrt(rms*rpfr)
!
    call msd_x%push(xsum)
    call msd_y%push(ysum)
    call msd_z%push(zsum)
    call msd_r%push(msd)
    call rms_r%push(rms)
    deallocate(rsum)
  end subroutine msd_calc
!
  subroutine PrintProgresBar(ntrj)
  integer,intent(in)    :: ntrj
    write(STDOUT,'(a,i0,a)') 'Processing for ',ntrj,' frames'
    write(STDOUT,'(a)') '|----------------------------------------|'
    write(STDOUT,'(a)',advance='no') '|' ; flush(STDOUT)
  end subroutine PrintProgresBar
!
  subroutine PrintCounter(ntrj,counter,progres)
  integer,intent(in)    :: ntrj,counter
  integer,intent(out)   :: progres
  integer               :: i,tmp
    tmp = 40*counter/ntrj
    do i=progres+1,tmp
      write(STDOUT,'(a)',advance='no') '>'
    enddo
    if(progres<tmp.and.tmp==40)write(STDOUT,'(a)') '|'
    progres = maxval([tmp,progres],1) ; flush(STDOUT)
  end subroutine PrintCounter
end program msd
