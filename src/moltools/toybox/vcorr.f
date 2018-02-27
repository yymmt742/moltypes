program vcorr
use spur_optparse
use moltypes
use spur_vector
use spur_io
implicit none
type(optparse)               :: arg
type(fileio)                 :: fio
double precision,allocatable :: vecx(:,:,:),vecy(:,:,:),vecz(:,:,:)
integer                      :: i,j
integer                      :: ntrj,nres,progres,nwork,work
type(vector_double)          :: acrx,rmsx,acry,rmsy,acrz,rmsz
!
  call arg%add_option("-p",narg=1,metavar='prmtop',    help='amber prmtop.')
  call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
  call arg%add_option("-c",narg=1,metavar='MaskString',help='Load mask string, for origin.')
  call arg%add_option("-x",narg=1,metavar='MaskString',help='Load mask string, for x axis.')
  call arg%add_option("-y",narg=1,metavar='MaskString',help='Load mask string, for y axis.')
  call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
                     &help='trajectry time steps(ps). default 1.0')
  call arg%parser()
  if(arg%narg()==0.or..not.arg%option('-p').or..not.arg%option('-c').or..not.arg%option('-x')) call arg%call_usage()
!
  write(STDOUT,'(a,i0,a)') '>>Loading coordinates...'
!
  nres = -1 ;  ntrj = -1
  call vecload(arg%optargs('-p',1),arg%optargs('-c',1),arg%args(),nres,ntrj,vecz)
  call vecload(arg%optargs('-p',1),arg%optargs('-x',1),arg%args(),nres,ntrj,vecx)
  call vecload(arg%optargs('-p',1),arg%optargs('-y',1),arg%args(),nres,ntrj,vecy)
!
  write(STDOUT,'(a,i0,a)') '>>Constructing vectors...'
!
  do j=1,ntrj
    vecx(:,:,j) = vecx(:,:,j) - vecz(:,:,j)
    do i=1,nres
      vecx(:,i,j) = vecx(:,i,j) / sqrt(dot_product(vecx(:,i,j),vecx(:,i,j)))
    enddo
  enddo
  do j=1,ntrj
    vecy(:,:,j) = vecy(:,:,j) - vecz(:,:,j)
    do i=1,nres
      vecy(:,i,j) = vecy(:,i,j) / sqrt(dot_product(vecy(:,i,j),vecy(:,i,j)))
    enddo
  enddo
!
  do j=1,ntrj
    do i=1,nres
      vecz(1,i,j) = vecx(2,i,j)*vecy(3,i,j)-vecx(3,i,j)*vecy(2,i,j)
      vecz(2,i,j) = vecx(3,i,j)*vecy(1,i,j)-vecx(1,i,j)*vecy(3,i,j)
      vecz(3,i,j) = vecx(1,i,j)*vecy(2,i,j)-vecx(2,i,j)*vecy(1,i,j)
      vecz(:,i,j) = vecz(:,i,j) / sqrt(dot_product(vecz(:,i,j),vecz(:,i,j)))
      vecy(1,i,j) = vecx(2,i,j)*vecz(3,i,j)-vecx(3,i,j)*vecz(2,i,j)
      vecy(2,i,j) = vecx(3,i,j)*vecz(1,i,j)-vecx(1,i,j)*vecz(3,i,j)
      vecy(3,i,j) = vecx(1,i,j)*vecz(2,i,j)-vecx(2,i,j)*vecz(1,i,j)
    enddo
  enddo
!
  call acrx%reserve(ntrj) ; call acry%reserve(ntrj) ; call acrz%reserve(ntrj)
  call rmsx%reserve(ntrj) ; call rmsy%reserve(ntrj) ; call rmsz%reserve(ntrj)
!
  progres = 0
  work    = 0
  nwork   = ntrj*(ntrj-1)/2
!
  call PrintProgresBar(nwork)
!
  j = ntrj + 1
  do i=1,ntrj-1
    j = j - 1
    work = work + i
    call vcorr_calc(nres,i,vecx(:,:,1:i),vecx(:,:,j:ntrj),acrx,rmsx)
    call vcorr_calc(nres,i,vecy(:,:,1:i),vecy(:,:,j:ntrj),acry,rmsy)
    call vcorr_calc(nres,i,vecz(:,:,1:i),vecz(:,:,j:ntrj),acrz,rmsz)
    call PrintCounter(nwork,work,progres)
  enddo
  call PrintCounter(nwork,nwork,progres)
!
  call fio%generate(arg%optargs('-o',1))
  do i=1,ntrj-1
    write(fio%devn(),'(f9.3,6f16.9)') real(i)*arg%optargf('-s',1),acrx%pop(),rmsx%pop(),acry%pop(),rmsy%pop(),acrz%pop(),rmsz%pop()
  enddo
  call fio%quit()
contains
  subroutine vecload(prm,mask,crd,nres,ntrj,vec)
  character(*),intent(in)                  :: prm,mask,crd(:)
  integer,intent(inout)                    :: ntrj,nres
  double precision,allocatable,intent(out) :: vec(:,:,:)
  type(trajectory)                         :: trj
  integer                                  :: i
    call trj%load(prm,mask=mask)
    call trj%load(crd)
    if(ntrj<0) ntrj = trj%n_frames()
    if(nres<0) nres = trj%n_residues()
    if(nres/=trj%n_residues()) STOP 'ERROR :: n residue is not match.'
    allocate(vec(3,nres,ntrj))
    do i=1,ntrj
      vec(:,:,i) = trj%center_residue(i)
    enddo
  end subroutine vecload
!
  subroutine vcorr_calc(nres,nframe,vec0,vec1,acr,rms)
  integer,intent(in)                :: nres,nframe
  double precision,intent(in)       :: vec0(3,nres,nframe),vec1(3,nres,nframe)
  type(vector_double),intent(inout) :: acr,rms
  double precision                  :: xsum,ysum
  double precision                  :: rsum(nframe)
  double precision                  :: cor,mse
  double precision                  :: rprs,rpfr
  double precision                  :: disp(3),dif
  integer                           :: i,j,k
    rprs = 1.d0 / dble(nres)
    rpfr = 1.d0 / dble(nframe)
!
    rsum = 0.d0
    do j = 1,nframe
      do i = 1,nres
        rsum(j) = rsum(j) + dot_product(vec0(:,i,j),vec1(:,i,j))
      enddo
      rsum(j) = rsum(j) * rprs
    enddo
!
    cor = sum(rsum) * rpfr
!
    mse = 0.d0
    do i = 1,nframe
      dif = rsum(i) - cor
      mse = mse + dif * dif
    enddo
!
    mse = sqrt(mse*rpfr)
!
    call acr%push(cor)
    call rms%push(mse)
  end subroutine vcorr_calc
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
end program vcorr
