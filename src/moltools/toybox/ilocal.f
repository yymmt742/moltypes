program ilocal
use spur_optparse
use moltypes
use spur_vector
use spur_io
use spur_histgram
implicit none
double precision,parameter   :: Pi = 3.1415926535897932d0
double precision,parameter   :: AngRad = Pi/180.d0
double precision,parameter   :: RadAng = 180.d0/Pi
type(optparse)               :: arg
type(fileio)                 :: fio
double precision,allocatable :: orig(:,:,:),vecx(:,:,:),vecy(:,:,:),vecz(:,:,:)
double precision,allocatable :: box(:,:),rbox(:,:)
integer                      :: i,j,k
integer                      :: ntrj,nres,progres
double precision             :: vol,volr,vols,volt,rcpr,tht,rcpn,rcpdr,rcpdrt,rcpdr3
double precision             :: radial,space(3),v(3)
type(histgram_1d)            :: rdf
type(histgram_2d)            :: rx,ry,rz
type(histgram_3d)            :: sdf
!
  call arg%add_option("-p",   narg=1, metavar='prmtop',   help='amber prmtop.')
  call arg%add_option("-c",   narg=1, metavar='Mask',     help='Load mask string, for origin.')
  call arg%add_option("-x",   narg=1, metavar='Mask',     help='Load mask string, for x axis.')
  call arg%add_option("-y",   narg=1, metavar='Mask',     help='Load mask string, for y axis.')
  call arg%add_option("-o",   narg=1, metavar='string',   help='output file name.',def=['ilocal'])
  call arg%add_option("-r",   narg=1, metavar='value',    help='radial bin size.   default [  1.0] angs.',def=['1.0'])
  call arg%add_option("-R",   narg=1, metavar='value',    help='radial maxval.     default [ 30.0] angs.',def=['30.0'])
  call arg%add_option("--cut",narg=1, metavar='value',    help='sdf cutoff length. default [999.0] angs.',def=['999.0'])
  call arg%add_option("-s",   narg=1, metavar='symmetry', help='symmetry group')
  call arg%parser()
  if(arg%narg()==0.or..not.arg%option('-p').or..not.arg%option('-c').or..not.arg%option('-x')) call arg%call_usage()
!
  write(STDOUT,'(a,i0,a)') '>>Loading coordinates...'
!
  nres = -1 ;  ntrj = -1
  call vecload(arg%optargs('-p',1),arg%optargs('-c',1),arg%args(),nres,ntrj,orig,box,rbox)
  call vecload(arg%optargs('-p',1),arg%optargs('-x',1),arg%args(),nres,ntrj,vecx)
  call vecload(arg%optargs('-p',1),arg%optargs('-y',1),arg%args(),nres,ntrj,vecy)
  allocate(vecz(3,nres,ntrj))
!
  write(STDOUT,'(a,i0,a)') '>>Constructing vectors...'
!
  do j=1,ntrj
    vecx(:,:,j) = vecx(:,:,j) - orig(:,:,j)
    vecy(:,:,j) = vecy(:,:,j) - orig(:,:,j)
    do i=1,nres
      vecx(:,i,j) = vecx(:,i,j) / sqrt(dot_product(vecx(:,i,j),vecx(:,i,j)))
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
  call rdf%setup(1.d0,arg%optargd('-R',1),arg%optargd('-r',1))
  call rx%setup(hmin1=1.d0,hmax1=arg%optargd('-R',1),hmin2=0.d0,hmax2= 90.d0,grid1=arg%optargd('-r',1),grid2=arg%optargd('-r',1))
  call ry%setup(hmin1=1.d0,hmax1=arg%optargd('-R',1),hmin2=0.d0,hmax2= 90.d0,grid1=arg%optargd('-r',1),grid2=arg%optargd('-r',1))
  call rz%setup(hmin1=1.d0,hmax1=arg%optargd('-R',1),hmin2=0.d0,hmax2= 90.d0,grid1=arg%optargd('-r',1),grid2=arg%optargd('-r',1))
  call sdf%setup(-arg%optargd('-R',1),arg%optargd('-R',1),arg%optargd('-r',1))
!
  progres = 0
!
  call PrintProgresBar(ntrj)
!
  rcpn   = 1.d0 / dble(nres*nres*ntrj)
  rcpdr  = arg%optargd('-r',1)
  rcpdr3 = rcpn /(rcpdr * rcpdr * rcpdr)
  rcpdr  = 2.d0 * rcpn / rcpdr
  rcpdrt = rcpdr * 90.d0
!
  do k=1,ntrj
    vol  = box(1,k)*box(2,k)*box(3,k)
    volr = vol  * rcpdr
    vols = vol  * rcpdr3
    volt = vol  * rcpdrt
    do j=1,nres
      do i=1,nres
        if(i==j)CYCLE
        v = orig(:,i,k) - orig(:,j,k)
        v = v - box(:,k) * dnint(v*rbox(:,k))
        rcpr      = dot_product(v,v)
        radial    = sqrt(rcpr)
        space(1)  = dot_product(vecx(:,i,k),v)
        space(2)  = dot_product(vecy(:,i,k),v)
        space(3)  = dot_product(vecz(:,i,k),v)
        call sdf%stack(space,vols)
        if(i<j)CYCLE
        call rdf%stack(radial,volr)
        rcpr = volt / rcpr
        tht  = theta(vecx(:,i,k),vecx(:,j,k))
        call rx%stack([radial,RadAng*tht],rcpr/sin(tht))
        tht  = theta(vecy(:,i,k),vecy(:,j,k))
        call ry%stack([radial,RadAng*tht],rcpr/sin(tht))
        tht  = theta(vecz(:,i,k),vecz(:,j,k))
        call rz%stack([radial,RadAng*tht],rcpr/sin(tht))
      enddo 
    enddo
    call PrintCounter(ntrj,k,progres)
  enddo
  deallocate(orig,vecx,vecy,vecz,box,rbox)
!
  call PrintCounter(ntrj,ntrj,progres)
!
  write(stdout,'(a)',advance='yes') 'rdf writing...'
  call rdf%scale(0.07957747154d0,-2.d0)
  call rdf%export(trim(arg%optargs('-o',1))//'.rdf')
!
  write(stdout,'(a)',advance='yes') 'sdf writing...'
  call sdf%symmetry(arg%optargs('-s',1))
  call sdf%export_dx(trim(arg%optargs('-o',1))//'.dx',cutoff=arg%optargd('--cutoff',1),cutval=1.d0)
!
  write(stdout,'(a)',advance='yes') 'local interaction writing...'
!  call rx%scale()
  call rx%export(trim(arg%optargs('-o',1))//'_rx.df')
!  call ry%scale()
  call ry%export(trim(arg%optargs('-o',1))//'_ry.df')
!  call rz%scale()
  call rz%export(trim(arg%optargs('-o',1))//'_rz.df')
!
contains
  subroutine vecload(prm,mask,crd,nres,ntrj,vec,box,rbox)
  character(*),intent(in)                           :: prm,mask,crd(:)
  integer,intent(inout)                             :: ntrj,nres
  double precision,allocatable,intent(inout)        :: vec(:,:,:)
  double precision,allocatable,intent(out),optional :: box(:,:),rbox(:,:)
  type(trajectory)                                  :: trj
  integer                                           :: i
    call trj%load(prm,mask=mask)
    call trj%load(crd)
    if(ntrj<0) ntrj = trj%n_frames()
    if(nres<0) nres = trj%n_residues()
    if(nres/=trj%n_residues()) STOP 'ERROR :: n residue is not match.'
    allocate(vec(3,nres,ntrj))
    do i=1,ntrj
      vec(:,:,i) = trj%center_residue(i)
    enddo
    if(present(box))then
      if(allocated(box))deallocate(box)
      allocate(box(3,ntrj))
      box = trj%box(:,1:ntrj)
      if(present(rbox))then
        if(allocated(rbox))deallocate(rbox)
        allocate(rbox(3,ntrj))
        rbox = 1.d0 / box
      endif
    endif
    call trj%clear()
  end subroutine vecload
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
!
  subroutine DestractAbort(errlog)
  character(*),intent(in) :: errlog
   if(allocated(orig))deallocate(orig)
   if(allocated(vecx))deallocate(vecx)
   if(allocated(vecy))deallocate(vecy)
   if(allocated(vecz))deallocate(vecz)
   if(allocated(box)) deallocate(box)
   if(allocated(rbox))deallocate(rbox)
   write(stderr,'(a)') errlog
   STOP 1
  end subroutine DestractAbort
!
  pure function theta(v1,v2) result(res)
  double precision,intent(in) :: v1(3),v2(3)
  double precision            :: res
    res = acos(abs(dot_product(v1,v2)))
  end function theta
end program ilocal
