program vpaa_local
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use moltypes
implicit none
real,parameter :: Pi = 3.1415926535897932
real,parameter :: AngRad = Pi/180.0
real,parameter :: RadAng = 180.0/Pi
real           :: time
  time = omp_get_wtime()
  call compute_vpaa_local()
  write(stdout,'(a,f11.3,a)') 'Total calculation time :: ',omp_get_wtime()-time,' sec.'
contains
  subroutine compute_vpaa_local()
  use spur_histgram
  use spur_optparse
  type(optparse)      :: arg
  type(moltype)       :: mt
! type(histgram_2d)   :: rx,ry,rz
! type(histgram_3d)   :: sdf
  integer             :: natm,nres,ntrj
  real,allocatable    :: X(:,:)
  integer,allocatable :: resid(:)
  logical,allocatable :: cm1(:,:),dx1(:,:),dy1(:,:)
  logical,allocatable :: cm2(:,:),dx2(:,:),dy2(:,:)
  integer             :: i
  call arg%add_description('cluster analysys. trajectry must be 0 fixed.')
  call arg%add_option("-d1",narg=3,metavar='MaskString',help='com,vector1,vector2.')
  call arg%add_option("-d2",narg=3,metavar='MaskString',help='com,vector1,vector2.')
  call arg%add_option("-r",narg=1,metavar='value', help='radial max. default 15.0',def=['20.0'])
  call arg%add_option("-rg",narg=1,metavar='value',help='radial grid. default 1.0',def=['0.5'])
  call arg%add_option("-tg",narg=1,metavar='value',help='theta grid. default 1.0',def=['1.0'])
  call arg%add_option("-sdf",narg=1,metavar='filename',help='sdf output file.',def=['sdf.dx'])
  call arg%add_option("-fv",narg=1,metavar='filename',help='feature vector output file.',def=['fvector'])
  call arg%parser()
  if(.not.arg%option('-d1').or..not.arg%option('-d2').or.arg%narg()==0) call arg%call_usage()
!
  call mt%fetch(arg%args())
  natm = mt%natoms() ; nres = mt%nresidues() ; ntrj = mt%nframes()
  allocate(X(3,natm),resid(natm))
  allocate(cm1(3,natm),dx1(3,natm),dy1(3,natm))
  allocate(cm2(3,natm),dx2(3,natm),dy2(3,natm))
  resid = mt%inq('resid',0)
!
  cm1 = mt%getmask(arg%optargs('-d1',1),3)
  dx1 = mt%getmask(arg%optargs('-d1',2),3)
  dy1 = mt%getmask(arg%optargs('-d1',3),3)
!
  cm2 = mt%getmask(arg%optargs('-d2',1),3)
  dx2 = mt%getmask(arg%optargs('-d2',2),3)
  dy2 = mt%getmask(arg%optargs('-d2',3),3)
!
!   if(natm<=0.or.nres<=0.or.nres/=dres1.or.nres/=dres2)then
!     write(stdout,'(a,a)') 'No atoms in ',prm ; RETURN
!   endif
  
! call compute(arg%narg(),arg%optargs('-p',1),arg%args(),arg%optargs('-m',1),arg%optargs('-d'),&
!             &arg%optargd('-r',1),arg%optargd('-rg',1),arg%optargd('-tg',1),&
!             &arg%option('-sdf'),arg%optargs('-sdf',1),arg%optargs('-fv',1))
  end subroutine compute_vpaa_local
!
! subroutine compute(ninp,prm,inp)
! subroutine compute(ninp,prm,inp,mask,dest,rmax,rgrid,tgrid,sdfcom,sdfout,fvout)
! use spur_io
! integer,intent(in)             :: ninp
! character(*),intent(in)        :: prm,inp(ninp),mask,dest(3)
! double precision,intent(in)    :: rmax,rgrid,tgrid
! logical,intent(in)             :: sdfcom
! character(*),intent(in)        :: sdfout,fvout
! type(trajectory)               :: trj
! integer                        :: natm,nres,dres1,dres2,ntrj
! double precision,allocatable   :: c(:,:),d1(:,:),d2(:,:)
! double precision               :: l1,l2,nrev,threshold
! integer,allocatable            :: resid(:)
! integer                        :: i,j,d,from,to,load
! integer                        :: ncid,varid,is
! integer                        :: nsum,ncatm
! integer                        :: progres
! type(fileio)                   :: nc
! character(:),allocatable       :: title
!   call trj%load(prm,mask=mask)
!   natm = trj%n_atoms()
!   allocate(resid(natm)) ; resid = trj%residue_index()
!   call trj%atomselect(dest(1)) ; nres  = trj%n_residues()
!   call trj%atomselect(dest(2)) ; dres1 = trj%n_residues()
!   call trj%atomselect(dest(3)) ; dres2 = trj%n_residues()
!   if(natm<=0.or.nres<=0.or.nres/=dres1.or.nres/=dres2)then
!     write(stdout,'(a,a)') 'No atoms in ',prm ; RETURN
!   endif
!   allocate(c(3,nres),d1(3,nres),d2(3,nres))
!
!   call rx%setup(hmin1=0.d0,hmax1=rmax,hmin2=0.d0,hmax2=180.d0,grid1=rgrid,grid2=tgrid)
!   call ry%setup(hmin1=0.d0,hmax1=rmax,hmin2=0.d0,hmax2= 90.d0,grid1=rgrid,grid2=tgrid)
!   call rz%setup(hmin1=0.d0,hmax1=rmax,hmin2=0.d0,hmax2= 90.d0,grid1=rgrid,grid2=tgrid)
!   call sdf%setup(hmin=-rmax,hmax=rmax,grid=rgrid)
!
!   l1 = 1.d0 / dble(nres)
!   l2 = 1.d0 / dble(nres*(nres-1))
!   nsum = 0
!   ncatm = count(resid==1)*2
!   d = 4
!
!   call generateFV(fvout,d,nres,ninp,inp,prm)
!   is = nf90_open(fvout,nf90_write,ncid)
!   is = nf90_inq_varid(ncid,"fvector",varid)
!
!   load = 5000
!   do i=1,ninp
!     write(stdout,'(a,a,a)') 'Processing ',inp(i),'...'
!     call trj%atomselect(mask)
!     from = 1 ; to = load
!     write(stdout,'(a)') '|----------------------------------------|'
!     do
!       call trj%load(inp(i),lb=from,ub=to,from=1)
!       ntrj = trj%n_frames()
!       if(ntrj<=0)EXIT
!       nsum = nsum + ntrj
!       progres = 0
!       write(stdout,'(a)',advance='no') '|' ; flush(stdout)
!       do j=1,ntrj
!         call PrintCounter(ntrj,j,progres)
!         call trj%atomselect(dest(1)) ; c  = trj%center_residue(j)
!         call trj%atomselect(dest(2)) ; d1 = trj%center_residue(j)
!         call trj%atomselect(dest(3)) ; d2 = trj%center_residue(j)
!         call analysis_structure(natm,nres,ncatm,nres*(nres-1)/2,ncid,varid,trj%xyz(1:3,1:natm,j),c,d1,d2,d,resid,sdf,rx,ry,rz,sdfcom)
!       enddo
!       call PrintCounter(ntrj,ntrj,progres)
!       write(stdout,'(a)') '|' ; flush(stdout)
!       from = from + load ; to = to + load
!     enddo
!   enddo
!   is = nf90_close(ncid)
!
!   nrev = 1.d0 / maxval([rx%maxval(),ry%maxval(),rz%maxval()],1)
!   call rx%scale(nrev)
!   call ry%scale(nrev)
!   call rz%scale(nrev)
!   call rx%export('x_dist.log')
!   call ry%export('y_dist.log')
!   call rz%export('z_dist.log')
!
!   if(sdfcom)then
!     write(stdout,'(a)',advance='no') 'Writing logfiles .....' ; flush(stdout)
!     call sdf%symmetry('c2v')
!     call sdf%export_dx(trim(sdfout))
!     write(stdout,'(a)') ' COMPLETE!' ; flush(stdout)
!   endif
! end subroutine compute
!
! subroutine analysis_structure(natm,nres,ncatm,l2,ncid,varid,xyz,c,d1,d2,d,resid,sdf,rx,ry,rz,sdfcom)
! integer,intent(in)              :: natm,nres,ncatm,l2,ncid,varid,d
! double precision,intent(in)     :: xyz(3,natm)
! double precision,intent(in)     :: c(3,nres),d1(3,nres),d2(3,nres)
! integer,intent(in)              :: resid(natm)
! type(histgram_3d),intent(inout) :: sdf
! type(histgram_2d),intent(inout) :: rx,ry,rz
! logical,intent(in)              :: sdfcom
! double precision                :: v(3),v1(3),v2(3)
! double precision                :: vxyz(9,nres),space(3),tmp(4)
! double precision                :: fv(d,l2)
! logical                         :: mmask(3,natm)
! logical                         :: minl(4)
! double precision                :: r(nres)
! integer                         :: i,j,k,l,is,ml
!   do i=1,nres
!     r(i) = dot_product(c(:,i),c(:,i))
!     r(i) = 1.d0 / sqrt(r(i))
!   enddo
!
!   call baseset(nres,c,d1,d2,vxyz)
!
!   k = 0
!   do j=1,nres
!     do i=j+1,nres
!       k = k + 1
!       v = c(:,i) - c(:,j)
!
!       if(sdfcom)then
!         space(1)  = dot_product(vxyz(1:3,j),v)
!         space(2)  = dot_product(vxyz(4:6,j),v)
!         space(3)  = dot_product(vxyz(7:9,j),v)
!         call sdf%stack(space,1.d0)
!       endif
!
!       fv(1,k) = sqrt(dot_product(v,v))
!       v       = v / fv(1,k)
!       fv(2,k) = dot_product(vxyz(1:3,i),v)
!       fv(3,k) = dot_product(vxyz(1:3,j),-v)
!       fv(4,k) = 2.d0*abs(dot_product(vxyz(7:9,i),vxyz(7:9,j)))
!
!       call rx%stack([fv(1,k),radang*acos(dot_product(vxyz(1:3,i),vxyz(1:3,j)))],1.d0)
!       call ry%stack([fv(1,k),radang*acos(abs(dot_product(vxyz(4:6,i),vxyz(4:6,j))))],1.d0)
!       call rz%stack([fv(1,k),radang*acos(abs(dot_product(vxyz(7:9,i),vxyz(7:9,j))))],1.d0)
!
!     enddo 
!   enddo
!   call writefv(ncid,varid,d,l2,fv)
!
!   RETURN
! end subroutine analysis_structure
!
! subroutine writefv(ncid,varid,d,l2,fv)
! integer,intent(in)              :: ncid,varid,l2,d
! double precision,intent(in)     :: fv(d,l2)
! integer,save                    :: frame = 0
! logical                         :: minl(l2)
! integer                         :: i,is,ml
!   minl = .TRUE. ; frame = frame + 1
!   do i=1,l2
!     ml = minloc(fv(1,:),1,mask=minl)
!     is = nf90_put_var(ncid,varid,fv(:,ml),start=[d*(i-1)+1,frame],count=[d,1])
!     minl(ml) = .FALSE.
!   enddo
! end subroutine writefv
!
! subroutine baseset(nres,v0,v1,v2,vxyz)
! integer,intent(in)               :: nres
! double precision,intent(in)      :: v0(3,nres),v1(3,nres),v2(3,nres)
! double precision,intent(out)     :: vxyz(9,nres)
! double precision                 :: vx(3),vy(3)
! double precision                 :: vt(3),vz(3)
! integer                          :: i
!!$omp parallel do private(i,vx,vy,vt,vz)
!   do i=1,nres
!     vx(:) = v1(:,i) - v0(:,i)
!     vy(:) = v2(:,i) - v0(:,i)
!     vz(1) = vx(2)*vy(3)-vx(3)*vy(2)
!     vz(2) = vx(3)*vy(1)-vx(1)*vy(3)
!     vz(3) = vx(1)*vy(2)-vx(2)*vy(1)
!     vt(1) = vx(2)*vz(3)-vx(3)*vz(2)
!     vt(2) = vx(3)*vz(1)-vx(1)*vz(3)
!     vt(3) = vx(1)*vz(2)-vx(2)*vz(1)
!     vxyz(1:3,i) = vx(:) / sqrt(dot_product(vx,vx))
!     vxyz(4:6,i) = vt(:) / sqrt(dot_product(vt,vt))
!     vxyz(7:9,i) = vz(:) / sqrt(dot_product(vz,vz))
!   enddo
!!$omp end parallel do
! end subroutine baseset
!
! subroutine generateFV(filename,d,nres,ninp,inp,prm)
! character(*),intent(in)  :: filename
! integer,intent(in)       :: nres,d,ninp
! character(*),intent(in)  :: inp(ninp),prm
! integer                  :: ncid,did,fdid,fvid
! integer                  :: ninpid
! character(12)            :: tmp
! integer                  :: i,is
!   is = nf90_create(filename,nf90_clobber,ncid)
!
!   is = nf90_def_dim(ncid,"frame",nf90_unlimited,fdid)
!   is = nf90_def_dim(ncid,"dimension",d*nres*(nres-1)/2,did)
!   is = nf90_def_dim(ncid,"ninp",ninp,ninpid)
!
!   is = nf90_def_var(ncid,"fvector",nf90_double,[did,fdid],fvid)
!   is = nf90_put_att(ncid,nf90_global,"prmtop",prm)
!   do i=1,ninp
!     write(tmp,'(i0)')i
!     is = nf90_put_att(ncid,nf90_global,"input"//trim(tmp),inp(i))
!   enddo
!
!   is = nf90_enddef(ncid)
!   is=nf90_close(ncid)
!   RETURN
! end subroutine generateFV
!
! subroutine PrintCounter(ntrj,counter,progres)
! integer,intent(in)    :: ntrj,counter
! integer,intent(inout) :: progres
! integer               :: i,tmp
!   tmp = 40*counter/ntrj
!   do i=progres+1,tmp
!     write(stdout,'(a)',advance='no') '>' ; flush(stdout)
!   enddo
!   progres = maxval([tmp,progres],1)
! end subroutine PrintCounter
end program vpaa_local
