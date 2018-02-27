include "mkl_vsl.fi"
program sdf
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use spur_optparse
use moltypes
use spur_io
use spur_vector
use spur_histgram
!
type(optparse)                :: arg
double precision              :: param(4)
character(:),allocatable      :: c
integer                       :: is
type(vector_character),save   :: inp
character(:),allocatable,save :: gout
character(:),allocatable,save :: m
double precision,save         :: gmax=30.d0,ggrd=1.0d0
double precision,save         :: dmax=20.d0,dgrd=0.5d0
double precision,save         :: omax=20.d0,ogrd=0.5d0
double precision,save         :: fmax=20.d0,fgrd=0.5d0
  call arg%add_option("-m",narg=1,metavar='MaskString',&
                     &help='Load mask string <from> <to>, like as vmd.')
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
  allocate(character(0)::c)
  c = arg%optargs('-g',1) ; read(c,*,iostat=is)param(1) ; if(is/=0)param(1) =  1.d0
  c = arg%optargs('-g',2) ; read(c,*,iostat=is)param(2) ; if(is/=0)param(2) =  1.d0
  c = arg%optargs('-g',3) ; read(c,*,iostat=is)param(3) ; if(is/=0)param(3) = 30.d0
  c = arg%optargs('-g',4) ; read(c,*,iostat=is)param(4) ; if(is/=0)param(4) = 20.d0
  deallocate(c)
!
  call mlts2_set_filename(inpfile= arg%args(),&
                        &mask   = arg%optargs('-m',1))
!
  call mlts2_compute_esp()
contains
  subroutine mlts2_set_filename(inpfile,mask)
  character(*),intent(in),optional :: inpfile(:)
  character(*),intent(in),optional :: mask
    if(present(inpfile)) call inp%push(inpfile)
    if(.not.allocated(m)) allocate(character(0)::m) ; if(present(mask)) m = trim(mask)
  end subroutine mlts2_set_filename
!
  subroutine mlts2_set_param(gyromax,gyrogrid,diagmax,diaggrid,oscmax,oscgrid,flmax,flgrid)
  double precision,intent(in),optional :: gyromax,gyrogrid
  double precision,intent(in),optional :: diagmax,diaggrid
  double precision,intent(in),optional :: oscmax,oscgrid
  double precision,intent(in),optional :: flmax,flgrid
    if(present(gyromax))  gmax = gyromax
    if(present(gyrogrid)) ggrd = gyrogrid
    if(present(diagmax))  dmax = diagmax
    if(present(diaggrid)) dgrd = diaggrid
    if(present(oscmax))   omax = oscmax
    if(present(oscgrid))  ogrd = oscgrid
    if(present(flmax))  fmax = flmax
    if(present(flgrid)) fgrd = flgrid
  end subroutine mlts2_set_param
!
  subroutine mlts2_compute_esp()
  type(trajectory)               :: trj
  double precision,allocatable   :: crd(:,:),cmat(:,:),chg(:)
  integer,allocatable            :: resid(:)
  integer                        :: natm,nres,ntrj
  double precision               :: rcprgrd,rcpsgrd,rcpn
  integer                        :: i,j,progres
  type(histgram_1d)              :: gyro,diag,osc,fl
    call mlts2_set_filename()
    call trj%load(inp%lookup(),mask=m)
!
    natm = trj%n_atoms() ; nres = trj%n_residues() ; ntrj = trj%n_frames()
    if(natm<=0.or.nres<=0.or.ntrj<=0)RETURN
!
    allocate(crd(3,natm),cmat(natm,natm),resid(natm),chg(natm))
!
    resid = trj%residue_index()
    chg = trj%charges()
    rcpn   = 1.d0 / dble(ntrj*nres**2)
!
    call get_cmat(natm,chg,cmat)
!
    call gyro%setup(1.d0,gmax,ggrd)
    call diag%setup(-dmax,dmax,dgrd)
    call osc%setup(-omax,omax,ogrd)
    call fl%setup(-fmax,fmax,fgrd)
!
!!!  Calc. Dipole Moment.
!
    write(stdout,'(a,i0,a)') 'Processing distribution... :: ',ntrj,' frames'
    write(stdout,'(a)') '|----------------------------------------|'
    write(stdout,'(a)',advance='no') '|' ; flush(stdout)
!
    progres = 0
    do i=1,ntrj
      crd = trj%coordinates(i)
      call calc_elpot(natm,nres,rcpn,resid,crd,chg,cmat,trj%box(:,i),gyro,diag,osc,fl)
!
      call PrintCounter(ntrj,i,progres)
    enddo
    write(stdout,'(a)') '|' ; flush(stdout)
!
!   rcprgrd = 1.d0 / rgrd
!   rcpsgrd = 1.d0 / sgrd
!
!   progres = 0
!   do i=1,ntrj
!     call get_coordinate(trj,o,d,x,y,nres,dres,i,origin,dest,vxyz)
!     call calc_distribution(nres,dres,rcprgrd,rcpsgrd,rcpn,origin,vxyz,dest,trj%box(:,i),SameResidue,rdf,sdf)
!     call PrintCounter(ntrj,i,progres)
!   enddo
!   write(stdout,'(a)') '|' ; flush(stdout)
!
!   call rdf%scale(0.07957747154d0,-2.d0)
!
    write(stdout,'(a)',advance='yes') 'gyro writing...'
    call gyro%export('gyro')
    call diag%export('diag')
    call osc%export('osc')
    call fl%export('fl')
!   write(stdout,'(a)',advance='yes') 'sdf writing...'
!   call sdf%symmetry(sym)
!   call sdf%export(sout)
!
!   deallocate(origin,vxyz,dest,SameResidue)
!   RETURN
  end subroutine mlts2_compute_esp
!
  subroutine get_cmat(natm,chg,cmat)
  integer,intent(in)              :: natm
  double precision,intent(in)     :: chg(natm)
  double precision,intent(out)    :: cmat(natm,natm)
  integer                         :: i,j
    do j=1,natm
      do i=j+1,natm
        cmat(i,j)=chg(i)*chg(j)*332.06d0
      enddo
    enddo
  end subroutine get_cmat
!
  subroutine calc_elpot(natm,nres,rcpn,resid,crd,chg,cmat,box,gyro,diag,osc,fl)
!!$ use omp_lib
  integer,intent(in)              :: natm,nres
  double precision,intent(in)     :: rcpn
  integer,intent(in)              :: resid(:)
  double precision,intent(in)     :: crd(3,natm),cmat(natm,natm),chg(natm),box(3)
  double precision                :: elstat(nres,nres),eigenmat(nres,nres)
  double precision                :: enorm(nres,nres),eigen(nres)
  double precision                :: myu(3,nres),myut(3,nres),myunorm(nres)
  double precision                :: com(3,nres,nres)
  double precision                :: rmean(3),rsum(nres),rtmp(3)
  double precision                :: RcpBox(3),v(3),myu2
  type(histgram_1d),intent(inout) :: gyro,diag,osc,fl
  double precision                :: RcpKbt,EFLORS
! type(histgram_3d),intent(inout) :: sdf
! double precision                :: volr,vols
  integer                         :: i,j
      myu = 0.d0
      Rcpbox = 1.d0/box
      do i=1,Natm
        myu(:,resid(i)) = myu(:,resid(i)) + chg(i) * crd(:, i)
      enddo
!
      elstat = 0.d0
      do j=1,natm
        do i=j+1,natm
         if(resid(i)==resid(j))CYCLE
         v = crd(:, i) - crd(:, j)
         v = v - box(1:3) * anint(v*rcpbox)
         elstat(resid(i), resid(j)) = elstat(resid(i), resid(j)) + cmat(i,j) / sqrt(dot_product(v,v))
       enddo
     enddo
!
     EigenMat = 0.d0
     do j=1, nres
       do i=j+1, nres
         elstat(j,i) = elstat(i,j)
         eigenmat(i,j) = elstat(i,j)
       enddo
     enddo
!
     call d_solver(eigenmat,eigen,nres)
!
     myut=0.d0
!
     do j=1,nres
       enorm(:,j) = eigenmat(:,j)**2.d0
       do i=1,nres
         myut(1:3,j) = myut(1:3,j) + myu(1:3,j) * eigenmat(i,j)
       enddo
       myunorm(j) = sqrt(dot_product(myut(1:3,j),myut(1:3,j)))
     enddo
!
!!! Calc Radius of Claster.
!
     do j=1,nres
         Rmean = 0.d0 ; Rsum =  0.d0 ; Rtmp =  0.d0
         do i=1,nres
             v = com(:,i,j) - rtmp ; v = v - box(:) * anint(v * rcpbox)
             rmean = rmean + enorm(i,j) * (rtmp + v)
             rsum(j)  = rsum(j) + enorm(i,j)
             rtmp  = rmean / rsum(j)
         enddo
         rmean = rmean / rsum(j)
!
         rsum(j) = 0.d0
         do i=1,nres
           v = com(:,i,j) - rmean
           v = v - box * anint(v * rcpbox)
           rsum(j) = rsum(j) + enorm(i,j) * dot_product(v,v)
         enddo
         rsum(j) = sqrt(rsum(j))
     enddo
     call gyro%stack(rsum,rcpn)
     call diag%stack(eigen,rcpn)
!
    RcpKbt = 1.d0 / (300.d0 * 0.00198717d0) !Popration for kcal/mol
    EFLORS = 28590.7104d0 / 300.d0
!
     do i = 1,nres
       myu2 = rcpn*MyuNorm(i)**2.d0
       call osc%stack([eigen(i)],myu2)
       call fl%stack([eigen(i)],myu2*(Eigen(i)+EFLORS)**3.d0*exp((minval(Eigen,1)-Eigen(i))*RcpKbT))
     enddo
!
!       if(Sum(FlHist)>10E-5)FlHist = FlHist / Sum(FlHist)
!       if(Sum(OscHist)>10E-5)OscHist = OscHist / Sum(OscHist)
!
!       write(DevNesp,'(a)')"#OscSpectrum"
!       do j=Hmax,Hmin,-1
!           write(DevNesp,'(3x,2f16.8)')j*HSTGRD, OscHist(j)
!       enddo
!
!       write(DevNesp,'(/,a)')"#F2"
!       do j=Hmin,Hmax
!           write(DevNesp,'(3x,2f16.8)')j*HSTGRD, FlHist(j)
!       enddo
!
!       write(DevNesp,'(/,a)')"#FluorecentSpectrum"
!       write(DevNesp,'(a,f16.3)')"#Minimam lambda = ",28590.7104d0 / ( MinE + EFLORS )
!       do j=Hmax,Hmin,-1
!           write(DevNesp,'(3x,2f16.8)')28590.7104d0 / (j*HSTGRD + EFLORS), FlHist(j)
!       enddo
!
!       DiagHist = DiagHist / NTRJ
!
!       write(DevNesp,'(/,a)')"#DENSITY OF STATE"
!       do j=Hmin,Hmax
!           write(DevNesp,'(f16.10,2f16.9)') j*HSTGRD, DiagHist(j)
!       enddo
!
!       GyroHist = GyroHist / SUM(GyroHist)
!
!       write(DevNesp,'(/,a)')"#RADIAL OF CLASTER"
!       do j=0,GyroMax
!           write(DevNesp,'(f8.3,f16.9)') j*RGRD, GyroHist(j)
!       enddo
!
  end subroutine calc_elpot
!
  subroutine D_Solver(A,EV,N)
  double precision,intent(inout) :: A(N,N)
  double precision,intent(out) :: EV(N)
  integer(4),intent(in) :: N
  double precision,allocatable :: WorkSpace(:)
  integer(4) :: lda,lwork,liwork
  integer(4) :: info
    LWork=-1
    allocate(WorkSpace(1))
    WorkSpace=0
    CALL DSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
!
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL DSYEV('V','L',N,A,N,EV,WorkSpace,LWork,Info)
    RETURN
  end subroutine D_Solver
!
  subroutine PrintCounter(ntrj,counter,progres)
  integer,intent(in)  :: ntrj,counter
  integer,intent(out) :: progres
  integer             :: i,tmp
    tmp = NINT(real(40*counter)/real(ntrj))
    do i=progres+1,tmp
      write(stdout,'(a)',advance='no') '>'
      flush(stdout)
    enddo
    progres = tmp
  end subroutine PrintCounter
end program sdf
