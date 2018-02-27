include "mkl_vsl.fi"
program xhamil
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use spur_optparse
use moltypes
use spur_histgram
use spur_vector
use spur_io
use amberprmtop
implicit none
type(trajectory)             :: trj
type(optparse)               :: arg
type(fileio)                 :: dat
double precision,parameter   :: StdChgR    = 18.2223d0                  ! For Amber Unit
double precision,parameter   :: AU2DEBYE = 4.803204544d0
double precision,parameter   :: AU2KCM   = 332.0522d0
double precision,parameter   :: KCM2NM   = 28590.7104d0
double precision             :: FCONST
double precision             :: beta,mono
integer                      :: ethr
double precision             :: time1,time2
integer                      :: natm,nres,ntrj
double precision,allocatable :: chg(:),cmol(:),cmat(:,:),evsum(:)
type(vector_integer)         :: resid
character(4),allocatable     :: aname(:)
character(6)                 :: seq
integer(8)                   :: cost,vcost
integer                      :: i,j,progres
integer                      :: maxiter,is
!
  time1 = omp_get_wtime()
!
  call SetupOptargs(arg)
  call loadTop(trj,arg,natm,nres,chg,cmol,cmat,evsum,aname,resid)
!
  call PrintHeader(dat,natm,nres,aname,chg,cmat)
!
  call SystemSetup(arg)
!
  do i=1,arg%narg()
    write(stdout,'(a,i4,2a)') 'Loading trajectry',i,'... ',arg%args(i)
    call trj%load(arg%args(i),from=1) ; ntrj = trj%n_frames()
!
    call dat%puts('----------------------------------------------------------------')
    write(dat%devn(),'(a,i3.3,a)') 'Calculation for trajectory file ',i,' << ['//trim(arg%args(i))//']'
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('')
!
    call PrintProgresBar(ntrj)
    progres = 0
    do j=1,ntrj
      write(seq,'(i6.6)') j
      call compute(arg,natm,nres,reshape(trj%xyz(:,1:natm*nres,j),[3,natm,nres]),&
                 & chg,cmat,trj%box(:,j),trj%reciprocal_boxes(j),evsum)
      call PrintCounter(ntrj,j,progres)
    enddo
  enddo
!
  evsum = evsum / sqrt(sum(evsum*evsum))
  call WriteTop(arg,size(cmol),nres,cmol,evsum,resid%lookup())
!
  deallocate(chg,cmol,cmat,aname,evsum)
!
  time2 = omp_get_wtime()
  write(stdout,'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
  call dat%puts('=====================================================================')
  call dat%puts('-----------------------   END OF CALUCLATION   ----------------------')
  write(dat%devn(),'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
  call dat%puts('---------------------------------------------------------------------')
  call dat%puts('=====================================================================')
  call dat%quit()
!
contains
  subroutine SetupOptargs(arg)
  type(optparse),intent(inout)   :: arg
    call arg%add_description('Caluclate excitonic interaction and radial of clusters.')
    call arg%add_option("-p",narg=1,metavar='prmtop',help='amber prmtop.')
    call arg%add_option("-m",narg=1,metavar='string',help='Loading mask string.')
    call arg%add_option("-c",narg=1,metavar='charge',help='charge file.')
    call arg%add_option("-V",narg=0,help='Verbose mode.')
    call arg%add_option("-C",narg=0,help='Check mode.')
!
    call arg%add_option("-o",narg=1,metavar='filename',def=['xhamil'],help='dencity of state output')
!
    call arg%add_option("-T",narg=1,metavar='value',def=['300.0'],help='Temperature.      default [300.0] K')
    call arg%add_option("-f",narg=1,metavar='value',def=['400.0'],help='monomer emission. default [400.0] nm')
    call arg%parser()
    if(.not.arg%option('-p').or.arg%narg()==0) call arg%call_usage()
  end subroutine SetupOptargs
!
  subroutine loadTop(trj,arg,natm,nres,chg,cmol,cmat,evsum,aname,resid)
  type(trajectory),intent(inout)           :: trj
  type(optparse),intent(in)                :: arg
  integer,intent(out)                      :: natm,nres
  double precision,allocatable,intent(out) :: chg(:),cmol(:),cmat(:,:),evsum(:)
  character(4),allocatable,intent(out)     :: aname(:)
  type(vector_integer),intent(out)         :: resid
  type(trajectory)                         :: tmp
  character(16)                            :: tmpmsk
  integer                                  :: test
    call trj%load(arg%optargs('-p',1),mask=arg%optargs('-m',1))
    call trj%load(arg%optargs('-c',1))
    if(trj%n_atoms()<=0)then
      write(stdout,'(a)') 'No atoms in ['//trim(arg%optargs('-p',1))//']' ; call abort(1)
    endif
!
    resid = trj%uniq_residue_index()
!
    nres   = trj%n_residues()
    natm   = count(resid%at(1)==trj%residue_index())
    write(tmpmsk,'(a,i6)') 'residue ',resid%at(1)
!
    do i=2,nres
      test = count(resid%at(i)==trj%residue_index()) ; if(natm/=test) call abort(1)
    enddo
!
    allocate(chg(natm),cmat(natm,natm),aname(natm),evsum(nres))
!
    aname = pack(trj%atomnames(),trj%residue_index()==minval(trj%residue_index(),1))
    chg   = trj%charges(resid=resid%at(1))
    evsum = 0.d0
!
    do j=1,natm
      do i=1,natm
        cmat(i,j) = chg(i) * chg(j) * AU2KCM       ! kcal/mol
      enddo
    enddo
!
    call tmp%load(arg%optargs('-p',1),mask=tmpmsk) ; call tmp%load(arg%optargs('-c',1))
    allocate(cmol(tmp%n_atoms()))
    cmol  = tmp%charges(resid=resid%at(1))
  end subroutine loadTop
!
  subroutine SystemSetup(arg)
  type(optparse),intent(in)   :: arg
    maxiter = nres
    cost = 0
!
    beta  = 1.d0 / (arg%optargd('-T',1) * 0.00198717d0) ! Popration for kcal/mol
    mono =KCM2NM / arg%optargd('-f',1)                  ! monomer emission nm for kcal/mol
    ethr  = arg%optargi('-s',1)
    FCONST = 1.d0 / (mono*mono*mono)  !temporary constant, check the value!
    !FCONST = 0.00140676d0 / (mono*mono*mono)  !temporary constant, check the value!
  end subroutine SystemSetup
!
  subroutine compute(arg,natm,nres,crd,chg,cmat,box,rcpbox,evsum)
  type(optparse),intent(in)      :: arg
  integer,intent(in)             :: natm,nres
  double precision,intent(in)    :: crd(3,natm,nres)
  double precision,intent(in)    :: chg(natm),cmat(natm,natm)
  double precision,intent(in)    :: box(3),rcpbox(3)
  double precision,intent(inout) :: evsum(nres)
  double precision               :: v(3),ofs(3)
  double precision               :: elpot(nres,nres)
  double precision               :: eigen(nres,nres),ev(nres)
  double precision               :: dpole(3),myut(3),myu2,myun
  double precision               :: enorm(nres,nres),MinE,omega,fpop,fspct
  double precision               :: rc,cc(3),emax,x(3,nres)
  double precision               :: vel(3,natm,nres),theta(nres)
  type(fileio)                   :: rst
  character(6)                   :: state
  integer                        :: iter,is
  integer                        :: i,j,k,l
    x = 0.d0
    do j=1,nres
      do i=1,natm
        x(:,j) = x(:,j) + crd(:,i,j)
      enddo
    enddo
    x = x / dble(natm)
!
    elpot = 0.d0
    do l=1,nres
      do k=l+1,nres
        ofs = x(:,k) - x(:,l) ; ofs = box * dnint(ofs*rcpbox)
        do j=1,natm
          do i=1,natm
            v = crd(:,i,k) - crd(:,j,l) - ofs
            elpot(k,l) = elpot(k,l) + cmat(i,j) / sqrt(dot_product(v,v))
          enddo
        enddo
      enddo
    enddo
!
    eigen = elpot ; call d_solver(eigen,ev,nres)
    enorm = eigen * eigen
    MinE  = minval(ev,1)
!
    if(arg%option('-V'))then
      call dat%puts('')
      call dat%puts('====================  Start verbose output   ===================')
      call dat%puts('')
      call dat%puts('----------------------------------------------------------------')
      call dat%puts('Resid     Dipole Moment [D]       /D/      Center of Mass [Ang] ')
      call dat%puts('----------------------------------------------------------------')
      do j=1,nres
        dpole = 0.d0
        do i=1,natm
          dpole = dpole + chg(i) * (crd(:,i,j) - x(:,j))
        enddo
        write(dat%devn(),'(i5,4f8.2,3f9.3)') j,AU2DEBYE*dpole,AU2DEBYE*sqrt(dot_product(dpole,dpole)),x(:,j)
      enddo
      call dat%puts('')
      call dat%puts('|Electro static potential') ; call PrintTriangle(nres,elpot)
      call dat%puts('|Eigen Matrix')             ; call PrintMatrix(nres,nres,eigen)
      call dat%puts('|Eigen Norm')               ; call PrintMatrix(nres,nres,enorm)
      call dat%puts('|Eigen value')              ; call PrintVector(nres,ev)
      call dat%puts('====================   End verbose output   ====================')
      call dat%puts('')
      call dat%puts('---------------------------------------------------------------------')
    endif
!
    call dat%puts('====================== SUMMARY OF STRUCTURE  ========================')
    write(dat%devn(),'(a,3f8.2,a,f16.4,a)') 'box size [',box,']    volume [',box(1)*box(2)*box(3),']'
    call dat%puts('   S[kcal/mol] W.Len[nm] F-Cfactor Fluorecent D.Mom[D]R.Clu[Ang]  itr')
    call dat%puts('---------------------------------------------------------------------')
!
    vel = 0.d0
!
    do k=1,nres
      call RadialOfCluster(3,nres,x,enorm(:,k),box,rcpbox,rc,cc,iter)
!
      myut = 0.d0
      do j=1,nres
        do i=1,natm
          myut = myut + chg(i) * eigen(j,k) * crd(:,i,j)
        enddo
      enddo
      myu2  = dot_product(myut,myut)
      myun  = AU2DEBYE*sqrt(myu2)
      omega = KCM2NM/(mono + ev(k))
      fpop  = exp((MinE-ev(k))*beta)
      fspct = FCONST*myu2*omega*omega*omega*fpop
!
      write(dat%devn(),'(i4,6f10.3,x,i4)') k,ev(k),omega,fpop,fspct,myun,rc,iter
      evsum = evsum + eigen(:,k) * fpop
!
      if(ethr>k)CYCLE
      do j=1,nres
        vel(1,1,j) = vel(1,1,j) + fspct*enorm(j,k)
      enddo
    enddo
!
    call dat%puts('=====================================================================')
    call dat%puts('')
!
  end subroutine compute
!
  subroutine RadialOfCluster(d,n,x,en,box,rbox,rc,cc,iter)
  integer,intent(in)          :: d,n
  double precision,intent(in) :: x(d,n),en(n)
  double precision,intent(in) :: box(d),rbox(d)
  double precision,intent(out):: rc,cc(d)
  integer,intent(out)         :: iter
  double precision            :: xcom(d),v(d)
  integer                     :: i
    xcom = 0.d0
    do i=1,n
      xcom = xcom + x(:,i)
    enddo
    xcom  = xcom / dble(n)
!
    iter  = 0
    cc = mean_position(d,n,x,xcom,en,box,rbox,iter)
!
    rc  = 0.d0
!
    do i=1,n
      v =  x(:,i) - cc ; v = v - box * dnint(v * rbox)
      rc  = rc  + en(i) * dot_product(v,v)
    enddo
    rc = sqrt(rc)
  end subroutine RadialOfCluster
!
  recursive function mean_position(d,n,x,xcom,enorm,box,rbox,iter) result(res)
  integer,intent(in)          :: d,n
  double precision,intent(in) :: x(d,n),xcom(d),enorm(n)
  double precision,intent(in) :: box(d),rbox(d)
  integer,intent(inout)       :: iter
  double precision            :: res(d)
  double precision            :: com(d),dif(d),v(d)
    com = xcom
    do i=1,n
      v = x(:,i) - xcom ; v = v - box * dnint(v * rbox)
      com = com + v * enorm(i)
    enddo
    dif = dble(n) * (com - xcom)
    if(dot_product(dif,dif)<10E-5.or.iter>=maxiter)then
      res = com
    else
      iter = iter + 1
      res = mean_position(d,n,x,com,enorm,box,rbox,iter)
    endif
  end function
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
  subroutine PrintHeader(dat,natm,nres,aname,chg,cmat)
  type(fileio),intent(inout)  :: dat
  integer,intent(in)          :: natm,nres
  character(4),intent(in)     :: aname(natm)
  double precision,intent(in) :: chg(natm),cmat(natm,natm)
  integer                     :: i
    call dat%generate(trim(arg%optargs('-o',1))//'.dat')
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('.........................xhamil.dat.............................')
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('|input_files')
    call dat%puts('topology file        << ['//trim(arg%optargs('-p',1))//']')
    if(arg%option('-c')) call dat%puts('charge file          << ['//trim(arg%optargs('-c',1))//']')
    do i=1,arg%narg()
      write(dat%devn(),'(a,i3.3,3a)') 'trajectory file(',i,') << [',trim(arg%args(i)),']'
    enddo
    call dat%puts('')
    call dat%puts('|Parameters info.')
    write(dat%devn(),'(a,f9.3,a)') 'TEMPERATURE         : [',arg%optargd('-T',1),'] K'
    write(dat%devn(),'(a,f9.3,a)') 'MONOMER EMISSION    : [',arg%optargd('-f',1),'] nm'
    if(arg%option('-s')) write(dat%devn(),'(a,f9.3,a)') 'WRITEN STATES       : [',arg%optargi('-s',1),'] States'
    call dat%puts('')
    call dat%puts('|Topology info.')
    write(dat%devn(),'(a,i4,a,i6)') 'atom/res : ',natm,' residue : ',nres
    call dat%puts('--------------------')
    call dat%puts('Atom     charges[AU]')
    call dat%puts('--------------------')
    do i=1,natm
      write(dat%devn(),'(a4,f16.9)'),aname(i),chg(i)
    enddo
    call dat%puts('--------------------')
    write(dat%devn(),'(a,f16.9)'),'SUM ',SUM(chg)
    call dat%puts('')
    call dat%puts('Charge matrix [angs.*kcal/mol]') ; call PrintTriangle(natm,cmat)
  end subroutine PrintHeader
!
  subroutine WriteTop(arg,natm,nres,cmol,evsum,resid)
  type(optparse),intent(in)       :: arg
  integer,intent(in)              :: natm,nres
  double precision,intent(in)     :: cmol(natm),evsum(nres)
  integer,intent(in)              :: resid(nres)
  type(AmberPrmtop)               :: ambprm
  integer                         :: from,to
  integer                         :: i,j
    call ambprm%load(arg%optargs('-p',1))
    do i = 1,nres
      from = ambprm%IPRES(resid(i))
      if(resid(i)>=ambprm%NRES)then
        to = ambprm%NATOM
      else
        to = ambprm%IPRES(resid(i)+1)-1
      endif
      ambprm%CHARGE(from:to) = StdChgR * cmol * evsum(i)
      !ambprm%CHARGE(from:to) = ambprm%CHARGE(from:to) + StdChgR * cmol * evsum(i)
    enddo
!
    call ambprm%export(trim(arg%optargs('-o',1))//'.prmtop')
  end subroutine WriteTop
!
  subroutine PrintTriangle(N,X)
  integer,intent(in)          :: N
  double precision,intent(in) :: X(N,N)
  integer                     :: i,j,k,step
  character(8)                :: ffmt
    step = 0
    do j=1,N,10
      step = minval([step+10,N],1)
      write(ffmt,'(i0)') step - j + 1
      write(dat%devn(),'(a5,'//trim(ffmt)//'i8)')'Resid',[(k,k=j,step)]
      do i=1,step
        write(dat%devn(),'(i5)',advance='no') i
        do k=j,i-1
          write(dat%devn(),'(a8)',advance='no') '  ------'
        enddo
        do k=maxval([j,i],1),step
          write(dat%devn(),'(f8.3)',advance='no') X(k,i)
        enddo
        write(dat%devn(),'(a)') ''
      enddo
      write(dat%devn(),'(a)') ''
    enddo
  end subroutine PrintTriangle
!
  subroutine PrintVector(N,X)
  integer,intent(in)          :: N
  double precision,intent(in) :: X(N)
  integer                     :: i,j,step
  character(8)                :: ffmt
    step = 0
    do i=1,N,10
      step = minval([step+10,N],1)
      write(ffmt,'(i0)') step - j + 1
      write(dat%devn(),'(a5,'//trim(ffmt)//'i8)')'     ',[(j,j=i,step)]
      write(dat%devn(),'(a5,'//trim(ffmt)//'f8.3)') '     ',X(i:step)
      write(dat%devn(),'(a)') ''
    enddo
  end subroutine PrintVector
!
  subroutine PrintMatrix(N,M,X)
  integer,intent(in)          :: N,M
  double precision,intent(in) :: X(N,M)
  integer                     :: i,j,k,step
  character(8)                :: ffmt
    step = 0
    do j=1,M,10
      step = minval([step+10,M],1)
      write(ffmt,'(i0)') step - j + 1
      write(dat%devn(),'(a5,'//trim(ffmt)//'i8)')'     ',[(k,k=j,step)]
      do i=1,N
        write(dat%devn(),'(i5,'//trim(ffmt)//'f8.3)') i,X(j:step,i)
      enddo
      write(dat%devn(),'(a)') ''
    enddo
  end subroutine PrintMatrix
!
  subroutine PrintProgresBar(ntrj)
  integer,intent(in)    :: ntrj
    write(STDOUT,'(a,i0,a)') 'Processing trajectry for ',ntrj,' frames'
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
  subroutine Abort(n)
  integer,intent(in)    :: n
    write(STDOUT,'(a)') 'Calclation was terminated abnormaly.'
    call exit(n)
  end subroutine Abort
end program xhamil
