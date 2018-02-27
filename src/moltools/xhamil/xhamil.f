include "mkl_vsl.fi"
program xhamil
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
double precision,parameter   :: AU2DEBYE = 4.803204544d0
double precision,parameter   :: AU2KCM   = 332.0522d0
double precision,parameter   :: KCM2NM   = 28590.7104d0
  call compute()
contains
  subroutine compute()
  use spur_optparse
  use moltypes
  use moltypes_export
  use spur_histgram
  use spur_vector
  use spur_stdio
  use spur_string
  type(moltype)                :: mt
  type(optparse)               :: arg
  type(histgram_1d)            :: stat,os,fl
  type(histgram_3d)            :: gy
  type(stdio)                  :: dat
  double precision,allocatable :: crd(:,:,:),vel(:,:,:)
  double precision,allocatable :: chg(:),cmat(:,:)
  double precision,allocatable :: elpot(:,:),eigen(:,:)
  double precision,allocatable :: enorm(:,:),ev(:)
  double precision,allocatable :: dpole(:,:),com(:,:)
  character(4),allocatable     :: aname(:)
  character(:),allocatable     :: logdir
  double precision             :: time1,time2
  double precision             :: FCONST
  double precision             :: beta,mono,MinE
  double precision             :: omega,fpop,fspct
  double precision             :: myu(3),osc,myn
  double precision             :: box(3),rcpbox(3),volume
  double precision             :: ntot,revm,revr
  double precision             :: cc(3),rc
  integer                      :: natm,nres,ntrj,nmol
  integer                      :: maxiter,iter
  integer                      :: i,j,k
  integer                      :: ethr
    call SetupOptargs(arg)
    time1 = omp_get_wtime()
!
    call mt%fetch(arg%args())
    if(arg%option('-m')) call mt%atomselect(arg%optargs('-m',1))
!
    natm = mt%natoms() ; nres = mt%nresidues() ; ntrj = mt%nframes()
    if(CheckAbort(dat,natm<=0,'natom is zero.')) RETURN
    if(CheckAbort(dat,nres<=0,'nres is zero.'))  RETURN
    if(CheckAbort(dat,ntrj<=0,'ntrj is zero.'))  RETURN
!
    nmol = natm / nres
    revm = 1.d0 / nmol
    revr = 1.d0 / nres
!
    allocate(crd(3,nmol,nres),vel(3,nmol,nres))
    allocate(chg(nmol),cmat(nmol,nmol))
    allocate(elpot(nres,nres),eigen(nres,nres))
    allocate(enorm(nres,nres),ev(nres))
    allocate(dpole(3,nres),com(3,nres))
    allocate(aname(nres))
    allocate(character(0)::logdir)
!
    aname = pack(mt%inq('name','XX  '),mt%inq('resid',0)==minval(mt%inq('resid',0),1))
    call loadchg(nmol,arg%optargs('-c',1),aname,chg)
    do j=1,nmol
      do i=1,nmol
        cmat(i,j) = chg(i) * chg(j) * AU2KCM       ! kcal/mol
      enddo
    enddo
!
    logdir = trim(arg%optargs('-o',1))
    call dat%fetch(logdir//'.dat')
    call dat%generate()
!
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('.........................xhamil.dat.............................')
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('|input_files')
    if(arg%option('-c')) call dat%puts('charge file          << ['//trim(arg%optargs('-c',1))//']')
    do i=1,arg%narg()
      write(dat%devn(),'(a,i3.3,3a)') 'trajectory file(',i,') << [',trim(arg%args(i)),']'
    enddo
    call dat%break()
    call dat%puts('|output_files')
    call dat%puts('DENCITY OF STATE and FLUORECENCE    >> ['//logdir//'.log'//']')
    call dat%puts('RADIUS OF GYRATION and OSCILATOR    >> ['//logdir//'.radii'//']')
    call dat%break()
    call dat%puts('|Parameters info.')
    write(dat%devn(),'(a,f9.3,a)') 'TEMPERATURE         : [',arg%optargd('-T',1),'] K'
    write(dat%devn(),'(a,f9.3,a)') 'MONOMER EMISSION    : [',arg%optargd('-f',1),'] nm'
    if(arg%option('-s')) write(dat%devn(),'(a,f9.3,a)') 'WRITEN STATES       : [',arg%optargi('-s',1),'] States'
    call dat%break()
    call dat%puts('|Topology info.')
    write(dat%devn(),'(a,i4,a,i6)') 'atom/res : ',nmol,' residue : ',nres
    call dat%puts('--------------------')
    call dat%puts('Atom     charges[AU]')
    call dat%puts('--------------------')
    do i=1,nmol
      write(dat%devn(),'(a4,f16.9)'),aname(i),chg(i)
    enddo
    call dat%puts('--------------------')
    write(dat%devn(),'(a,f16.9)'),'SUM ',SUM(chg)
    call dat%break()
    call dat%puts('Charge matrix [angs.*kcal/mol]') ; call PrintTriangle(dat%devn(),nmol,cmat)
    call dat%break()
!
    call stat%setup(-arg%optargd('-E',1),arg%optargd('-E',1),arg%optargd('-e',1))
    call os%setup(-arg%optargd('-E',1),arg%optargd('-E',1),arg%optargd('-e',1))
    call fl%setup(-arg%optargd('-E',1),arg%optargd('-E',1),arg%optargd('-e',1))
    call gy%setup(hmin1=1.d0, hmax1=arg%optargd('-R',1),grid1=arg%optargd('-r',1),&
                   &hmin2=0.d0, hmax2=arg%optargd('-D',1),grid2=arg%optargd('-d',1),&
                   &hmin3=-arg%optargd('-E',1),hmax3=arg%optargd('-E',1),grid3=arg%optargd('-e',1))
!
    maxiter = nres
    ntot    = 0.d0
    volume  = 0.d0
    beta  = 1.d0 / (arg%optargd('-T',1) * 0.00198717d0) ! Popration for kcal/mol
    mono =KCM2NM / arg%optargd('-f',1)                  ! monomer emission nm for kcal/mol
    FCONST = 1.d0 / (mono*mono*mono)  !temporary constant, check the value!
!
    do k=1,ntrj
      call mt%load(lb=k,ub=k)
      crd = dble(reshape(mt%xyz(),[3,nmol,nres]))
      box = dble(reshape(mt%box(),[3]))
      rcpbox = 1.d0/box
!
      call calc_elpot(natm,nmol,nres,crd,box,chg,cmat,revm,elpot,dpole,com)
!
      eigen = elpot
      call d_solver(eigen,ev,nres)
      enorm = eigen * eigen
      MinE  = minval(ev,1)
      call stat%stack(ev,1.d0)
!
      if(arg%option('--verbose')) call PrintVerbose(dat,nres,dpole,com,elpot,eigen,enorm,ev)
!
      call dat%puts('==================== SUMMARY OF STRUCTURE '//Tostr(k)//' ====================')
      call dat%puts('   S[kcal/mol] W.Len[nm] F-Cfactor Fluorecent D.Mom[D]R.Clu[Ang]  itr')
      call dat%puts('---------------------------------------------------------------------')
!
      vel = 0.d0
!
      do j=1,nres
        call RadialOfCluster(3,nres,com,enorm(:,j),box,rcpbox,rc,cc,iter,maxiter)
        myu  = calc_oscilator(nmol,nres,crd,chg,eigen(:,j))
        osc  = dot_product(myu,myu)
        myn  = AU2DEBYE*sqrt(osc)
        omega = KCM2NM/(mono + ev(j))
!
        fpop  = exp((MinE-ev(j))*beta)
        fspct = FCONST*osc*omega*omega*omega*fpop
!
        call os%stack(ev(j),osc)
        call fl%stack(ev(j),fspct)
        call gy%stack([rc,myn,ev(j)],fspct)
        write(dat%devn(),'(i4,6f10.3,x,i4)') j,ev(j),omega,fpop,fspct,myn,rc,iter
!
        do i=1,nres
          vel(1,1,i) = vel(1,1,i) + fspct * enorm(i,j)
        enddo
      enddo
!
      call dat%puts('=====================================================================')
      call dat%break()
!
      do j=1,nres
        do i=2,nmol
          vel(:,i,j) = vel(:,1,j)
        enddo
      enddo
      call ExportRST7(logdir//'/'//Str_pad(k,5)//'.rst7',natm,reshape(real(crd),[3,natm]),real(box),&
         &            [90.0,90.0,90.0],reshape(real(vel),[3,natm]),real(k))
    enddo
!
    call gy%scale() ; call stat%scale() ; call os%scale() ; call fl%scale()
    call HistExport(trim(arg%optargs('-o',1))//'/'//'x.log',fl%size(),fl%range(),stat%hvalue(),os%hvalue(),fl%hvalue(),mono)
    call gy%export(trim(arg%optargs('-o',1))//'/'//'xradii.log')
!
    time2 = omp_get_wtime()
    write(stdout,'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
    call dat%puts('=====================================================================')
    call dat%puts('-----------------------   END OF CALUCLATION   ----------------------')
    write(dat%devn(),'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
    call dat%puts('---------------------------------------------------------------------')
    call dat%puts('=====================================================================')
    call dat%quit()
  end subroutine compute
!
  subroutine SetupOptargs(arg)
  use spur_optparse
  type(optparse),intent(inout)   :: arg
    call arg%add_description('Caluclate excitonic interaction and radial of clusters.')
    call arg%add_option("-c",narg=1,metavar='charge',help='charge file.')
    call arg%add_option("-m",narg=1,metavar='string',help='Loading mask string.')
    call arg%add_option("-V",narg=0,help='Verbose mode.')
    call arg%add_option("-C",narg=0,help='Check mode.')
!
    call arg%add_option("-o",narg=1,metavar='filename',def=['xhamil'],help='dencity of state output')
!
    call arg%add_option("-e",narg=1,metavar='value',def=['1.0'], help='energy bin size. default [1.0] kcal/mol')
    call arg%add_option("-E",narg=1,metavar='value',def=['20.0'],help='histgram maxval. default [20.0] kcal/mol')
    call arg%add_option("-d",narg=1,metavar='value',def=['1.0'], help='dipole bin size. default [1.0] kcal/mol')
    call arg%add_option("-D",narg=1,metavar='value',def=['50.0'],help='dipole maxval.   default [20.0] debye.')
    call arg%add_option("-r",narg=1,metavar='value',def=['1.0'], help='radial bin size. default [1.0] kcal/mol')
    call arg%add_option("-R",narg=1,metavar='value',def=['50.0'],help='radial maxval.   default [50.0] angs.')
!
    call arg%add_option("-T",narg=1,metavar='value',def=['300.0'],help='Temperature.      default [300.0] K')
    call arg%add_option("-f",narg=1,metavar='value',def=['400.0'],help='monomer emission. default [400.0] nm')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
  end subroutine SetupOptargs
!
  subroutine loadchg(nmol,chgin,atmnam,chg)
  use spur_stdio
  use spur_vector
  use spur_string
  integer,intent(in)           :: nmol
  character(*),intent(in)      :: chgin,atmnam(nmol)
  double precision,intent(out) :: chg(nmol)
  type(stdio)                  :: io
  type(vector_character)       :: atm,tmp
  integer                      :: res(nmol),i,j,k
    chg = 0.d0
    atm = atmnam
    call io%fetch(chgin)
    if(io%isnotReadable()) RETURN
    call io%load()
    do i=1,io%nlines()
      call tmp%split(io%gets())
      if(tmp%size()<2) CYCLE
      j = atm%find(tmp%at(1))
      if(j>0) chg(j) = ToNum(tmp%at(2),0.d0)
      call tmp%clear()
    enddo
    call io%clear()
  end subroutine loadchg
!
  subroutine calc_elpot(natm,nmol,nres,crd,box,chg,cmat,revm,elpot,dpole,com)
  integer,intent(in)             :: natm,nmol,nres
  double precision,intent(in)    :: crd(3,nmol,nres),box(3)
  double precision,intent(in)    :: chg(nmol),cmat(nmol,nmol),revm
  double precision,intent(out)   :: elpot(nres,nres),dpole(3,nres),com(3,nres)
  double precision               :: rcpbox(3),ofs(3),v(3)
  integer                        :: i,j,k,l
    com = 0.d0
    do j=1,nres
      do i=1,nmol
        com(:,j) = com(:,j) + crd(:,i,j)
      enddo
    enddo
    com = com * revm
!
    elpot  = 0.d0
    rcpbox = 1.d0 / box
    do l=1,nres
      do k=l+1,nres
        ofs = com(:,k) - com(:,l) ; ofs = box * dnint(ofs*rcpbox)
        do j=1,nmol
          do i=1,nmol
            v = crd(:,i,k) - crd(:,j,l) - ofs
            elpot(k,l) = elpot(k,l) + cmat(i,j) / sqrt(dot_product(v,v))
          enddo
        enddo
      enddo
    enddo
!
    dpole = 0.d0
    do j=1,nres
      do i=1,nmol
        dpole(:,j) = dpole(:,j) + chg(i) * (crd(:,i,j) - com(:,j))
      enddo
   enddo
  end subroutine calc_elpot
!
  subroutine RadialOfCluster(d,n,x,en,box,rbox,rc,cc,iter,maxiter)
  integer,intent(in)          :: d,n,maxiter
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
    cc = mean_position(d,n,x,xcom,en,box,rbox,iter,maxiter)
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
  recursive function mean_position(d,n,x,xcom,enorm,box,rbox,iter,maxiter) result(res)
  integer,intent(in)          :: d,n,maxiter
  double precision,intent(in) :: x(d,n),xcom(d),enorm(n)
  double precision,intent(in) :: box(d),rbox(d)
  integer,intent(inout)       :: iter
  double precision            :: res(d)
  double precision            :: com(d),dif(d),v(d)
  integer                     :: i
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
      res = mean_position(d,n,x,com,enorm,box,rbox,iter,maxiter)
    endif
  end function mean_position
!
  pure function calc_oscilator(nmol,nres,crd,chg,eigen) result(res)
  integer,intent(in)          :: nmol,nres
  double precision,intent(in) :: crd(3,nmol,nres),chg(nmol),eigen(nmol)
  double precision            :: res(3)
  integer                     :: i,j
    res = 0.d0
    do j=1,nres
      do i=1,nmol
        res = res + chg(i) * eigen(j) * crd(:,i,j)
      enddo
    enddo
  end function calc_oscilator
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
  use spur_stdio
  type(stdio),intent(inout)   :: dat
  integer,intent(in)          :: natm,nres
  character(4),intent(in)     :: aname(natm)
  double precision,intent(in) :: chg(natm),cmat(natm,natm)
  integer                     :: i
  end subroutine PrintHeader
!
  subroutine PrintVerbose(dat,nres,dpole,com,elpot,eigen,enorm,ev)
  use spur_stdio
  type(stdio),intent(inout)   :: dat
  integer,intent(in)          :: nres
  double precision,intent(in) :: dpole(3,nres),com(3,nres)
  double precision,intent(in) :: elpot(nres,nres),eigen(nres,nres),enorm(nres,nres),ev(nres)
  integer                     :: i
    call dat%puts('====================  Start verbose output   ===================')
    call dat%puts('|Monomer dipole')
    call dat%puts('----------------------------------------------------------------')
    call dat%puts('Resid     Dipole Moment [D]       /D/    Center of Mass [Ang]   ')
    call dat%puts('----------------------------------------------------------------')
    do i=1,nres
      write(dat%devn(),'(i5,4f8.2,3f9.3)') i,AU2DEBYE*dpole(:,i),AU2DEBYE*sqrt(dot_product(dpole(:,i),dpole(:,i))),com(:,i)
    enddo
    call dat%puts('----------------------------------------------------------------')
    call dat%break()
    call dat%puts('|Electro static potential') ; call PrintTriangle(dat%devn(),nres,elpot)    ; call dat%break()
    call dat%puts('|Eigen Matrix')             ; call PrintMatrix(dat%devn(),nres,nres,eigen) ; call dat%break()
    call dat%puts('|Eigen Norm')               ; call PrintMatrix(dat%devn(),nres,nres,enorm) ; call dat%break()
    call dat%puts('|Eigen value')              ; call PrintVector(dat%devn(),nres,ev)         ; call dat%break()
    call dat%puts('====================   End verbose output   ====================')
    call dat%break()
  end subroutine PrintVerbose
!
  subroutine HistExport(f,n,hrange,st,os,fl,mono)
  use spur_stdio
  character(*),intent(in)     :: f
  integer,intent(in)          :: n
  double precision,intent(in) :: hrange(n),st(n),os(n),fl(n),mono
  type(stdio)                 :: fout
  integer                     :: i
    call fout%fetch(f) ; call fout%generate()
    do i=1,n
      write(fout%devn(),'(2f9.3,3f16.9)') hrange(i),KCM2NM/(mono+hrange(i)),st(i),os(i),fl(i)
    enddo
  end subroutine HistExport
!
  subroutine PrintTriangle(devn,N,X)
  integer,intent(in)          :: devn,N
  double precision,intent(in) :: X(N,N)
  integer                     :: i,j,k,step
  character(8)                :: ffmt
    step = 0
    do j=1,N,10
      step = minval([step+10,N],1)
      write(ffmt,'(i0)') step - j + 1
      write(devn,'(a5,'//trim(ffmt)//'i8)')'Resid',[(k,k=j,step)]
      do i=1,step
        write(devn,'(i5)',advance='no') i
        do k=j,i-1
          write(devn,'(a8)',advance='no') '  ------'
        enddo
        do k=maxval([j,i],1),step
          write(devn,'(f8.3)',advance='no') X(k,i)
        enddo
        write(devn,'(a)') ''
      enddo
      write(devn,'(a)') ''
    enddo
  end subroutine PrintTriangle
!
  subroutine PrintVector(devn,N,X)
  integer,intent(in)          :: devn,N
  double precision,intent(in) :: X(N)
  integer                     :: i,j,step
  character(8)                :: ffmt
    step = 0
    do i=1,N,10
      step = minval([step+10,N],1)
      write(ffmt,'(i0)') step - j + 1
      write(devn,'(a5,'//trim(ffmt)//'i8)')'     ',[(j,j=i,step)]
      write(devn,'(a5,'//trim(ffmt)//'f8.3)') '     ',X(i:step)
      write(devn,'(a)') ''
    enddo
  end subroutine PrintVector
!
  subroutine PrintMatrix(devn,N,M,X)
  integer,intent(in)          :: devn,N,M
  double precision,intent(in) :: X(N,M)
  integer                     :: i,j,k,step
  character(8)                :: ffmt
    step = 0
    do j=1,M,10
      step = minval([step+10,M],1)
      write(ffmt,'(i0)') step - j + 1
      write(devn,'(a5,'//trim(ffmt)//'i8)')'     ',[(k,k=j,step)]
      do i=1,N
        write(devn,'(i5,'//trim(ffmt)//'f8.3)') i,X(j:step,i)
      enddo
      write(devn,'(a)') ''
    enddo
  end subroutine PrintMatrix
!
  logical function CheckAbort(logf,test,errmsg) result(res)
  use spur_stdio
  type(stdio),intent(inout) :: logf
  logical,intent(in)        :: test
  character(*),intent(in)   :: errmsg
    res = test ; if(.not.res) RETURN
    call logf%puts(errmsg)
    call logf%puts('CALCLATION WAS TERMINATED ABNORMARY!')
  end function CheckAbort
end program xhamil
