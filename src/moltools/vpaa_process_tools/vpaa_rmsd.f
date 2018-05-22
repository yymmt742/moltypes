program vpaa_rmsd
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
real,parameter   :: Pi = 3.1415926535897932
real,parameter   :: AngRad = Pi/180.0
real,parameter   :: RadAng = 180.0/Pi
real,parameter   :: chkflp = 1.0
logical          :: xwrt
integer          :: istat
  istat = 0
  call compute_vpaa_rmsd()
  if(istat/=0) ERROR STOP
contains
  subroutine compute_vpaa_rmsd()
  use moltypes_parser
  use moltypes_export
  use spur_string
  use spur_histgram
  use spur_optparse
  use spur_stdio
  type(optparse)           :: arg
  type(molparser)          :: mt
  type(stdio)              :: dat,logf
  character(:),allocatable :: ifmt,nfmt,ncout
  integer                  :: natm,nmol,nres,ntrj,nswp,nflp,ncpy
  real,allocatable         :: X(:,:,:,:),FLPX(:,:,:,:),c0(:,:),c1(:,:,:)
  real,allocatable         :: timestep(:),tmprms(:),minrms(:),rms(:),rmscor(:,:)
  integer,allocatable      :: vswp(:,:),vflp(:),minidx(:)
  real                     :: revn,revt,rmse,rerr
  integer                  :: i,j,k,inc,progres
  integer                  :: ikey,nkey,iswp,iflp,ichiral,flpcnt
  double precision         :: time
    call arg%add_description('cluster analysys. trajectry must be 0 fixed.')
    call arg%add_option("-i",narg=1,metavar=['num'],def=['100'],help='n sampling. default [100]')
    call arg%add_option("-s",narg=1,metavar=['num'],def=['0.1'],help='time step size. default [0.1]')
    call arg%add_option("-m",narg=1,metavar=['string'],help='loading mask')
    call arg%add_option("-o",narg=1,metavar=['path'],help='output file name.')
    call arg%add_option("-dat",narg=1,metavar=['path'],def=['rmsd.dat'],help='dat file name.')
    call arg%add_option("-flp",narg=1,metavar=['path'],help='c2v molecule flip list')
    call arg%add_option("-x",narg=1,metavar=['index'],def=['0'],help='fiting frame for trajout')
    call arg%add_option("--silent",help='run silent mode')
    call arg%add_option("--noswp", help='run without swaping')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
!
    time  = omp_get_wtime()
!
    if(arg%option('-o'))then
      call logf%fetch(trim(arg%optargs('-o',1))) ; call logf%generate()
    endif
!
    call mt%fetch(arg%args())
!
    if(arg%option('-m')) call mt%atomselect(arg%optargs('-m',1))
!
    natm = mt%nfetchatoms() ; nres = mt%nresidues() ; ntrj = mt%nfetchframes()
!
    if(CheckAbort(logf,natm<1,1,'natom is zero.')) RETURN
    if(CheckAbort(logf,nres<1,1,'nres is zero.'))  RETURN
    if(CheckAbort(logf,ntrj<1,1,'ntrj is zero.'))  RETURN
!
    nmol = natm / nres
    revn = 1.0 / natm
    revt = 1.0 / ntrj
    nswp = factorial(nres)
!
    nkey = minval([arg%optargi('-i',1),ntrj],1) ; if(nkey<=0) nkey = ntrj
    inc  = ntrj / nkey
!
    nflp = 1 ; if(arg%option('-flp')) nflp = 2**nres
    ncpy = 2*nflp*nswp
!
    allocate(character(0) :: ifmt,nfmt)
    ifmt  = 'I'//ToStr(digit(nres)+2)
    nfmt  = 'I'//ToStr(digit(nkey))
!
    call PrintHeader(logf,arg%args(),natm,nres,ntrj,nflp,nswp,ncpy,nkey)
!
    allocate(vswp(nres,nswp),vflp(nmol))
    vswp = permutation(nres,nres,nswp,[(i,i=1,nres)])
!
    call logf%puts('* >> CONSTRUCTING SWAPING MATRIX')
    do j=1,nswp
      write(logf%devn(),'(A,I6,A,I0)',err=100,advance='no') '| ',j,'  [',vswp(1,j)
      do i=2,nres
        write(logf%devn(),'(A,I0)',err=100,advance='no') ',',vswp(i,j)
      enddo
      write(logf%devn(),'(A)',err=100) ']'
    enddo
    call logf%break()
!
    allocate(X(3,nmol,nres,ntrj),FLPX(3,nmol,nres,ntrj))
    allocate(c0(3,natm),c1(3,natm,ncpy))
    allocate(timestep(ntrj),rms(ntrj),minrms(ntrj))
    allocate(rmscor(ntrj,3),tmprms(ncpy))
    allocate(minidx(ntrj))
!
    timestep = real([(i,i=1,ntrj)]) * arg%optargf('-s',1)
    rmscor   = 0.0
!
    vflp = load_vflp(logf,nmol,arg%optargs('-flp',1),mt%inq('name','XX  '))
    call mt%load() ; call mt%centering_coordinates()
!
    X = reshape(mt%xyz(:,:,:ntrj),[3,nmol,nres,ntrj]) ; call mt%clear()
    FLPX  = mol_flip(nmol,nres,ntrj,vflp,X)
!
    call logf%puts('* >> CHECK FLIPING VECTOR')
!
    rmse = 0.0
    rms  = HUGE(0.0)
    do j=1,ntrj
      rerr = 0.0
      do i=1,nres
        rerr = checkflip(nmol,vflp,X(:,:,i,j),FLPX(:,:,i,j))
        if(rerr<rms(j)) rms(j) = rerr
        rmse = rmse + rerr
      enddo
    enddo
!
    rerr = minval(rms,1)
    rmse = rmse / real(nres*ntrj)
    write(logf%devn(),'(2(A,F9.3))',err=100) '|AVERAGE RMSD = ',rmse,' MINIMAM RMSD = ',rerr
!
    if(rerr>=chkflp)then
      call logf%break()
      call logf%puts('CAUTION :: Too large C2V frip rms error (check '//trim(arg%optargs('-flp',1))//')')
    endif
    call logf%break()
!
    call dat%fetch(trim(arg%optargs('-dat',1))) ; call dat%generate()
    call dat%puts('#     [TIME]       [KEY]  [MIN RMSD]      [RMSD]      [DIFF]   ['//Join([(i,i=1,nres)],'] [',digit(nres))//'] [CHI]')
!
    i = index(dat%is(),'.',.TRUE.)
    if(i==0) i = len_trim(dat%is()) + 1
    allocate(character(i-1) :: ncout)
    ncout(:) = dat%is() ; ncout = trim(ncout)//'.nc'
!
    call logf%puts('* >> MINIMAM RMSD CALCULATION')
    write(logf%devn(),'(A,'//nfmt//',A,'//nfmt//')',err=100,advance='no') '| ',0,'/',nkey
    call logf%puts('   [TIME]   [RMSE]   [NFLIP] [ % ]')
!
    ikey = 0
!
    do
      ikey = ikey + 1
      k    = nint(real(ntrj*(ikey-1))/real(nkey)) + 1
      c0   = reshape(X(:,:,:,k),[3,natm])
!
      do j=1,ntrj
        c1  = residue_swap(natm,nmol,nres,nswp,nflp,vswp,ncpy,X(:,:,:,j),FLPX(:,:,:,j))
        do i=1,ncpy
          call fit_rmsd(natm,c0,c1(:,:,i))
          tmprms(i) = calc_rmsd(natm,c0,c1(:,:,i),revn)
        enddo
        minidx(j) = minloc(tmprms,1)
        minrms(j) = tmprms(minidx(j))
        rms(j)    = tmprms(1)
      enddo
!
      rmse    = 0.0
      do j=1,ntrj
        rerr = rms(j) - minrms(j)
        rmse = rmse + rerr
        write(dat%devn(),'(5F12.3,A)',err=100,advance='no') timestep(j),timestep(k),minrms(j),rms(j),rerr,'  '
!
        ichiral = int((minidx(j)+1)/2)
        iflp    = int((ichiral-1)/nswp)
        iswp    = ichiral - iflp*nswp ; if(iswp==0) iswp = nswp
        ichiral = minidx(j) - ichiral * 2
        do i=1,nres
          if(btest(iflp,i-1))then
            write(dat%devn(),'('//ifmt//',A)',err=100,advance='no') vswp(i,iswp),'*'
          else
            write(dat%devn(),'('//ifmt//',A)',err=100,advance='no') vswp(i,iswp),' '
          endif
        enddo
        if(ichiral<0)then
          write(dat%devn(),'(A)',err=100) ' FALSE'
        else
          write(dat%devn(),'(A)',err=100) '  TRUE'
        endif
      enddo
      call dat%break()
!
      j = ntrj-k+1
      rmscor(1:j,1) = rmscor(1:j,1) + rms(k:ntrj)
      rmscor(1:j,2) = rmscor(1:j,2) + minrms(k:ntrj)
      rmscor(1:j,3) = rmscor(1:j,3) + 1.0
!
      if(ikey==arg%optargi('-x',1))then
        call GenerateAmberNetcdf(ncout,natm,xyz=.true.)
        do j=1,ntrj
          call ExportAmberNetcdf(ncout,natm,1,c1(:,:,minidx(j):minidx(j)))
        enddo
      endif
!
      flpcnt = count(minidx/=1)
      write(logf%devn(),'(2X,'//nfmt//',A,'//nfmt//',2F9.3,2X,I8,F6.1)',err=100)&
     &          ikey,'/',nkey,timestep(k),rmse*revt,flpcnt,real(100*flpcnt)*revt
!
      if(ikey>=nkey) EXIT
    enddo
!
    call logf%break()
    call logf%puts('* >> RMSD TIME CORRELATION (Angs.)')
    call logf%puts('|        [TIME]   [CORR]  [MINIM] [RMSD(K=1)] [RADIUS]')
    do i=1,ntrj
      c0 = reshape(X(:,:,:,i),[3,natm])
      call fit_rmsd(natm,reshape(X(:,:,:,1),[3,natm]),c0)
      if(rmscor(i,3)>0.5) rmscor(i,1:2) = rmscor(i,1:2) / rmscor(i,3)
      write(logf%devn(),'(i6,3F9.3,F12.3,F9.3)',err=100) i,timestep(i),rmscor(i,1),rmscor(i,2),&
                                                       & calc_rmsd(natm,c0,reshape(X(:,:,:,1),[3,natm]),revn),&
                                                       & calc_rog(natm,c0,revn)
    enddo
!
    call logf%break()
    call logf%puts('------------------------------------------------------------')
    call logf%puts('---                  END OF CALCULATION                  ---')
    write(logf%devn(),'(a,f11.3,a)') 'Total calculation time :: ',omp_get_wtime()-time,' sec.'
    call logf%puts('============================================================')
    call logf%quit()
    call dat%quit()
    RETURN
100 CONTINUE
    if(CheckAbort(logf,.TRUE.,1,'OUTPUT ERROR')) RETURN
  end subroutine compute_vpaa_rmsd
!
  function load_vflp(logf,nmol,swpin,atmnam) result(res)
  use spur_stdio
  use spur_vector_chr
  type(stdio),intent(inout) :: logf
  integer,intent(in)        :: nmol
  character(*),intent(in)   :: swpin,atmnam(nmol)
  type(stdio)               :: sio
  type(vector_chr)          :: atm,swpatm
  integer                   :: res(nmol),i,j,k
    atm = atmnam
    res = [(i,i=1,nmol)]
    call sio%fetch(swpin)
    if(sio%isnotReadable())then
      call logf%puts('* >> C2V FLIP [FALSE]')
      call logf%break() ; RETURN
    endif
    call sio%load()
    do i=1,sio%nlines()
      call swpatm%split(sio%gets())
      if(swpatm%size()<2) CYCLE
      j = atm%find(swpatm%at(1))
      k = atm%find(swpatm%at(2))
      if(j>0.and.k>0)then
        res(j) = k ; res(k) = j
      endif
      call swpatm%clear()
    enddo
    call sio%clear()
    call logf%puts('* >> C2V FLIP [TRUE]')
    write(logf%devn(),'(A,I6,A)') '| natm in each molecule : [',nmol,']'
    do i=1,nmol
      write(logf%devn(),'(A,I6,A)') '| ',i,'  ['//atmnam(i)//'] >> ['//atmnam(res(i))//']'
    enddo
    call logf%break()
  end function load_vflp
!
  pure recursive integer function factorial(n)
  integer,intent(in) :: n
    if(n<=0)then ; factorial = 1
    else         ; factorial = n * factorial(n-1)
    endif
  end function factorial
!
  recursive function permutation(n,nx,fn,v) result(res)
  integer,intent(in) :: n,nx,fn,v(n)
  integer            :: i,j,k,fx,res(n,fn)
    if(fn<=1)then
      res(:,1) = v
    else
      fx  = fn/nx
      j  = 0 ; k = 0
      do i=nx,1,-1
        j  = k + 1 ; k = k + fx
        res(:,j:k) = permutation(n,nx-1,fx,[pack(v(:nx),v(:nx)/=v(i)),v(i),v(nx+1:)])
      enddo
    endif
  end function permutation
!
  real function checkflip(nmol,vflp,c,f) result(res)
  integer,intent(in) :: nmol,vflp(nmol)
  real,intent(in)    :: c(3,nmol),f(3,nmol)
  real               :: c0(3,nmol),c1(3,nmol)
    c0 = centering(nmol,c) ; c1 = centering(nmol,f)
    call fit_rmsd(nmol,c0,c1)
    res = calc_rmsd(nmol,c0,c1,1.0/real(nmol))
  end function checkflip
!
  pure function centering(natm,c) result(res)
  integer,intent(in) :: natm
  real,intent(in)    :: c(3,natm)
  real               :: com(3),res(3,natm)
  integer            :: i
    com  = 0.0
    do i=1,natm
      com = com + c(:,i)
    enddo
    com = com / real(natm)
    do i=1,natm
      res(:,i) = c(:,i) - com
    enddo
  end function centering
!
  subroutine fit_rmsd(natm,c0,c1)
  integer,intent(in) :: natm
  real,intent(in)    :: c0(3,natm)
  real,intent(inout) :: c1(3,natm)
  real               :: rot(3,3)
  integer            :: i
    rot  = rmsrot(natm,c1,c0)
    do concurrent (i=1:natm)
      c1(:,i) = matmul(rot,c1(:,i))
    enddo
  end subroutine fit_rmsd
!
  pure function calc_rmsd(natm,c0,c1,revn) result(res)
  integer,intent(in) :: natm
  real,intent(in)    :: c0(3,natm),c1(3,natm),revn
  real               :: res,r(3)
  integer            :: i
    res = 0.0
    do i=1,natm
      r   = c1(:,i) - c0(:,i)
      res = res + dot_product(r,r)
    enddo
    res = sqrt(res*revn)
  end function calc_rmsd
!
  pure function calc_rog(natm,c,revn) result(res)
  integer,intent(in) :: natm
  real,intent(in)    :: c(3,natm),revn
  real               :: rot(3,3),r(3),res
  integer            :: i
    res = 0.0
    do i=1,natm
      res = res + dot_product(c(:,i),c(:,i))
    enddo
    res = sqrt(res*revn)
  end function calc_rog
!
  pure function mol_flip(nmol,nres,ntrj,vflp,X) result(res)
  integer,intent(in) :: nmol,nres,ntrj,vflp(nmol)
  real,intent(in)    :: X(3,nmol,nres,ntrj)
  real               :: res(3,nmol,nres,ntrj)
  integer            :: i,j,k
    do concurrent(k=1:ntrj)
    do concurrent(j=1:nres)
    do concurrent(i=1:nmol)
      res(:,i,j,k) = X(:,vflp(i),j,k)
    enddo
    enddo
    enddo
  end function mol_flip
!
  pure function residue_swap(natm,nmol,nres,nswp,nflp,vswp,ncpy,X,FLPX) result(res)
  integer,intent(in) :: natm,nmol,nres,nswp,nflp,ncpy,vswp(nres,nswp)
  real,intent(in)    :: X(3,nmol,nres),FLPX(3,nmol,nres)
  real               :: res(3,natm,ncpy)
  integer            :: i,j,at1,at2,lb,ub
  integer(4)         :: k
    at1 = 1 ; at2 = 2
    do k=0,nflp-1
      do j=1,nswp
        lb = 0 ; ub = 0
        do i=1,nres
          lb = ub + 1 ; ub = ub + nmol
          if(btest(k,i-1))then
            res(:,lb:ub,at1) = FLPX(:,:,vswp(i,j))
          else
            res(:,lb:ub,at1) = X(:,:,vswp(i,j))
          endif
        enddo
!
        do i=1,natm
          res(1,i,at2) =  res(1,i,at1)
          res(2,i,at2) =  res(2,i,at1)
          res(3,i,at2) = -res(3,i,at1)
        enddo
        at1 = at1 + 2 ; at2 = at2 + 2
      enddo
    enddo
  end function residue_swap
!
  function rmsrot(natm,c1,c0) result(res)
  use spur_matcalc
  integer,intent(in) :: natm
  real,intent(in)    :: c1(3,natm),c0(3,natm)
  real               :: res(3,3)
  real               :: cvm(3,3),s(3),u(3,3),vt(3,3),d(3,3)
    cvm = cov(3,3,natm,c1,c0)
    call svd(cvm,3,s,u,vt)
    cvm = matmul(u,vt)
    res = matmul(u,reshape([1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,anint(det(cvm))],[3,3]))
    res = matmul(res,vt)
    res = transpose(res)
  end function rmsrot
!
  subroutine PrintHeader(logf,args,natm,nres,ntrj,nflp,nswp,ncpy,nkey)
  use spur_vector_chr
  use spur_stdio
  type(stdio),intent(inout) :: logf
  character(*),intent(in)   :: args(:)
  integer,intent(in)        :: natm,nres,ntrj,nflp,nswp,ncpy,nkey
  type(vector_chr)          :: wrap
    call logf%puts('============================================================')
    call logf%puts('---           SUMMARY OF VPAA_RMSD CALCULATION           ---')
    call logf%puts('============================================================')
    call logf%puts('* >> HERE IS INPUT FILES.')
    call wrap%push(args) ; call wrap%textwrap(60)
    call logf%puts(wrap%lookup())
    call logf%break()
    call logf%puts('* >> TRAJECTORY INFOMATION')
    write(logf%devn(),'(3(A,I6),A)') '| natom :: [',natm,'] / nres  :: [',nres, '] / nframe :: [',ntrj  ,']'
    write(logf%devn(),'(3(A,I6),A)') '| nswap :: [',nswp,'] / nflip :: [',nflp, '] / ncopy  :: [',ncpy  ,']'
    write(logf%devn(),'(A,I6,A)')    '| nkey  :: [',nkey,']'
    call logf%break()
  end subroutine PrintHeader
!
  logical function CheckAbort(logf,test,ierr,errmsg) result(res)
  use spur_stdio
  type(stdio),intent(inout) :: logf
  logical,intent(in)        :: test
  integer,intent(in)        :: ierr
  character(*),intent(in)   :: errmsg
    res = test ; if(.not.res) RETURN
    istat = ierr
    call logf%puts(errmsg)
    call logf%puts('CALCLATION WAS TERMINATED ABNORMARY!')
  end function CheckAbort
end program vpaa_rmsd
