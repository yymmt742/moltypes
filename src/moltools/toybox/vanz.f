program vanz
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT, STDERR => ERROR_UNIT
use netcdf
use moltypes
use spur_optparse
implicit none
double precision,parameter :: Pi = 3.1415926535897932d0
double precision,parameter :: AngRad = Pi/180.d0
double precision,parameter :: RadAng = 180.d0/Pi
type(optparse)             :: arg
double precision           :: time1,time2
  call arg%add_description('process feature vector.')
  call arg%add_option("-eps",narg=1,metavar='value',help='eps.',def=['1.0'])
  call arg%add_option("-npt",narg=1,metavar='value',help='npt.',def=['10'])
  call arg%add_option("-fv",narg=1,metavar='filename',help='feature vector output file.',def=['fvector'])
  call arg%add_option("-log",narg=1,metavar='filename',help='vanz logfile.')
  call arg%parser()
!
  time1 = omp_get_wtime()
  call fv_analyze(arg%optargs('-fv',1),arg%optargd('-eps',1),arg%optargi('-npt',1),arg%optargs('-log',1))
  time2 = omp_get_wtime()
  write(stdout,'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
contains
  subroutine fv_analyze(fv,eps,npts,logf)
  use spur_vector
  character(*),intent(in)         :: fv,logf
  double precision,intent(in)     :: eps
  integer,intent(in)              :: npts
  integer                         :: ninp
  character(:),allocatable        :: inp,prm
  character(12)                   :: seq
  type(vector_character)          :: trajin
  integer,allocatable             :: label(:)
  double precision,allocatable    :: fvector(:,:)
  double precision,allocatable    :: fval(:,:),fgyro(:),gratio(:),fratio(:)
  integer                         :: ncid,varid,dimid
  integer                         :: d,n,nlabel,lenchr
  integer                         :: i,j,is
    is = nf90_open(fv,nf90_nowrite,ncid)
    if(is/=nf90_NoErr)then
      write(stderr,'(a,a,a)') 'feature vector file(',trim(fv),') was not found.' ; RETURN
    endif
    write(stdout,'(a,a,a)') 'Loading feature vector file(',fv,')'
    is = nf90_inq_dimid(ncid,"dimension",dimid)
    is = nf90_inquire_dimension(ncid,dimid,len=d)
    is = nf90_inq_dimid(ncid,"frame",dimid)
    is = nf90_inquire_dimension(ncid,dimid,len=n)
    is = nf90_inq_dimid(ncid,"ninp",dimid)
    is = nf90_inquire_dimension(ncid,dimid,len=ninp)
    is = nf90_inquire_attribute(ncid,nf90_global,"prmtop",len=lenchr)
    allocate(character(lenchr)::inp)
    is = nf90_get_att(ncid,nf90_global,"prmtop",inp)
    call trajin%push(inp)
    do i=1,ninp
      write(seq,'(i0)') i
      is = nf90_inquire_attribute(ncid,nf90_global,"input"//trim(seq),len=lenchr)
      deallocate(inp) ; allocate(character(lenchr)::inp)
      is = nf90_get_att(ncid,nf90_global,"input"//trim(seq),inp)
      call trajin%push(inp)
    enddo
    deallocate(inp)
!
    allocate(fvector(d,n),label(n))
!
    is = nf90_inq_varid(ncid,"fvector",varid)
    is = nf90_get_var(ncid,varid,fvector,start=[1,1],count=[d,n])
!
    call dbscan(d,n,fvector,eps,npts,label)
    is = nf90_close(ncid)
    nlabel = maxval(label,1)
!
    allocate(fval(d,nlabel),fgyro(nlabel),gratio(nlabel),fratio(nlabel))
!
    call calc_fvalue(d,n,nlabel,fvector,label,fval,fgyro,gratio,fratio)
!
    call output_clusters(ninp,trajin%lookup(),d,n,nlabel,label)
    call output_summary(logf,d,n,nlabel,eps,npts,fval,fgyro,gratio,fratio)
    deallocate(fvector,label,fval,fgyro,gratio,fratio)
  end subroutine fv_analyze
!
  subroutine dbscan(d,n,x,eps,npts,label)
  use spur_vector
  integer,intent(in)           :: d,n
  double precision,intent(in)  :: x(d,n)
  double precision,intent(in)  :: eps
  integer,intent(in)           :: npts
  integer,intent(inout)        :: label(n)
  double precision             :: eps2,v(d)
  logical                      :: neighbor(n),visited(n),c(n)
  type(vector_integer)         :: pairlist(n)
  integer                      :: i,j,k,nlabel,progres
    eps2 = eps * eps ; label = 0
!
    nlabel = 0 ; progres = 0
    c = .FALSE. ; visited = .FALSE.
    do j=1,n
      do i=j+1,n
        v = x(:,i) - x(:,j)
        if(dot_product(v,v)<=eps2)then
          call pairlist(j)%push(i)
          call pairlist(i)%push(j)
        endif
      enddo
    enddo
!
    call PrintProgresBar(n)
    do j=1,n
      call PrintCounter(n,j,progres)
      if(visited(j))CYCLE ; visited(j) = .TRUE.
      if(pairlist(j)%size()<=npts) CYCLE
      nlabel = nlabel + 1
      i = 0
      c = .FALSE.
      do i = 1,pairlist(j)%size()
        c(pairlist(j)%at(i)) = .TRUE.
      enddo
!
      do while(i<=n)
        i = i + 1
        if(.not.c(i).or.visited(i))CYCLE
        visited(i) = .TRUE.
        if(pairlist(i)%size()>npts)then
          do k = 1,pairlist(i)%size()
            c(pairlist(i)%at(k)) = .TRUE.
          enddo
          i = 0
        endif
      enddo
      do i=1,n
        if(c(i)) label(i) = nlabel
      enddo
    enddo
    call PrintCounter(n,n,progres)
  end subroutine dbscan
!
  subroutine calc_fvalue(d,n,nlabel,x,label,fval,fgyro,gratio,fratio)
  integer,intent(in)           :: d,n,nlabel
  double precision,intent(in)  :: x(d,n)
  integer,intent(in)           :: label(n)
  double precision,intent(out) :: fval(d,nlabel),fgyro(nlabel),gratio(nlabel),fratio(nlabel)
  integer                      :: nf(nlabel)
  double precision             :: v(d),r,gyro
  integer                      :: i,j
    fval = 0.d0 ; fgyro = 0.d0 ; nf = 0
    gyro = 0.d0
    do i=1,n
      if(label(i)==0)CYCLE
      nf(label(i)) = nf(label(i)) + 1
      fval(:,label(i)) = fval(:,label(i)) + x(:,i)
    enddo
    do i=1,nlabel
      fval(:,i) = fval(:,i) / dble(nf(i))
    enddo
    do i=1,n
      v = x(:,i) - fval(:,label(i))
      r = dot_product(v,v)
      gyro = gyro + r
      if(label(i)==0)CYCLE
      fgyro(label(i)) = fgyro(label(i)) + r
    enddo
    gyro = sqrt(dble(n)/gyro)
    do i=1,nlabel
      fgyro(i)  = sqrt(fgyro(i) / dble(nf(i)))
      gratio(i) = dble(100) * gyro * fgyro(i)
      fratio(i) = dble(100*nf(i)) / dble(n)
    enddo
  end subroutine calc_fvalue
!
  subroutine output_clusters(ninp,inp,ndim,ntrj,nlabel,label)
  integer,intent(in)           :: ninp
  character(*),intent(in)      :: inp(ninp+1)
  integer,intent(in)           :: ndim,ntrj,nlabel
  integer,intent(in)           :: label(ntrj)
  type(trajectory)             :: trj
  logical,allocatable          :: mask(:,:,:),tmp(:),mv(:)
  integer,allocatable          :: order(:)
  double precision,allocatable :: crd(:,:),com0(:,:),com(:,:),ctmp(:,:),r(:)
  double precision             :: rot(3,3)
  integer                      :: ncatm(ndim),ntmp
  integer                      :: ncid(nlabel),varid(nlabel),frame(nlabel)
  integer                      :: i,j,is
  character(4)                 :: di
    call trj%load(inp(1))
    do i=2,ninp+1
      write(stdout,'(a,a,a)') 'Loading coordinate ',inp(i),'...'
      call trj%load(inp(i),ub=-1)
    enddo
!
    allocate(mask(3,trj%n_atoms(),trj%n_residues()),tmp(trj%n_atoms()))
    allocate(crd(3,trj%n_atoms()))
    allocate(ctmp(3,trj%n_residues()),com0(3,trj%n_residues()),com(3,trj%n_residues()))
    allocate(order(trj%n_residues()),r(trj%n_residues()),mv(trj%n_residues()))
!
    do j=1,trj%n_residues()
      tmp = j==trj%residue_index()
      ncatm(j) = count(tmp)
      do i=1,trj%n_atoms()
        mask(:,i,j) = tmp(i)
      enddo
    enddo
!
    ctmp = trj%center_residue(1)
    do i=1,trj%n_residues()
      r(i) = dot_product(ctmp(:,i),ctmp(:,i))
    enddo
    mv = .TRUE.
    do i=1,trj%n_residues()
      order(i) = minloc(r,1,mask=mv) ; mv(order(i)) = .FALSE.
      com0(:,i) = ctmp(:,order(i))
    enddo
!
    do i=1,nlabel
      write(di,'(i4.4)')i
      call generateNC('fv'//di//'.nc',trj%n_atoms())
      is = nf90_open('fv'//di//'.nc',nf90_write,ncid(i))
      is = nf90_inq_varid(ncid(i),"coordinates",varid(i))
    enddo
!
    frame = 0
    do j=1,ntrj
      if(label(j)==0)CYCLE
!
      ctmp = trj%center_residue(j)
      do i=1,trj%n_residues()
        r(i) = dot_product(ctmp(:,i),ctmp(:,i))
      enddo
      mv = .TRUE.
      do i=1,trj%n_residues()
        order(i) = minloc(r,1,mask=mv) ; mv(order(i)) = .FALSE.
        com(:,i) = ctmp(:,order(i))
      enddo
      rot = rmsrot(trj%n_residues(),com,com0)
!
      ntmp = 0
      do i=1,trj%n_residues()
        crd(:,ntmp+1:ntmp+ncatm(order(i))) = reshape(pack(trj%coordinates(j),mask(:,:,order(i))),[3,ncatm(order(i))])
        ntmp = ntmp + ncatm(order(i))
      enddo
      do i=1,trj%n_atoms()
        crd(:,i) = matmul(rot,crd(:,i))
      enddo
      frame(label(j)) = frame(label(j)) + 1
      is = nf90_put_var(ncid(label(j)),varid(label(j)),crd,start=[1,1,frame(label(j))],count=[3,trj%n_atoms(),1])
    enddo
    do i=1,nlabel
      is = nf90_close(ncid(i))
    enddo
  end subroutine output_clusters
!
  subroutine output_summary(logf,ndim,npts,nlabel,eps,minpts,fval,fgyro,gratio,fratio)
  use spur_io
  character(*),intent(in)      :: logf
  integer,intent(in)           :: ndim,npts,nlabel,minpts
  double precision,intent(in)  :: eps
  double precision,intent(in)  :: fval(ndim,nlabel),fgyro(nlabel),gratio(nlabel),fratio(nlabel)
  double precision             :: fto
  type(fileio)                 :: flog
  character(8)                 :: cdim
  integer                      :: i,j
    write(cdim,'(i0,a,i0)') 4,'*',ndim/4 ; cdim = adjustr(cdim)
    fto = sum(fratio)
    call flog%generate(logf)
    write(flog%devn(),'(a)')           '====== SUMMARY OF DBSCAN CLUSTERING ======'
    write(flog%devn(),'(a)')           '* >> Here is clustering parameters.       '
    write(flog%devn(),'(a)')               ''
    write(flog%devn(),'(a,f8.5,a,i8)') 'epsilon   :: ',eps, ' / minPts :: ',minpts
    write(flog%devn(),'(a,a,a,i8)')    'dimension :: ',cdim,' / NumPts :: ',npts
    write(flog%devn(),'(a)') ''
    write(flog%devn(),'(a,i3,a)')      '* >>',nlabel,' cluster informations follow.      '
    write(flog%devn(),'(a)')               ''
    write(flog%devn(),'(a)')           'No.  probability        size  size_ratio  '
    do j=1,nlabel
      write(flog%devn(),'(i3.3,a,f7.3,a,f9.3,a,f7.3,a)')&
                                     & j,'     ',fratio(j),' %  ',fgyro(j),'   ',gratio(j),' %  '
    enddo
    write(flog%devn(),'(a,f7.3,a)')    'Total   ',fto,' %                         '
    write(flog%devn(),'(a)') ''
    write(flog%devn(),'(a,i3,a)')      '* >> More details of clusters             '
    do j=1,nlabel
      write(flog%devn(),'(a)')               ''
      write(flog%devn(),'(a,i3.3,a,f7.3,a)') '|Cluster No.',j,' / Existence   [',fratio(j),' %] '
      write(flog%devn(),'(a,f9.3,a,f7.3,a)') '| size',fgyro(j),' / size ratio is',gratio(j),' %  '
      write(flog%devn(),'(a)')               '|_________________________________________'
      write(flog%devn(),'(a)')               '|   (Angs.)  (angles) (angles) (angles)  |'
      do i=1,ndim,4
        write(flog%devn(),'(a,4f9.3,a)') '| ',fval(i,j),radang*acos(fval(i+1,j)/fval(i,j)),radang*acos(fval(i+2,j)/fval(i,j)),radang*acos(0.5d0*fval(i+3,j)),' '
      enddo
    enddo
    write(flog%devn(),'(a)')           '------------------------------------------'
    call flog%quit()
  end subroutine output_summary
!
  subroutine writefv(ncid,indid,varid,l2,fvlen,fvtht,fvfai)
  integer,intent(in)              :: ncid,indid,varid,l2
  double precision,intent(in)     :: fvlen(l2),fvtht(l2),fvfai(l2)
  integer,save                    :: frame = 0
  logical                         :: minl(l2)
  integer                         :: i,is,ml
    minl = .TRUE. ; frame = frame + 1
    do i=1,l2
      ml = minloc(fvlen,1,mask=minl)
      is = nf90_put_var(ncid,indid,[ml],start=[i,frame],count=[1,1])
      is = nf90_put_var(ncid,varid,[fvlen(ml),fvtht(ml),fvfai(ml)],start=[i*3-2,frame],count=[3,1])
      minl(ml) = .FALSE.
    enddo
  end subroutine writefv
!
  function rmsrot(natm,c1,c0) result(res)
  integer,intent(in)             :: natm
  double precision,intent(in)    :: c1(3,natm),c0(3,natm)
  double precision               :: res(3,3)
  double precision               :: cov(3,3),s(3),u(3,3),vt(3,3),d(3,3),det
    cov = covariance(3,natm,c1,c0)
    call DSVD(cov,3,s,u,vt)
    cov = matmul(u,vt)
    det = dnint(det3d(cov))
    res = matmul(u,reshape([1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,det],[3,3]))
    res = matmul(res,vt)
    res = transpose(res)
  end function rmsrot
!
  pure function covariance(d,n,X,Y) result(res)
  integer,intent(in)          :: d,n
  double precision,intent(in) :: X(d,n),Y(d,n)
  double precision            :: res(d,d)
  integer                     :: i,j,k
    res = 0.d0
    do k=1,n
      do j=1,d
        do i=1,d
          res(i,j) = res(i,j) + X(i,k)*Y(j,k)
        enddo
      enddo
    enddo
  end function covariance
!
  pure function det3d(X) result(res)
  double precision,intent(in) :: X(3,3)
  double precision            :: res
    res = X(1,1)*X(2,2)*X(3,3)+X(1,2)*X(2,3)*X(3,1)+X(1,3)*X(2,1)*X(3,2)&
        &-X(1,3)*X(2,2)*X(3,1)-X(1,2)*X(2,1)*X(3,3)-X(1,1)*X(2,3)*X(3,2)
  end function det3d
!
  subroutine DSVD(A,N,S,U,VT)
  integer,intent(in)             :: N
  double precision,intent(inout) :: A(N,N)
  double precision,intent(out)   :: S(N),U(N,N),VT(N,N)
  double precision,allocatable   :: WorkSpace(:)
  integer                        :: lwork,liwork
  integer                        :: info
    LWork=-1
    allocate(WorkSpace(lWork))
    WorkSpace=0
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
    LWork=Int(WorkSpace(1))
    deallocate(WorkSpace) ; allocate(WorkSpace(1:LWork))
    CALL DGESVD('S','S',N,N,A,N,S,U,N,VT,N,WorkSpace,LWork,Info)
  end subroutine DSVD
!
  subroutine generateNC(filename,natm)
  character(*),intent(in)  :: filename
  integer,intent(in)       :: natm
  integer                  :: ncid,atmdid,csdid,label,cadid,sdid,fdid
  integer                  :: time,sptvid,crdvid,cellvid,cspat,cangu,clanvid
  integer                  :: is
    is = nf90_create(filename,nf90_clobber,ncid)
!
    is = nf90_def_dim(ncid,"frame",nf90_unlimited,fdid)
    is = nf90_def_dim(ncid,"spatial",3,sdid)
    is = nf90_def_dim(ncid,"atom",natm,atmdid)
    is = nf90_def_dim(ncid,"cell_spatial",3,csdid)
    is = nf90_def_dim(ncid,"label",5,label)
    is = nf90_def_dim(ncid,"cell_angular",3,cadid)
!
    is = nf90_def_var(ncid,"spatial",nf90_char,[sdid],sptvid)
    is = nf90_def_var(ncid,"coordinates",nf90_float,[sdid,atmdid,fdid],crdvid)
    is = nf90_def_var(ncid,"cell_spatial",nf90_char,[csdid],cspat)
    is = nf90_def_var(ncid,"cell_angular",nf90_char,[label,cadid],cangu)
!
    is = nf90_put_att(ncid,time,"units","picosecond")
    is = nf90_put_att(ncid,crdvid,"units","angstrom")
!
    is = nf90_put_att(ncid,nf90_global,"title","default_name")
    is = nf90_put_att(ncid,nf90_global,"application","AMBER")
    is = nf90_put_att(ncid,nf90_global,"program","pmemd")
    is = nf90_put_att(ncid,nf90_global,"programVersion","16.0")
    is = nf90_put_att(ncid,nf90_global,"Conventions","AMBER")
    is = nf90_put_att(ncid,nf90_global,"ConventionVersion","1.0")
!
    is = nf90_enddef(ncid)
!
    is = nf90_put_var(ncid,cspat,["a","b","c"],start=[1],count=[3])
    is = nf90_put_var(ncid,cangu,reshape(["a","l","p","h","a","b","e","t","a"," ",&
                      &"g","a","m","m","a"],[5,3]),start=[1,1],count=[5,3])
    is = nf90_put_var(ncid,sptvid,["x","y","z"],start=[1],count=[3])
!
    is=nf90_close(ncid)
    RETURN
  end subroutine generateNC
!
  subroutine generateFV(filename,nres)
  character(*),intent(in)  :: filename
  integer,intent(in)       :: nres
  integer                  :: ncid,did,fdid,fvid
  integer                  :: is
    is = nf90_create(filename,nf90_clobber,ncid)
!
    is = nf90_def_dim(ncid,"frame",nf90_unlimited,fdid)
    is = nf90_def_dim(ncid,"dimension",3*nres*(nres-1)/2,did)
!
    is = nf90_def_var(ncid,"fvector",nf90_double,[did,fdid],fvid)
!
    is = nf90_enddef(ncid)
    is=nf90_close(ncid)
    RETURN
  end subroutine generateFV
!
  subroutine PrintProgresBar(n)
  integer,intent(in)    :: n
    write(STDOUT,'(a,i0,a)') 'Processing vector space for ',n,' points'
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
end program vanz
