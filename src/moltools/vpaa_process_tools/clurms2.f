include "mkl_vsl.fi"
program clurms
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use spur_optparse
use moltypes
use spur_vector
use spur_io
use netcdf
!
implicit none
!
type(optparse)               :: arg
double precision             :: time1,time2
integer                      :: interval
double precision             :: threshold2, step
integer                      :: natm,nres,ntrj,ratm,rres
double precision,allocatable :: crd(:,:,:,:),refcrd(:,:,:,:),box(:,:)
logical,allocatable          :: am(:,:),rm(:,:)
!
  call arg%add_description('Cluster analysis and calculate their fluctuations.')
  call arg%add_option("-p",narg=1,metavar='prmtop',&
                     &help='amber prmtop.')
  call arg%add_option("-m",narg=1,metavar='String',help='Loading mask string.')
  call arg%add_option("-mol",narg=1,metavar='String',def=['all'],&
                     &help='clustering molecules mask.')
  call arg%add_option("-ref",narg=1,metavar='String',def=['none'],&
                     &help='reference moleclules mask.')
  call arg%add_option("-tcorr",help='Calculate timecorr if this option exist.')
  call arg%add_option("-g",narg=1,metavar='filename',def=['gyro'],&
                     &help='Cluster Radius of Gyration Output file.')
  call arg%add_option("-n",narg=1,metavar='filename',def=['nwrk'],&
                     &help='Cluster Network Output file.')
  call arg%add_option("-r",narg=1,metavar='filename',def=['rmsd'],&
                     &help='Cluster RMSD Output file.')
  call arg%add_option("-a",narg=1,metavar='filename',def=['acrr'],&
                     &help='Cluster Auto correlation Output file.')
  call arg%add_option("-x",narg=1,metavar='filename',def=['clst'],&
                     &help='Cluster coordinate Output file.')
  call arg%add_option("-i",alias=["--time0"],narg=1,metavar='value',def=['1'],&
                     &help='sampling interval. default 1.')
  call arg%add_option("-t",alias=["--threshold"],narg=1,metavar='value',def=['5.0'],&
                     &help='cluster decision threshold(angstrom). default 5.0')
  call arg%add_option("-s",alias=["--second"],narg=1,metavar='value',def=['1.0'],&
                     &help='trajectry time steps(ps). default 1.0')
  call arg%parser()
  if(.not.arg%option('-p').or.arg%narg()==0) call arg%call_usage()
!
  interval   = arg%optargi('-i',1)
  threshold2 = arg%optargd('-t',1) ; threshold2 = threshold2 * threshold2
  step       = arg%optargd('-s',1) * 0.001d0
!
  time1 = omp_get_wtime()
!
  call loadFiles(arg%narg(),arg%args(),arg%optargs('-p',1),arg%optargs('-m',1),arg%optargs('-mol',1),arg%optargs('-ref',1),&
                &natm,nres,ntrj,ratm,rres,crd,refcrd,box,am,rm)
!
  call clurms_compute(natm,nres,ntrj,ratm,rres,crd,refcrd,box,am,rm,&
                     &arg%optargs('-x',1),arg%optargs('-r',1),arg%optargs('-n',1),&
                     &arg%optargs('-a',1),arg%optargs('-g',1),arg%option('-tcorr'))
!
  time2 = omp_get_wtime()
  write(STDOUT,'(a,f11.3,a)') 'Total calculation time :: ',time2-time1,' sec.'
!
contains
  subroutine loadFiles(ninp,inp,p,m,mol,ref,natm,nres,ntrj,ratm,rres,crd,refcrd,box,am,rm)
  integer,intent(in)                       :: ninp
  character(*),intent(in)                  :: inp(:)
  character(*),intent(in)                  :: p,m,mol,ref
  integer,intent(out)                      :: natm,nres,ratm,rres,ntrj
  double precision,intent(out),allocatable :: crd(:,:,:,:),refcrd(:,:,:,:)
  double precision,intent(out),allocatable :: box(:,:)
  logical,intent(out),allocatable          :: am(:,:),rm(:,:)
  type(trajectory)                         :: trj
  integer                                  :: minidx,test,i,j
    call trj%load(p,mask=m)
!
    if(trj%n_atoms()<=0)then
      write(STDOUT,'(a,a)') 'No atoms in ',p ; call abort(1)
    endif
!
    do i=1,ninp
      write(STDOUT,'(a,a,a)') 'Loading coordinate ',inp(i),'...'
      call trj%load(inp(i))
    enddo
!
    ntrj = trj%n_frames()
!
    allocate(box(3,ntrj)) ; box = trj%box(:,1:ntrj)
!
    call NeatenCrds(trj,mol,natm,nres,crd,am)
    call NeatenCrds(trj,ref,ratm,rres,refcrd,rm)
    RETURN
  end subroutine loadFiles
!
  subroutine NeatenCrds(trj,mol,natm,nres,crd,am)
  type(trajectory),intent(inout)           :: trj
  character(*),intent(in)                  :: mol
  integer,intent(out)                      :: nres,natm
  double precision,allocatable,intent(out) :: crd(:,:,:,:)
  logical,intent(out),allocatable          :: am(:,:)
  integer,allocatable                      :: resid(:)
  integer                                  :: minidx,test,i,j
  character(4),allocatable                 :: aname(:),mname(:)
  logical,allocatable                      :: tm(:)
  type(vector_character)                   :: rid
  character(12)                            :: tmp
    call trj%atomselect(mol)
    nres   = trj%n_residues()
!
    allocate(resid(trj%n_atoms()))
    resid  = trj%residue_index()
!
    minidx = minval(resid,1)
    natm   = count(minidx==resid)
    write(tmp,'(i0)') minidx ; call rid%push(tmp)
!
    do i=2,nres
      minidx = minval(resid,1,resid>minidx)
      test   = count(minidx==resid) ; if(natm/=test) call abort(3)
      write(tmp,'(i0)') minidx ; call rid%push(tmp)
    enddo
!
    allocate(aname(natm))
    aname = pack(trj%atomnames(),minidx==resid)
!
    call trj%atomselect('residue '//rid%join())
!
    deallocate(resid) ; allocate(resid(trj%n_atoms()))
    resid  = trj%residue_index()
!
    minidx = minval(resid,1)
    natm   = count(minidx==resid)
!
    do i=2,nres
      minidx = minval(resid,1,resid>minidx)
      test   = count(minidx==resid) ; if(natm/=test) call abort(4)
    enddo
!
    allocate(crd(3,natm,nres,ntrj),am(3,natm),mname(natm),tm(natm))
    mname = pack(trj%atomnames(),minidx==resid)
!
    tm = .FALSE.
    do i=1,size(aname)
      tm = tm.or.aname(i)==mname
    enddo
    do i=1,natm
      am(:,i) = tm(i)
    enddo
!
    do i=1,trj%n_frames()
      crd(:,:,:,i) = reshape(trj%coordinates(i),[3,natm,nres])
    enddo
    deallocate(resid,tm,aname,mname) ; call rid%clear()
  end subroutine NeatenCrds
!
  subroutine clurms_compute(natm,nres,ntrj,ratm,rres,crd,refcrd,box,am,rm,x,r,n,a,g,tc_run)
  integer,intent(in)             :: natm,nres,ntrj,ratm,rres
  double precision,intent(in)    :: crd(3,natm,nres,ntrj),refcrd(3,ratm,rres,ntrj)
  double precision,intent(in)    :: box(3,ntrj)
  logical,intent(in)             :: am(3,natm),rm(3,ratm)
  character(*),intent(in)        :: x,r,n,a,g
  logical,intent(in)             :: tc_run
  double precision               :: rc(ntrj,nres,0:rres),nc(ntrj,nres,0:rres)
  double precision               :: ac(ntrj,nres,0:rres),gc(ntrj,nres,0:rres)
  double precision               :: nr(ntrj,nres,0:rres)
  integer                        :: label(nres+rres)
  type(vector_integer)           :: bonds(nres)
  integer                        :: nclu,rclu,nsum,nlen
  integer                        :: seq(maxval([nres,rres],1))
  logical                        :: ns(3,natm,nres),rs(3,ratm,rres)
  integer                        :: i,j,progres
    nr = -1.d0 ; rc = 0.d0 ; nc = 0.d0 ; ac = 0.d0 ; gc = 0.d0
    seq = [(i,i=1,maxval([nres,rres],1))]
!
    call PrintProgresBar(ntrj)
    progres = 0
!
    do i=1,ntrj,interval
      call get_network(natm,nres,ratm,rres,crd(:,:,:,i),refcrd(:,:,:,i),box(:,i),am,rm,bonds,label)
      do j=1,nres+rres
        nclu = count(j==label(1:nres))
        rclu = count(j==label) - nclu
        if(nclu==0)CYCLE
        ns = mres(natm,nres,pack(seq(1:nres),j==label(1:nres)))
        rs = mres(ratm,rres,pack(seq(1:rres),j==label(nres+1:)))
        nsum = natm*nclu + ratm*rclu
        nlen = ntrj - i + 1
!
        if(nr(1,nclu,rclu)<0.d0) nr(1:nlen,nclu,rclu) = 0.d0
        nr(1:nlen,nclu,rclu) = nr(1:nlen,nclu,rclu) + 1.d0
!
        call catchSnap(x,natm,nclu,ratm,rclu,pack(crd(:,:,:,i),ns),pack(refcrd(:,:,:,i),rs),box(:,i))
!
        if(.not.tc_run)CYCLE
        call compute_timecorr(natm,nres,nclu,ratm,rres,rclu,nsum,nlen,bonds(j)%size(),bonds(j)%lookup(),&
                             &crd(:,:,:,i:),refcrd(:,:,:,i:),box(:,i:),rc(1:nlen,nclu,rclu),&
                             &nc(1:nlen,nclu,rclu),ac(1:nlen,nclu,rclu),gc(1:nlen,nclu,rclu),ns,rs)
      enddo
      call PrintCounter(ntrj,i,progres)
    enddo
!
    call PrintCounter(ntrj,ntrj,progres)
    if(.not.tc_run) RETURN
!
    call output_timecorr(natm,nres,rres,ntrj,trim(r),trim(n),trim(a),trim(g),rc,nc,ac,gc,nr)
!
  end subroutine clurms_compute
!
  subroutine get_network(natm,nres,ratm,rres,crd,refcrd,box,am,rm,bonds,label)
  integer,intent(in)               :: natm,nres,ratm,rres
  double precision,intent(in)      :: crd(3,natm,nres),refcrd(3,ratm,rres),box(3)
  logical,intent(in)               :: am(3,natm),rm(3,ratm)
  type(vector_integer),intent(out) :: bonds(nres)
  integer,intent(out)              :: label(nres+rres)
  double precision                 :: rcpbox(3)
  type(vector_integer)             :: bond
  integer                          :: i,j,big,lit,natm3,ratm3
    rcpbox = reciprocalBox(box)
    natm3  = count(am) ; ratm3 = count(rm)
    do i=1,nres
      do j=i+1,nres
        if(checkAttach(natm3,natm3,pack(crd(:,:,j),am),pack(crd(:,:,i),am),box,rcpbox)) call bond%push([i,j])
      enddo
    enddo
!
    do i=1,rres
      do j=i+1,rres
        if(checkAttach(ratm3,ratm3,pack(refcrd(:,:,j),rm),pack(refcrd(:,:,i),rm),box,rcpbox)) call bond%push([nres+i,nres+j])
      enddo
    enddo
!
    do j=1,rres
      do i=1,nres
        if(checkAttach(natm3,ratm3,pack(crd(:,:,i),am),pack(refcrd(:,:,j),rm),box,rcpbox)) call bond%push([i,nres+j])
      enddo
    enddo
! i<j
    label = [(i,i=1,nres+rres)]
    do i=1,bond%size(),2
      if(label(bond%at(i))==label(bond%at(i+1)))CYCLE
      lit = minval(label(bond%at(i:i+1)),1)
      big = maxval(label(bond%at(i:i+1)),1)
      do j = big,nres+rres
        if(label(j)==big) label(j) = lit
      enddo
    enddo
!
    call vclear(bonds)
    do i=1,bond%size(),2
      call bonds(label(bond%at(i)))%push([bond%at(i:i+1)])
    enddo
    call bond%clear()
  end subroutine get_network
!
  logical function checkAttach(natmi,natmj,crdi,crdj,box,rcpbox) result(res)
  integer,intent(in)             :: natmi,natmj
  double precision,intent(in)    :: crdi(natmi),crdj(natmj),box(3),rcpbox(3)
  double precision               :: cnri(3,2),cnrj(3,2)
  integer                        :: i,j,k
    cnri = maxmin_corner(natmi,crdi) ; cnrj = maxmin_corner(natmj,crdj)
    res = .not.isAttach([cnri(1,1),cnri(2,1),cnri(3,1)] - [cnrj(1,2),cnrj(2,2),cnrj(3,2)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,1),cnri(2,1),cnri(3,2)] - [cnrj(1,2),cnrj(2,2),cnrj(3,1)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,1),cnri(2,2),cnri(3,1)] - [cnrj(1,2),cnrj(2,1),cnrj(3,2)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,1),cnri(2,2),cnri(3,2)] - [cnrj(1,2),cnrj(2,1),cnrj(3,1)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,2),cnri(2,1),cnri(3,1)] - [cnrj(1,1),cnrj(2,2),cnrj(3,2)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,2),cnri(2,1),cnri(3,2)] - [cnrj(1,1),cnrj(2,2),cnrj(3,1)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,2),cnri(2,2),cnri(3,1)] - [cnrj(1,1),cnrj(2,1),cnrj(3,2)],box,rcpbox) ; if(.not.res)RETURN
    res = .not.isAttach([cnri(1,2),cnri(2,2),cnri(3,2)] - [cnrj(1,1),cnrj(2,1),cnrj(3,1)],box,rcpbox) ; if(.not.res)RETURN
!
    do j=1,natmj,3
      do i=1,natmi,3
        res = isAttach(crdi(i:i+2) - crdj(j:j+2),box,rcpbox) ; if(res)RETURN
      enddo
    enddo
  end function checkAttach
!
  logical function isAttach(v,box,rcpbox) result(res)
  double precision,intent(in)    :: v(3),box(3),rcpbox(3)
  double precision               :: vt(3)
    vt = v - box * dnint(v*rcpbox)
    res = dot_product(vt,vt) < threshold2
  end function isAttach
!
  pure function maxmin_corner(natm,crd) result(res)
  integer,intent(in)             :: natm
  double precision,intent(in)    :: crd(natm)
  double precision               :: res(3,2)
  integer                        :: i
    res(:,1) = -HUGE(0.d0)
    res(:,2) =  HUGE(0.d0)
    do i=1,natm,3
      if(res(1,1)<crd(i))  res(1,1) = crd(i)
      if(res(2,1)<crd(i+1))res(2,1) = crd(i+1)
      if(res(3,1)<crd(i+2))res(3,1) = crd(i+2)
      if(res(1,2)>crd(i))  res(1,2) = crd(i)
      if(res(2,2)>crd(i+1))res(2,2) = crd(i+1)
      if(res(3,2)>crd(i+2))res(3,2) = crd(i+2)
    enddo
  end function maxmin_corner
!
  subroutine catchSnap(x,natm,nres,ratm,rres,crd,refcrd,box)
  character(*),intent(in)        :: x
  integer,intent(in)             :: natm,nres,ratm,rres
  double precision,intent(in)    :: crd(3*natm*nres),refcrd(3*ratm*nres),box(3)
  double precision               :: c0(3,natm*nres+ratm*rres),rcpbox(3)
  integer                        :: frame
  type(fileio)                   :: ncout
  character(15)                   :: serial
    rcpbox = reciprocalBox(box)
    c0 = unwrap(natm,nres,ratm,rres,crd,refcrd,box,rcpbox)
    write(serial,'(a1,i3.3,11x)')'_',nres
    if(rres>0) write(serial(5:),'(a4,i3.3,a4)')'with',rres,'refs'
    ncout = trim(x)//trim(serial)//".nc"
    call writeNC(ncout,natm*nres+ratm*rres,c0)
  end subroutine catchSnap
!
  subroutine compute_timecorr(natm,nres,nclu,ratm,rres,rclu,nsum,ntrj,nbond,bond,crd,refcrd,box,rc,nc,ac,gc,ns,rs)
  integer,intent(in)             :: natm,nres,nclu,ratm,rres,rclu,nsum,ntrj,nbond,bond(nbond)
  double precision,intent(in)    :: crd(3,natm,nres,ntrj),refcrd(3,ratm,rres,ntrj),box(3,ntrj)
  double precision,intent(inout) :: rc(ntrj),nc(ntrj),ac(ntrj),gc(ntrj)
  logical,intent(in)             :: ns(3,natm,nres),rs(3,ratm,rres)
  double precision               :: c0(3,nsum),c1(3,nsum),rcpbox(3),rot(3,3)
  double precision               :: r(3),rms,rog,aco,rcpn,rcpac
  logical                        :: attach
  integer                        :: i,j
    rcpbox = reciprocalBox(box(:,1))
    c0 = unwrap(natm,nclu,ratm,rclu,pack(crd(:,:,:,1),ns),pack(refcrd(:,:,:,1),rs),box(:,1),rcpbox)
!
!$omp parallel do reduction(+:nc,rc,ac,gc), private(rot,rms,aco,rog,r,c1,attach)
    do i=1,ntrj
      rcpbox = reciprocalBox(box(:,i))
      c1 = unwrap(natm,nclu,ratm,rclu,pack(crd(:,:,:,i),ns),pack(refcrd(:,:,:,i),rs),box(:,i),rcpbox)
      rot = rmsrot(nsum,c1,c0)
!
      do j=1,nbond,2
        if(bond(j)<=nres.and.bond(j+1)<=nres)then
          attach = checkAttach(natm,natm,crd(:,:,bond(j),i),crd(:,:,bond(j+1),i),box(:,i),rcpbox)
        elseif(bond(j)<=nres.and.bond(j+1)>nres)then
          attach = checkAttach(natm,ratm,crd(:,:,bond(j),i),refcrd(:,:,bond(j+1)-nres,i),box(:,i),rcpbox)
        elseif(bond(j)>nres.and.bond(j+1)<=nres)then
          attach = checkAttach(ratm,natm,refcrd(:,:,bond(j)-nres,i),crd(:,:,bond(j+1),i),box(:,i),rcpbox)
        else
          attach = checkAttach(ratm,ratm,refcrd(:,:,bond(j)-nres,i),refcrd(:,:,bond(j+1)-nres,i),box(:,i),rcpbox)
        endif
        if(attach) nc(i) = nc(i) + 1.d0
      enddo
!
      rms = 0.d0 ; aco = 0.d0 ; rog = 0.d0
      do j=1,nclu*natm
        c1(:,j) = matmul(rot,c1(:,j))
        r = c1(:,j) - c0(:,j)
        rms = rms + dot_product(r,r)
        aco = aco + dot_product(c1(:,j),c0(:,j))
        rog = rog + dot_product(c1(:,j),c1(:,j))
      enddo
      aco   = aco / rog
      rc(i) = rc(i) + rms
      ac(i) = ac(i) + aco
      gc(i) = gc(i) + rog
    enddo
!$omp end parallel do
  end subroutine compute_timecorr
!
  function unwrap(natm,nres,ratm,rres,crd,refcrd,box,rcpbox) result(res)
  integer,intent(in)                   :: natm,nres,ratm,rres
  double precision,intent(in)          :: crd(3,natm,nres),refcrd(3,ratm,rres)
  double precision,intent(in)          :: box(3),rcpbox(3)
  double precision                     :: res(3,natm*nres+ratm*rres)
  double precision                     :: com(3,nres+rres),comcom(3)
  double precision                     :: offset(3,nres+rres),nrev
  integer                              :: i,j,k
    nrev = 1.d0 / dble(natm)
    com  = 0.d0
    k    = nres
!
    do j=1,nres
      do i=1,natm
        com(:,j) = com(:,j) + crd(:,i,j)
      enddo
      com(:,j) = com(:,j) * nrev
      offset(:,j) = box * dnint((com(:,j)-com(:,1))*rcpbox)
    enddo
!
    if(ratm>0)nrev = 1.d0/dble(ratm)
    do j=1,rres
      k = k + 1
      do i=1,ratm
        com(:,k) = com(:,k) + refcrd(:,i,j)
      enddo
      com(:,k) = com(:,k) * nrev
      offset(:,k) = box * dnint((com(:,k)-com(:,1))*rcpbox)
    enddo
!
    k = 0
    do j=1,nres
      do i=1,natm
        k=k+1
        res(:,k) = crd(:,i,j) - offset(:,j)
        comcom = comcom + res(:,k)
      enddo
    enddo
!
    do j=1,rres
      do i=1,ratm
        k=k+1
        res(:,k) = refcrd(:,i,j) - offset(:,nres+j)
      enddo
    enddo
!
    comcom = 0.d0 
    do i=1,nres+rres
      comcom = comcom + com(:,i) - offset(:,i)
    enddo
    comcom = comcom / dble(nres+rres)
!
    do i=1,natm*nres+ratm*rres
      res(:,i) = res(:,i) - comcom
    enddo
  end function Unwrap
!
  pure function reciprocalBox(box) result(res)
  double precision,intent(in) :: box(3)
  double precision            :: res(3)
    if(any(box<10E-5))then
      res = 0.d0
    else
      res = 1.d0 / box
    endif
  end function reciprocalBox
!
  pure function mres(natm,nres,idx) result(res)
  integer,intent(in) :: natm,nres,idx(:)
  logical            :: res(3,natm,nres)
  integer            :: i
    do i = 1,nres
      res(:,:,i) = any(idx==i)
    enddo
  end function mres
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
  subroutine writeNC(ncout,natm,crd)
  type(fileio),intent(inout)  :: ncout
  integer,intent(in)          :: natm
  double precision,intent(in) :: crd(3*natm)
  integer                     :: is,ncid,fid,varid,frame
    ncid = openNC(ncout,natm)
    is = nf90_inq_dimid(ncid,"frame",fid)
    is = nf90_inquire_dimension(ncid,fid,len=frame)
    is = nf90_inq_varid(ncid,"coordinates",varid)
    is = nf90_put_var(ncid,varid,reshape(crd,[3,natm]),start=[1,1,frame+1],count=[3,natm,1])
    is = nf90_close(ncid) ; RETURN
  end subroutine writeNC
!
  integer function openNC(ncout,natm) result(res)
  type(fileio),intent(inout) :: ncout
  integer,intent(in)         :: natm
  integer                    :: atmdid,csdid,label,cadid,sdid,fdid
  integer                    :: time,sptvid,crdvid,cellvid,cspat,cangu,clanvid
  integer                    :: is
    if(ncout%isExist())then
      is = nf90_open(ncout%nameis(),nf90_write,res) ; RETURN
    endif
!
    is = nf90_create(ncout%nameis(),nf90_clobber,res)
!
    is = nf90_def_dim(res,"frame",nf90_unlimited,fdid)
    is = nf90_def_dim(res,"spatial",3,sdid)
    is = nf90_def_dim(res,"atom",natm,atmdid)
    is = nf90_def_dim(res,"cell_spatial",3,csdid)
    is = nf90_def_dim(res,"label",5,label)
    is = nf90_def_dim(res,"cell_angular",3,cadid)
!
    is = nf90_def_var(res,"spatial",nf90_char,[sdid],sptvid)
    is = nf90_def_var(res,"coordinates",nf90_float,[sdid,atmdid,fdid],crdvid)
    is = nf90_def_var(res,"cell_spatial",nf90_char,[csdid],cspat)
    is = nf90_def_var(res,"cell_angular",nf90_char,[label,cadid],cangu)
!
    is = nf90_put_att(res,time,"units","picosecond")
    is = nf90_put_att(res,crdvid,"units","angstrom")
!
    is = nf90_put_att(res,nf90_global,"title","default_name")
    is = nf90_put_att(res,nf90_global,"application","AMBER")
    is = nf90_put_att(res,nf90_global,"program","pmemd")
    is = nf90_put_att(res,nf90_global,"programVersion","16.0")
    is = nf90_put_att(res,nf90_global,"Conventions","AMBER")
    is = nf90_put_att(res,nf90_global,"ConventionVersion","1.0")
!
    is = nf90_enddef(res)
!
    is = nf90_put_var(res,cspat,["a","b","c"],start=[1],count=[3])
    is = nf90_put_var(res,cangu,reshape(["a","l","p","h","a","b","e","t","a"," ",&
                      &"g","a","m","m","a"],[5,3]),start=[1,1],count=[5,3])
    is = nf90_put_var(res,sptvid,["x","y","z"],start=[1],count=[3])
    RETURN
  end function openNC
!
  subroutine output_timecorr(natm,nres,rres,ntrj,r,n,a,g,rc,nc,ac,gc,nr)
  integer,intent(in)             :: natm,nres,rres,ntrj
  character(*),intent(in)        :: r,n,a,g
  double precision,intent(in)    :: rc(ntrj,nres,0:rres),nc(ntrj,nres,0:rres)
  double precision,intent(in)    :: ac(ntrj,nres,0:rres),gc(ntrj,nres,0:rres)
  double precision,intent(inout) :: nr(ntrj,nres,0:rres)
  double precision               :: na(ntrj,nres,0:rres)
  character(3)                   :: serial
  integer                        :: i
    write(STDOUT,'(a)') ' writing logfiles... '
    nr = 1.d0 / nr ; na = nr / dble(natm)
    call PrintLog(r//'.dat',nres,ntrj,rc(:,:,0),na(:,:,0),.TRUE.)
    call PrintLog(n//'.dat',nres,ntrj,nc(:,:,0),nr(:,:,0),.FALSE.)
    call PrintLog(a//'.dat',nres,ntrj,ac(:,:,0),nr(:,:,0),.FALSE.)
    call PrintLog(g//'.dat',nres,ntrj,gc(:,:,0),na(:,:,0),.TRUE.)
    do i=1,rres
      if(all(nr(1,:,i)<0.d0))CYCLE
      write(serial,'(i3.3)') i
      call PrintLog(r//'with'//serial//'refs.dat',nres,ntrj,rc(:,:,i),na(:,:,i),.TRUE.)
      call PrintLog(n//'with'//serial//'refs.dat',nres,ntrj,nc(:,:,i),nr(:,:,i),.FALSE.)
      call PrintLog(a//'with'//serial//'refs.dat',nres,ntrj,ac(:,:,i),nr(:,:,i),.FALSE.)
      call PrintLog(g//'with'//serial//'refs.dat',nres,ntrj,gc(:,:,i),na(:,:,i),.TRUE.)
    enddo
  end subroutine output_timecorr
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
  subroutine PrintLog(f,nres,ntrj,v,n,sq)
  character(*),intent(in)     :: f
  integer,intent(in)          :: nres,ntrj
  double precision,intent(in) :: v(ntrj,nres),n(ntrj,nres)
  logical,intent(in)          :: sq
  type(fileio)                :: fout
  integer                     :: i,j,dev
    fout = f
    call fout%generate()
    dev = fout%devn()
!
    write(dev,'(a)',advance='no') '# time(ns)  '
    do i=2,nres
      if(n(1,i)<0.0d0)CYCLE ; write(dev,'(i12)',advance='no') i
    enddo
!
    write(dev,'(a)') '  |'
    do j=1,ntrj
      write(dev,'(f12.3)',advance='no'),step*j
      do i=2,nres
        if(n(1,i)<0.d0)CYCLE
        if(n(j,i)<0.d0)then
          write(dev,'(a12)',advance='no')'   ------   ' ; CYCLE
        endif
        if(sq)then
          write(dev,'(f12.3)',advance='no'),sqrt(v(j,i)*n(j,i)/dble(i))
        else
          write(dev,'(f12.3)',advance='no'),v(j,i)*n(j,i)
        endif
      enddo
      write(dev,'(a)') '  |'
    enddo
  end subroutine PrintLog
!
  elemental subroutine vclear(v)
  type(vector_integer),intent(inout) :: v
    call v%clear() ; RETURN
  end subroutine vclear
!
  subroutine Abort(n)
  integer,intent(in)    :: n
    write(STDOUT,'(a)') 'Calclation was terminated abnormaly.'
    call exit(n)
  end subroutine Abort
end program clurms
