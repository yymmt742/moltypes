program vpaa_trjcut
!$ use omp_lib
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use moltypes
use spur_vector
implicit none
double precision     :: time
  time = omp_get_wtime()
  call compute_trjcut()
  write(STDOUT,'(a,f11.3,a)') 'Total calculation time :: ',omp_get_wtime()-time,' sec.'
contains
  subroutine compute_trjcut()
  use spur_optparse
  use spur_string
  use spur_stdio
  type(moltype)            :: mt
  type(optparse)           :: op
  type(stdio)              :: cutlog
  real                     :: threshold,threshold2
  integer                  :: nxyz,nrrs,ntrj
  integer                  :: natma,natm,nres
  integer                  :: ratma,ratm,rres
  integer                  :: dig1,dig2,dig3,progres
  character(:),allocatable :: residmol,residref
  real,allocatable         :: X(:,:),integral(:,:),breakdown(:,:)
  real                     :: box(3),rcpbox(3),revn,step
  integer,allocatable      :: label(:),memo(:),seq(:)
  logical,allocatable      :: mola(:,:),mol(:,:),refa(:,:),ref(:,:)
  logical,allocatable      :: molwrt(:,:),refwrt(:,:)
  integer                  :: nclu,rclu,nsum,serial
  logical,allocatable      :: new(:),old(:,:)
  character(:),allocatable :: nout
  integer                  :: i,j,k
    call op%add_description('Cluster analysis and calculate their fluctuations.')
    call op%add_option("-mol",narg=1,metavar='string',def=['all'], help='clustering molecules mask.')
    call op%add_option("-ref",narg=1,metavar='string',def=['none'],help='reference molecules mask.')
    call op%add_option("-log",narg=1,metavar='string',def=['trjcut.log'],help='reference molecules mask.')
    call op%add_option("-s",narg=1,metavar='num',def=['0.1'],help='time step size. default [0.1]')
    call op%add_option("-t",alias=["--threshold"],narg=1,metavar='value',def=['5.0'],&
                      &help='cluster decision threshold(angstrom). default 5.0')
    call op%parser()
    if(op%narg()==0) call op%call_usage()
!
    threshold = op%optargf('-t',1) ; threshold2 = threshold * threshold
    step      = op%optargf('-s',1)
!
    mt%terminates_at_abnormal = .TRUE.
    call mt%fetch(op%args())
!
    call GetResidlist(mt,op%optargs('-mol',1),residmol)
    call GetResidlist(mt,op%optargs('-ref',1),residref)
!
    call mt%atomselect(op%optargs('-mol',1))
    nres = mt%nresidues()
    natm = mt%natoms()
    if(nres>0) natm = natm/nres 
    call mt%atomselect(residmol)
    natma = mt%natoms()
    if(nres>0) natma = natma/nres 
    call mt%atomselect(op%optargs('-ref',1))
    rres = mt%nresidues()
    ratm = mt%natoms()
    if(rres>0) ratm = ratm/rres 
    call mt%atomselect(residref)
    ratma = mt%natoms()
    if(rres>0) ratma = ratma/rres 
    call mt%atomselect([residmol,residref])
    nxyz = mt%natoms()
    nrrs = nres + rres
    ntrj = mt%nframes()
    if(natm<=0.or.nres<=1.or.ntrj<=0) call Abort(1)
!
    allocate(X(3,nxyz))
    allocate(mola(3,nxyz),mol(3,nxyz),refa(3,nxyz),ref(3,nxyz))
    allocate(molwrt(3,nxyz),refwrt(3,nxyz))
    allocate(label(nrrs),memo(nrrs),seq(nrrs))
    allocate(breakdown(2:nres,0:rres),integral(2:nres,0:rres))
    allocate(new(nrrs),old(nrrs,nrrs))
    allocate(character(0)::nout)
!
    mola  = mt%getmask(residmol,3)
    mol   = mt%getmask(op%optargs('-mol',1),3)
    refa  = mt%getmask(residref,3)
    ref   = mt%getmask(op%optargs('-ref',1),3)
!
    seq   = [(i,i=1,nrrs)]
    memo  = 0
!
    progres = 0
    old = .FALSE.
    dig1 = digit(nrrs)
    dig2 = digit(rres)
    dig3 = digit(ntrj)
    integral = 0.0
    revn = 1.0/ntrj
!
    cutlog = op%optargs('-log',1)
    call cutlog%generate()
    if(rres>0)  write(cutlog%devn(),'(A12,*(I9))') '#HOST MOL   ',[([(j,i=2,nrrs)],j=0,rres)]
    write(cutlog%devn(),'(A12,*(I9))')  '#CLUSTER MOL',[([(i,i=2,nrrs)],j=0,rres)]
!
    call PrintProgresBar(ntrj)
    do i=1,ntrj
      breakdown = 0.0
      call mt%load(lb=i,ub=i)
      X   = mt%xyz(i)
      box = mt%box(i)
      rcpbox = reciprocal(box)
      call get_network(natm,nres,ratm,rres,&
                      &reshape(pack(X,mol),[3,natm,nres]),&
                      &reshape(pack(X,ref),[3,ratm,rres]),&
                      &box,rcpbox,threshold,threshold2,label)
      do j=1,nrrs
        new = j==label
        nclu = count(new(1:nres)) ; rclu = count(new) - nclu
        if(nclu<=1)CYCLE
        breakdown(nclu,rclu) = breakdown(nclu,rclu) + 1.0
        integral(nclu,rclu)  = integral(nclu,rclu)  + revn
!
        serial = -1
        do k=1,nrrs
          if(all(new==old(:,k)))then
            serial = memo(k) ; EXIT
          endif
        enddo
        if(serial==-1)then
          memo(j) = i ; serial = memo(j)
        endif
!
        nsum = natma*nclu + ratma*rclu
        molwrt = filter(nxyz,natma,nres,pack(seq,new),mola)
        refwrt = filter(nxyz,ratma,rres,pack(seq,new),refa)
!
        if(rclu>0)then
          nout = cutlog%basename()//padding(nclu,dig1,'0')//'with'//padding(rclu,dig2,'0')//'guest/'&
                &//padding(serial,dig3,'0')//'.'//join(pack(seq,new),'+')//'.nc'
        else
          nout = cutlog%basename()//padding(nclu,dig1,'0')//'/'&
                &//padding(serial,dig3,'0')//'.'//join(pack(seq,new),'+')//'.nc'
        endif
        call CatchSnap(nout,nsum,&
                     & wrap(natma,nclu,ratma,rclu,&
                     & reshape(pack(X,molwrt),[3,natma,nclu]),&
                     & reshape(pack(X,refwrt),[3,ratma,rclu]),&
                     & box,rcpbox))
!
        old(:,j) = new
      enddo
      do j=0,rres
        if(sum(breakdown(:,j))<0.5) CYCLE
        breakdown(:,j) = breakdown(:,j) / sum(breakdown(:,j))
        write(cutlog%devn(),'(F12.3)',advance='no') real(i)*step
        write(cutlog%devn(),'(F9.6)',advance='no') breakdown(2,j)
        do k=3,nres
          breakdown(k,j) = breakdown(k,j) + breakdown(k-1,j)
          write(cutlog%devn(),'(F9.6)',advance='no') breakdown(k,j)
        enddo
        write(cutlog%devn(),'(A)') ''
      enddo
      call PrintCounter(ntrj,i,progres)
    enddo
    if(rres>0)  write(cutlog%devn(),'(A12,*(I9))') '#HOST MOL   ',[([(j,i=2,nrrs)],j=0,rres)]
    write(cutlog%devn(),'(A12,*(I9))')  '#CLUSTER MOL',[([(i,i=2,nrrs)],j=0,rres)]
    do j=0,rres
      if(sum(integral(:,j))<0.5) CYCLE
      integral(:,j) = integral(:,j) / sum(integral(:,j))
      write(cutlog%devn(),'(A12)',advance='no')'#ALL OVER   '
      write(cutlog%devn(),'(F9.6)',advance='no') integral(2,j)
      do k=3,nres
        integral(k,j) = integral(k,j) + integral(k-1,j)
        write(cutlog%devn(),'(F9.6)',advance='no') integral(k,j)
      enddo
      write(cutlog%devn(),'(A)') ''
    enddo
    deallocate(nout)
    call cutlog%quit()
    call PrintCounter(ntrj,ntrj,progres)
  end subroutine compute_trjcut
!
  function filter(nxyz,natm,nres,seq,mol) result(res)
  integer,intent(in) :: nxyz,natm,nres,seq(:)
  logical,intent(in) :: mol(3,nxyz)
  logical            :: res(3,nxyz)
  integer            :: i,atm,resid
    atm = 1 ; resid = 1
    do i=1,nxyz
      res(:,i) = any(resid==seq)
      if(all(mol(:,i)))then
        atm = atm + 1
        if(atm>natm)then
          atm = 1 ; resid = resid + 1
        endif
      endif
    enddo
  end function filter
!
  subroutine GetResidlist(mt,mask,res)
  use spur_string
  type(moltype),intent(inout) :: mt
  character(*),intent(in)     :: mask
  character(:),allocatable    :: res
  type(vector_integer)        :: resid,unqid
  type(vector_character)      :: tmp
  integer                     :: i
    allocate(character(0)::res)
    call mt%AtomSelect(mask)
    resid = mt%inq('resid',1)
    unqid = resid%uniq()
    if(unqid%size()==0)then
      res = 'none' ; return
    endif
    do i=1,unqid%size()
      call tmp%push(ToStr(unqid%at(i)))
    enddo
    if(allocated(res)) deallocate(res)
    res = 'resid '//tmp%join()
  end subroutine GetResidlist
!
  subroutine get_network(natm,nres,ratm,rres,crd,ref,box,rcpbox,threshold,threshold2,label)
  integer,intent(in)               :: natm,nres,ratm,rres
  real,intent(in)                  :: crd(3,natm,nres),ref(3,ratm,rres)
  real,intent(in)                  :: box(3),rcpbox(3),threshold,threshold2
  integer,intent(out)              :: label(nres+rres)
  type(vector_integer)             :: bond
  integer                          :: i,j,big,lit
    do i=1,rres ; do j=i+1,rres
        if(checkAttach(ratm,ratm,ref(:,:,i),ref(:,:,j),box,rcpbox,threshold,threshold2)) call bond%push([nres+i,nres+j])
    enddo ; enddo
!
    do j=1,rres ; do i=1,nres
        if(checkAttach(natm,ratm,crd(:,:,i),ref(:,:,j),box,rcpbox,threshold,threshold2)) call bond%push([i,nres+j])
    enddo ; enddo
!
    do i=1,nres ; do j=i+1,nres
      if(checkAttach(natm,natm,crd(:,:,i),crd(:,:,j),box,rcpbox,threshold,threshold2)) call bond%push([i,j])
    enddo ; enddo
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
    call bond%clear()
  end subroutine get_network
!
  logical function checkAttach(natmi,natmj,crdi,crdj,box,rcpbox,threshold,threshold2) result(res)
  integer,intent(in) :: natmi,natmj
  real,intent(in)    :: crdi(3,natmi),crdj(3,natmj)
  real,intent(in)    :: box(3),rcpbox(3),threshold,threshold2
  real               :: corneri(3,2),cornerj(3,2),maxmin(3),minmax(3),v(3)
  integer            :: i,j
    corneri = maxmin_corner(natmi,crdi)
    cornerj = maxmin_corner(natmj,crdj)
    maxmin = corneri(:,1) - cornerj(:,2) + threshold ; maxmin = maxmin - box * anint(maxmin*rcpbox)
    minmax = corneri(:,2) - cornerj(:,1) - threshold ; minmax = minmax - box * anint(minmax*rcpbox)
    if(ANY(maxmin*minmax>0))RETURN
 
    do j=1,natmj
      do i=1,natmi
        v = crdj(:,j) - crdi(:,i)
        v = v - box * anint(v*rcpbox)
        res = dot_product(v,v) < threshold2
        if(res) RETURN
      enddo
    enddo
  end function checkAttach
!
  pure function maxmin_corner(natm,crd) result(res)
  integer,intent(in) :: natm
  real,intent(in)    :: crd(3,natm)
  real               :: res(3,2)
  integer            :: i
    res(:,1) = -HUGE(0.0) ; res(:,2) = -res(:,1)
    do i=1,natm
      if(res(1,1)<crd(1,i)) res(1,1) = crd(1,i)
      if(res(2,1)<crd(2,i)) res(2,1) = crd(2,i)
      if(res(3,1)<crd(3,i)) res(3,1) = crd(3,i)
      if(res(1,2)>crd(1,i)) res(1,2) = crd(1,i)
      if(res(2,2)>crd(2,i)) res(2,2) = crd(2,i)
      if(res(3,2)>crd(3,i)) res(3,2) = crd(3,i)
    enddo
  end function maxmin_corner
!
  subroutine CatchSnap(x,nsum,c0)
  use moltypes_export
  character(*),intent(in)  :: x
  integer,intent(in)       :: nsum
  real,intent(in)          :: c0(3,nsum,1)
    call ExportAmberNetcdf(x,nsum,1,c0,overwrite=.FALSE.)
  end subroutine CatchSnap
!
  function wrap(natm,nres,ratm,rres,crd,refcrd,box,rcpbox) result(res)
  integer,intent(in)       :: natm,nres,ratm,rres
  real,intent(in)          :: crd(3,natm,nres),refcrd(3,ratm,rres)
  real,intent(in)          :: box(3),rcpbox(3)
  real                     :: res(3,natm*nres+ratm*rres)
  real                     :: com(3,nres+rres),revn,ccom(3),ofs(3)
  integer                  :: i,j,k,iter
    com  = 0.0
!
    revn = 1.0 / real(natm)
    do j=1,nres
      do i=1,natm
        com(:,j) = com(:,j) + crd(:,i,j)
      enddo
      com(:,j) = com(:,j) * revn
    enddo
!
    k = nres
    if(rres>0)then
      revn = 1.0 / dble(ratm)
      do j=1,rres
        k=k+1
        do i=1,ratm
          com(:,k) = com(:,k) + refcrd(:,i,j)
        enddo
        com(:,k) = com(:,k) * revn
      enddo
    endif
!
    iter = 0
    ccom = mean_position(3,nres+rres,com,com(:,nres+rres),box,rcpbox,iter)
!
    k = 0
    do j=1,nres
      ofs =  com(:,j) - ccom      ; ofs = - ccom - box * anint(ofs * rcpbox)
      do i=1,natm
        k=k+1
        res(:,k) = crd(:,i,j) + ofs
      enddo
    enddo
!
    do j=1,rres
      ofs =  com(:,nres+j) - ccom ; ofs = - ccom - box * anint(ofs * rcpbox)
      do i=1,ratm
        k=k+1
        res(:,k) = refcrd(:,i,j) + ofs
      enddo
    enddo
  end function wrap
!
  recursive function mean_position(d,n,x,xcom,box,rbox,iter) result(res)
  integer,intent(in)    :: d,n
  real,intent(in)       :: x(d,n),xcom(d)
  real,intent(in)       :: box(d),rbox(d)
  integer,intent(inout) :: iter
  real                  :: res(d)
  real                  :: com(d),dif(d),v(d)
  integer               :: i
    com = xcom * dble(n)
    do i=1,n
      v = x(:,i) - xcom ; v = v - box * anint(v * rbox)
      com = com + v
    enddo
    com = com / dble(n)
    dif = dble(n) * (com - xcom)
    if(dot_product(dif,dif)<10E-5.or.iter>=200)then
      res = com
    else
      iter = iter + 1
      res = mean_position(d,n,x,com,box,rbox,iter)
    endif
  end function mean_position
!
  pure function reciprocal(box) result(res)
  real,intent(in) :: box(3)
  real            :: res(3)
    if(any(box<10E-5))then
      res = 0.0
    else
      res = 1.0 / box
    endif
  end function reciprocal
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
end program vpaa_trjcut
