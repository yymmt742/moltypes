!------------------------xyzio.f--------------------------!
!                        Ver.1.0.0                        !
!  XYZGET :: Read PDB file and return atominfo format     !
!---------------------------------------------------------!
!
  logical function MolLoadXYZ(this,fdata) result(res)
  class(molinfo),intent(inout)      :: this
  class(fileio),intent(inout)       :: fdata
  character(LenAtmName),allocatable :: AtomName(:)
  real                              :: Dummy(3)
  character(80)                     :: Line
  integer                           :: i,is,dev,natm
    call fdata%fetch(STAT=is) ; res = is/=0 .or. .not.fdata%isExist()
    call this%ERR%check(res,"FILE OPEN FAILED. "//fdata%NameIs())
    if(res)RETURN
!
    dev = fdata%DevN()
    read(dev,*,err=100,end=100) natm
    read(dev,*,err=100,end=100) line
    res = this%NATM>0.and.this%NATM/=natm
    if(res)RETURN
    allocate(AtomName(NATM))
    do i=1,NATM
      read(dev,*,err=100,end=100)AtomName(i),Dummy(:)
    enddo
    this%NATM    = natm
    this%NRES    = 1
    this%title   = line
    this%AtmName = AtomName
    call fdata%quit()
    deallocate(AtomName)
    RETURN
100 continue
  end function MolLoadXYZ
!
logical function TrjLoadXYZ(this,fdata,lus) result(res)
class(Trajectory),intent(inout)   :: this
class(fileio),intent(inout)       :: fdata
integer,intent(in)                :: lus(3)
character(LenAtmName)             :: Dummy
integer                           :: i, j, nmax, is, dev, NATM
logical                           :: ioerr,nerr
  call fdata%fetch(STAT=is) ; res = is/=0
  call this%top%ERR%check(res,"FILE OPEN FAILED. "//fdata%NameIs())
  if(res)RETURN
!
  dev = fdata%DevN()
!
  read(dev,*,iostat=is) NATM
  res = TrjCheckNatm(this,NATM)
  call this%top%ERR%check(res, "NATM is not match   "//fdata%NameIs())
  if(res)RETURN
!
  do i=1,(NATM+2)*(lus(1)-1)
    read(dev,*,iostat=is)
  enddo
!
  j = 0
  nmax = lus(2)-lus(1)+1
  do
    j = j + 1
    if(lus(2)>0)then
      if(j==nmax)EXIT
    endif
    res = is>0 ; if(res.or.is<0)EXIT
!
    if(mod(j,lus(3))/=0)then
      do i=1,NATM+2
        read(dev,*,iostat=is)
      enddo
      CYCLE
    endif
!
    read(fdata%DevN(),*,iostat=is)
    this%NTRJ = this%NTRJ + 1
    call TrjExpand(this)
    do i=1,NATM
      if(this%top%mask(i))then
        read(dev,*,iostat=is)Dummy,this%xyz(:,i,this%NTRJ)
      else
        read(dev,*,iostat=is)
      endif
    enddo
    this%Box(:,this%NTRJ) = TrjVDWBox(this%xyz(1,:,this%NTRJ),&
                                     &this%xyz(2,:,this%NTRJ),&
                                     &this%xyz(3,:,this%NTRJ),&
                                     &this%NATM)
    read(dev,*,iostat=is)
  enddo
  RETURN
end function TrjLoadXYZ
!
logical function TrjExportXYZ(this,fdata,fmode) result(res)
class(trajectory),intent(inout)   :: this
type(fileio),intent(inout)        :: fdata
integer,intent(in)                :: fmode
character(LenAtmName)             :: AtomName(count(this%top%mask))
integer                           :: i,j,k,is,dev
  if(fmode==0)then
    call fdata%generate(fdata%nameis(),STAT=is)
  elseif(fmode==1)then
    call fdata%fetch(fdata%nameis(),APPEND=.TRUE.,STAT=is)
  endif
  res = this%NATM<=0 ; if(res)RETURN
  dev = fdata%DevN()
  if(this%top%atmname%empty())then
    AtomName = 'Xx'
  else
    AtomName = pack(this%top%atmname%lookup(),this%selection)
  endif
!
  do j=1,this%NTRJ
    write(dev,'(I0,/,A)',IOSTAT=is)this%Natm,this%top%Title
    do i = 1,this%NATM
      write(dev,'(A,4X,3F16.9)',IOSTAT=is)AtomName(i),this%xyz(1:3,i,j)
    enddo
  enddo
  res = is/=0
  RETURN
end function TRJExportXYZ
