!----------------------rst7io.f90-------------------------!
!                       Ver.1.0.0                         !
!  RSTGET :: Read RST7 file                               !
!---------------------------------------------------------!
  integer function ReadNatmFromRst7(fdata) result(res)
  class(fileio),intent(inout)       :: fdata
  integer                           :: dev
  type(vector_character)            :: spl
  character(:),allocatable          :: line
    call fdata%gofoward() ; call spl%Split(fdata%gets())
    allocate(character(0)::line)
    if(spl%size()>0) line = spl%at(1)
    read(line,*,err=100)res
    if(.not.fdata%isAbnormal())RETURN
100 continue
    res = -1 ; RETURN
  end function ReadNatmFromRST7
!
  logical function TrjLoadRST(this,fdata) result(res)
  use netcdf
  class(Trajectory),intent(inout)   :: this
  class(fileio),intent(inout)       :: fdata
  integer                           :: is,dev,NATM
  integer                           :: ncid
  logical                           :: nerr, iserr
  call fdata%fetch(STAT=is)
  if(fdata%IsBinary())then
    call fdata%quit()
    iserr = nf90_open(fdata%NameIs(),nf90_nowrite,ncid)   /= nf90_NoErr
!
    NATM = ReadNatmFromNetcdf(ncid)
    nerr = TrjCheckNatm(this,NATM)
!
    if(.not. iserr.or.nerr) iserr = ReadRst7Netcdf(ncid,this%top%NATM,this%top%mask)
    iserr = nf90_close(ncid) /= nf90_NoErr .or. iserr
  else
    NATM = ReadNatmFromRst7(fdata)
    nerr = TrjCheckNatm(this,NATM)
!
    dev = fdata%devN()
    iserr = ReadRst7(dev,this%top%NATM,this%top%mask)
    call fdata%quit()
  endif
  call this%top%ERR%check(iserr,"File open failed. "//fdata%NameIs())
  call this%top%ERR%check(nerr, "NATM is not match   "//fdata%NameIs())
  res = iserr.or.nerr ; RETURN
  RETURN
contains
  logical function ReadRST7(dev,NATM,mask) result(res)
  integer,intent(in)                :: dev,NATM
  logical,intent(in)                :: mask(NATM)
  double precision                  :: CRD(3,NATM),C(6*NATM+6)
  integer                           :: i,j,is
    read(dev,'(6F12.7)',err=100,iostat=is)C
!
    this%NTRJ = this%NTRJ + 1 ; call TrjExpand(this)
    CRD = reshape(C(1:3*NATM),[3,NATM])
    j = 0
    do i=1,NATM
      if(mask(i))then
        j = j + 1
        this%xyz(:,j,this%NTRJ) = CRD(:,i)
      endif
    enddo
    if(is<0)then
      this%Box(:,this%NTRJ) = C(3*NATM+1:3*NATM+3)
    elseif(is==0)then
      this%Box(:,this%NTRJ) = C(6*NATM+1:6*NATM+3)
    endif
    res = .FALSE. ; RETURN
100 continue
    res = .TRUE.  ; RETURN
  end function ReadRST7
!
  logical function ReadRst7Netcdf(ncid,NATM,mask) result(res)
  integer,intent(in)           :: ncid,NATM
  logical,intent(in)           :: mask(NATM)
  double precision             :: CRD(3,NATM)
  integer                      :: i,j
  integer                      :: VarID1,VarID2
    res = nf90_inq_varid(ncid,"coordinates",VarID1)  /= nf90_NoErr
    res = nf90_inq_varid(ncid,"cell_lengths",VarID2) /= nf90_NoErr .or. res
    res = nf90_get_var(ncid,VarID1,CRD)              /= nf90_NoErr .or. res
    if(res)RETURN
!
    this%NTRJ = this%NTRJ + 1 ; call TrjExpand(this)
    j = 0
    do i=1,NATM
      if(mask(i))then
        j = j + 1 ; this%xyz(:,j,this%NTRJ) = CRD(:,i)
      endif
    enddo
!
    res = nf90_get_var(ncid,VarID2,this%Box(:,this%NTRJ)) /= nf90_NoErr
    RETURN
  end function ReadRst7Netcdf
end function TrjLoadRST
!
logical function TrjExportRST(this,fdata) result(res)
class(trajectory),intent(inout)   :: this
type(fileio),intent(inout)        :: fdata
integer                           :: is,DevN
  if(res.or.this%NTRJ<=0)RETURN
  DevN = fdata%DevN()
  write(DevN,'(A)',IOSTAT=is)"default_name"
  write(DevN,'(i5,5e15.7)',IOSTAT=is)this%NATM,0.d0
  write(DevN,'(6F12.7)',iostat=is) this%xyz(:,:,this%NTRJ)
  write(DevN,'(6F12.7)',iostat=is)this%Box(:,this%NTRJ),90.0,90.0,90.0
  res = fdata%isAbnormal().or.is/=0
  RETURN
end function TrjExportRST
