!------------------------chgio.f--------------------------!
logical function MolLoadCHARGE(this,fdata) result(res)
use spur_string
class(molinfo),intent(inout)      :: this
type(fileio),intent(inout)        :: fdata
integer                           :: is
  res = this%NATM<=0.or.this%AtmName%Empty() ; if(res)RETURN
  call fdata%fetch(STAT=is)
  res = .not.fdata%isExist() ; if(res) RETURN
  this%chg = GetChgArray(this%AtmName%size(),this%AtmName%lookup())
contains
  function GetChgArray(NATM,AtmList) result(res)
  integer,intent(in)                :: NATM
  character(*),intent(in)           :: AtmList(NATM)
  double precision                  :: res(NATM)
  character(lenAtmname)             :: ATM
  logical                           :: mask(NATM)
  double precision                  :: C
  integer                           :: i,j,dev,is
  dev = fdata%DevN()
  res = 0.d0
  do
    read(dev,*,iostat=is) ATM,C
    if(is>0)CYCLE ; if(is<0)EXIT
    mask = ATM/=AtmList
    do i = 1,NATM
      if(mask(i))CYCLE
      res(i) = C
    enddo
  enddo
  end function GetChgArray
end function MolLoadCHARGE
!
logical function TrjExportCHG(this,fdata) result(res)
class(trajectory),intent(inout)   :: this
type(fileio),intent(inout)        :: fdata
character(LenAtmName)             :: AtomName(count(this%top%mask))
integer                           :: i,is,dev
  call fdata%generate(fdata%nameis(),STAT=is)
  res = this%top%AtmName%Empty().or.this%top%chg%Empty() ; if(res)RETURN
  dev = fdata%DevN()
!
  write(dev,'(I0,/,A)',IOSTAT=is)this%Natm,'#'//this%top%Title
  do i = 1,this%NATM
    write(dev,'(A,4X,F16.9)',IOSTAT=is)this%top%AtmName%at(i),this%top%chg%at(i)
  enddo
  res = fdata%isAbnormal().or.is/=0
  RETURN
end function TRJExportCHG
