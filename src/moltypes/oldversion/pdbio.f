!------------------------pdbio.f90------------------------!
!                        Ver.1.0.0                        !
!  PDBGET :: Read PDB file and return atomINFO format     !
!---------------------------------------------------------!
logical function MolLoadPDB(this) result(res)
use spur_string
class(molinfo),intent(inout)      :: this
real                              :: xyz(3),Cryst1(3)
type(vector_character)            :: title
type(vector_real)                 :: crd
character(6)                      :: Section
character(80)                     :: Line
integer                           :: Resid, is, i,j
!
  Cryst1 = 0.0
  do
    line = this%fdata%gets()
    res = this%fdata%isERR() ; if(res) RETURN
!
    Section = large(Line(1:6))
!
    select case(Section)
    case("TITLE")
      call title%push(adjustl(Line(10:80)))
    case("CRYST1")
      read(Line(7:33),fmt='(3f9.3)',iostat=is)Cryst1(:) ; res = is/=0 ; if(res)RETURN
    case("ATOM","HETATM")
      read(Line(23:27),fmt='(i4)',iostat=is)Resid       ; res = is/=0 ; if(res)RETURN
      read(Line(31:54),fmt='(3f8.3)',iostat=is)xyz(:)   ; res = is/=0 ; if(res)RETURN
      call this%AtmName%push(adjustl(Line(13:16)))
      call this%ResName%push(adjustl(Line(18:20)))
      call this%Atmtype%push(adjustl(Line(77:78)))
      call this%Residue%push(Resid)
      call crd%push(xyz)
    case("END")
      EXIT
    end select
    if(this%fdata%isEOF())EXIT
  enddo
!
  this%NATM = this%AtmName%size()
  this%TITLE = title%join()
!
  select type(this)
  type is(molcrd)
    call MolcrdReserve(this,this%NATM,1)
    do i=1,this%NATM
      j = i * 3
      this%xyz(1:3,i,1) = crd%at(j-2:j)
    enddo
    this%box(1:3,1) = cryst1(1:3)
  end select
end function MolLoadPDB
!
logical function TrjLoadPDB(this,inpfile) result(res)
use spur_string
class(Trajectory),intent(inout)   :: this
class(fileio),intent(inout)       :: inpfile
logical                           :: ioerr,nerr
real                              :: xyz(3),Cryst1(3)
type(vector_real)                 :: crd
character(6)                      :: Section
character(80)                     :: Line
integer                           :: Resid, is, i,j,NATM
  call inpfile%fetch(STAT=is) ; res = is/=0
  call ERR%check(res,"FILE OPEN FAILED. "//inpfile%NameIs())
  if(res)RETURN
!
  do
    NATM = 0
    if(this%hasBox)Cryst1 = 0.0
    do
      line = inpfile%gets()
      res = inpfile%isERR() ; if(res) RETURN
      if(inpfile%isEOF())then
        call inpfile%quit() ; RETURN
      endif
!
      Section = large(Line(1:6))
!
      select case(Section)
      case("CRYST1")
        if(this%hasBox)read(Line(7:33),fmt='(3f9.3)',iostat=is)Cryst1
      case("ATOM","HETATM")
        NATM = NATM + 1
        read(Line(31:54),fmt='(3f8.3)',iostat=is)xyz
        call crd%push(xyz)
      case("END")
        EXIT
      end select
    enddo
!
    res = TrjCheckNatm(this,NATM)
    call ERR%check(res, "NATM is not match   "//inpfile%NameIs())
    if(res) RETURN
!
    this%NTRJ = this%NTRJ + 1 ; call TrjExpand(this)
    do i=1,NATM
      if(this%mask(i))then
        j = i * 3
        this%xyz(1:3,i,this%NTRJ) = crd%at(j-2:j)
      endif
    enddo
    if(this%hasBox) this%box(1:3,this%NTRJ) = cryst1(1:3)
  enddo
end function TrjLoadPDB
!
logical function MolExportPDB(this,fout) result(res)
use spur_string
class(molinfo),intent(inout)       :: this
type(fileio),intent(inout)         :: fout
  select type(this)
  class is(molcrd)
    call MolCrdExportPDB(this)
  class default
    res = .FALSE.
  end select
  RETURN
contains
subroutine MolCrdExportPDB(this)
class(molcrd),intent(inout)        :: this
type(vector_character)            :: wrap
integer                           :: i,j,k,is,DevN,NatmLocal
character(:),allocatable          :: FFMT,CFMT
character(4)                      :: AtmName(this%NATM)
character(3)                      :: ResName(this%NATM)
character(2)                      :: Element(this%NATM)
integer                           :: Residue(this%NATM)
logical                           :: mask(this%NATM)
  mask = this%Mask%lookup()
  NatmLocal = count(mask)
  res = NatmLocal==0 ; if(res)RETURN
!
  devn = fout%DevN()
  allocate(character(19) :: CFMT)
  CFMT = '("CRYST1",4X,3F9.3)'
  allocate(character(45) :: FFMT)
  FFMT = '(A6,I5,X,A4,A1,A3,X,A1,I4,4X,3F8.3,22X,A2,2X)'
!
  if(this%AtmName%empty())then;  AtmName = "  XX"
  else                        ;  AtmName = this%AtmName%lookup()
  endif
!
  if(this%ResName%empty())then;  ResName = "LIG"
  else                        ;  ResName = this%ResName%lookup()
  endif
!
  if(this%Residue%empty())then  ;  Residue = 1
  else                        ;  Residue = this%Residue%lookup()
  endif
!
  if(this%Element%empty())then;  Element = "Xx"
  else
    Element = AtmNameFromAtomicNum(this%Element%lookup())
  endif
!
  call wrap%textwrap(this%title,70)
  write(devn,'("TITLE",5X,A)',iostat=is)wrap%lookup()
  res = is/=0 ; if(res)RETURN
  do k = 1,this%NTRJ
    write(devn,CFMT,iostat=is) this%box(1:3,k)
    res = is/=0 ; if(res)RETURN
    j=0
    do i = 1,this%NATM
      if(this%mask%at(i))then
        j=j+1
        write(devn,FFMT,IOSTAT=is)"ATOM  ",j,atmname(i)," ",&
                                 & ResName(i)," ",Residue(i),&
                                 & this%xyz(1:3,i,k), Element(i)
        res = is/=0 ; if(res)RETURN
      endif
    enddo
    write(DevN,'(A)',IOSTAT=is)"TER   " ; res = is/=0 ; if(res)RETURN
  enddo
  write(DevN,'(A)',IOSTAT=is)"END   " ; res = is/=0 ; if(res)RETURN
end subroutine MolCrdExportPDB
end function MolExportPDB
