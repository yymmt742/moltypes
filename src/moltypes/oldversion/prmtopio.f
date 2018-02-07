!----------------------prmtopio.f90-----------------------!
!                       Ver.1.0.0                         !
!  PMTGET :: Read PRMTOP file, return as atomINFO format  !
!  Usage                                                  !
!    call PMTGET(FILE,A,NATM,NRES,INFO)                   !
!         FILE :: character(*)                            !
!         A    :: type(atomINFO),allocatable,dimension(:) !
!         NATM :: integer                                 !
!         NRES :: integer                                 !
!         INFO :: integer                                 !
!---------------------------------------------------------!
!
logical function MolLoadPRMTOP(this,fdata) result(res)
use spur_string
class(molinfo),intent(inout)      :: this
type(fileio),intent(inout)        :: fdata
integer                           :: POINTERS(31)
character(4),allocatable          :: TITLE(:)
integer,allocatable               :: ATMICNUM(:),RESIDUE(:)
character(4),allocatable          :: ATMNAM(:),ATMTYP(:)
real,allocatable                  :: MASS(:),CHG(:),RADII(:)
character(4),allocatable          :: RESNAM(:),RN(:)
integer,allocatable               :: ATM_P_MOL(:)
integer,allocatable               :: BONDH(:),BONDA(:)
integer,allocatable               :: ANGLEH(:),ANGLEA(:)
integer,allocatable               :: DIHEDH(:),DIHEDA(:)
integer,allocatable               :: RESPNT(:)
character(80)                     :: Line
character(12)                     :: CFMT=""
integer                           :: NATM,NRES,NBONH,NBONA
integer                           :: NANGH,NANGA,NDIHH,NDIHA
integer                           :: i,j,is,io
  call fdata%fetch(STAT=is)
  res = .not.fdata%isExist() ; if(res) RETURN
  line = fdata%seek("%FLAG TITLE")
  res = fdata%isAbnormal() ; if(res) RETURN
  call ReadPrmA(TITLE,20)
!
  line = fdata%seek("%FLAG POINTERS")
  res = fdata%isAbnormal() ; if(res) RETURN
  CFMT = ReadFMT()              ; if(res) RETURN
!
  read(fdata%devn(),CFMT,iostat=io)POINTERS
  res = io/=0 ; if(res) RETURN
!
  NATM =POINTERS(1) ; NRES=POINTERS(12)
  NBONH=POINTERS(3) ; NBONA=POINTERS(13)
  NANGH=POINTERS(5) ; NANGA=POINTERS(14)
  NDIHH=POINTERS(7) ; NDIHA=POINTERS(15)
  res = this%NATM>0.and.this%NATM/=NATM ; if(res)RETURN
!
  do
    line = fdata%seek("%FLAG")
    res = fdata%isERR() ; if(res) RETURN
    if(fdata%isEOF())EXIT
!
    select case(LARGE(Line(7:)))
      case("ATOMIC_NUMBER")              ; call ReadPrmI(ATMICNUM,NATM)
      case("ATOM_NAME")                  ; call ReadPrmA(ATMNAM,NATM)
      case("AMBER_ATOM_TYPE")            ; call ReadPrmA(ATMTYP,NATM)
      case("CHARGE")                     ; call ReadPrmF(CHG,NATM)     ;  CHG = CHG * StdChg
      case("MASS")                       ; call ReadPrmF(MASS,NATM)
      case("RADII")                      ; call ReadPrmF(RADII,NATM)
      case("RESIDUE_POINTER")            ; call ReadPrmI(RESPNT,NRES)
      case("RESIDUE_LABEL")              ; call ReadPrmA(RESNAM,NRES)
      case("BONDS_INC_HYDROGEN")         ; call ReadPrmI(BONDH,3*NBONH)
      case("BONDS_WITHOUT_HYDROGEN")     ; call ReadPrmI(BONDA,3*NBONA)
      case("ANGLES_INC_HYDROGEN")        ; call ReadPrmI(ANGLEH,4*NANGH)
      case("ANGLES_WITHOUT_HYDROGEN")    ; call ReadPrmI(ANGLEA,4*NANGA)
      case("DIHEDRALS_INC_HYDROGEN")     ; call ReadPrmI(DIHEDH,5*NDIHH)
      case("DIHEDRALS_WITHOUT_HYDROGEN") ; call ReadPrmI(DIHEDA,5*NDIHA)
      case default
    end select
  enddo
!
  allocate(RESIDUE(NATM),RN(NATM))
!
  if(NRES>1.and.allocated(RESPNT))then
    do i=1,NRES-1
      RESIDUE(RESPNT(i):RESPNT(i+1)-1)=i
      RN(RESPNT(i):RESPNT(i+1)-1)=RESNAM(i)
    enddo
    RESIDUE(RESPNT(NRES):)=NRES
    RN(RESPNT(NRES):)=RESNAM(NRES)
  else
    RESIDUE = 1 ; RN      = "LIG"
  endif
!
  this%NATM    = NATM
  this%NRES    = NRES
  if(allocated(title)) this%title   = join(TITLE)
  if(allocated(ATMNAM))this%AtmName = ATMNAM
  if(allocated(RN))    this%ResName = RN
  if(allocated(ATMTYP))this%AtmType = ATMTYP
  if(allocated(CHG))   this%chg     = CHG
  if(allocated(RADII)) this%vdw     = RADII
  if(allocated(MASS))  this%mass    = MASS
  if(allocated(RESIDUE))this%residue= RESIDUE
  if(allocated(ATMICNUM))then
    if(MINVAL(ATMICNUM)>0)this%element = ATMICNUM
  endif
  call this%bond%reserve((NBONH+NBONA)*2)
  call this%angle%reserve((NANGH+NANGA)*3)
  call this%dihed%reserve((NDIHH+NDIHA)*4)
  if(allocated(BONDH))call this%bond%push(pack(BONDH,[(modulo(i,3)/=0,i=1,NBONH*3)])/3+1)
  if(allocated(BONDA))call this%bond%push(pack(BONDA,[(modulo(i,3)/=0,i=1,NBONA*3)])/3+1)
  if(allocated(ANGLEH))call this%angle%push(pack(ANGLEH,[(modulo(i,4)/=0,i=1,NANGH*4)])/3+1)
  if(allocated(ANGLEA))call this%angle%push(pack(ANGLEA,[(modulo(i,4)/=0,i=1,NANGA*4)])/3+1)
  if(allocated(DIHEDH))call this%dihed%push(abs(pack(DIHEDH,[(modulo(i,5)/=0,i=1,NDIHH*5)]))/3+1)
  if(allocated(DIHEDA))call this%dihed%push(abs(pack(DIHEDA,[(modulo(i,5)/=0,i=1,NDIHA*5)]))/3+1)
  RETURN
contains
  subroutine ReadPrmI(A,N)
  integer,intent(inout),allocatable :: A(:)
  integer,intent(in)                :: N
    if(N<=0)RETURN
    if(allocated(A))deallocate(A) ; allocate(A(N))
    CFMT = ReadFMT() ; if(res) RETURN
    read(fdata%devn(),CFMT,iostat=io) A
    res = io/=0 ; if(res) RETURN
  end subroutine ReadPrmI
!
  subroutine ReadPrmF(A,N)
  real,intent(inout),allocatable :: A(:)
  integer,intent(in)             :: N
    if(N<=0)RETURN
    if(allocated(A))deallocate(A) ; allocate(A(N))
    CFMT = ReadFMT() ; if(res) RETURN
    read(fdata%devn(),CFMT,iostat=io) A
  end subroutine ReadPrmF
!
  subroutine ReadPrmA(A,N)
  character(*),intent(inout),allocatable :: A(:)
  integer,intent(in)                     :: N
    if(N<=0)RETURN
    if(allocated(A))deallocate(A) ; allocate(A(N))
    CFMT = ReadFMT() ; if(res) RETURN
    read(fdata%devn(),CFMT,iostat=io) A
    res = io/=0 ; if(res) RETURN
  end subroutine ReadPrmA
!
  character(12) function ReadFMT() result(cfmt)
  integer                     :: i,j
    cfmt = ''
    Line = fdata%gets()
    res  = fdata%isAbnormal() ; if(res) RETURN
    if(Line(1:7)=="%FORMAT")then
      i = 7
      do j=1,12
        i = i + 1
        cfmt(j:j)=Line(i:i)
        if(Line(i:i)==")")RETURN
      enddo
    endif
    res = .TRUE. ; RETURN
  end function ReadFMT
end function MolLoadPRMTOP
!
logical function MolExportPRMTOP(this,fdata) result(res)
class(molinfo),intent(inout)      :: this
type(fileio),intent(inout)        :: fdata
integer                           :: i,j,DevN
integer                           :: NATM,NTYPE,NRES
integer                           :: NBND,NANG,NDIH
integer                           :: ckey(this%NATM)
type(vector_character)            :: rname,atype,uniqtype
type(vector_integer)              :: uniqres,rid
type(vector_integer)              :: bond,angle,dihed
  NATM = count(this%Mask) ;  res = NATM<=0
  call this%ERR%check(res, "This trajectory is empty.   "//fdata%NameIs())
  if(res)RETURN
!
  if(this%Residue%empty())then
    NRES = 1
  else
    rid = this%Residue%lookup()
    uniqres = rid%uniq() ; NRES = uniqres%size()
  endif
!
  if(.not.this%ResName%empty())rname = this%ResName%lookup()
!
  if(this%AtmType%empty())then
    do i=1,NATM
      call atype%push('xx')
    enddo
  else
    atype = this%AtmType%lookup()
  endif
!
  uniqtype = atype%uniq()
  NTYPE = uniqtype%size()
!
  NBND = 0
!
  j = 0
  do i=1,this%NATM
    if(this%mask(i))then ; j = j + 1 ; ckey(i) = j
    else                 ; ckey(i)=0
    endif
  enddo
!
  if(.not.this%bond%empty())then
    do i=1,this%bond%size(),2
      if(this%mask(this%bond%at(i)).and.this%mask(this%bond%at(i+1)))then
        call bond%push((ckey(this%bond%at(i))-1)*3)
        call bond%push((ckey(this%bond%at(i+1))-1)*3)
        call bond%push(0)
      endif
    enddo
  endif
!
  if(.not.this%angle%empty())then
    do i=1,this%angle%size(),3
      if(this%mask(this%angle%at(i)).and.  &
        &this%mask(this%angle%at(i+1)).and.&
        &this%mask(this%angle%at(i+2)))then
        call angle%push((ckey(this%angle%at(i))-1)*3)
        call angle%push((ckey(this%angle%at(i+1))-1)*3)
        call angle%push((ckey(this%angle%at(i+2))-1)*3)
        call angle%push(0)
      endif
    enddo
  endif
!
  if(.not.this%dihed%empty())then
    do i=1,this%dihed%size(),4
      if(this%mask(this%dihed%at(i)).and.&
        &this%mask(this%dihed%at(i+1)).and.&
        &this%mask(this%dihed%at(i+2)).and.&
        &this%mask(this%dihed%at(i+3)))then
        call dihed%push((ckey(this%dihed%at(i))-1)*3)
        call dihed%push((ckey(this%dihed%at(i+1))-1)*3)
        call dihed%push((ckey(this%dihed%at(i+2))-1)*3)
        call dihed%push((ckey(this%dihed%at(i+3))-1)*3)
        call dihed%push(0)
      endif
    enddo
  endif
!
  NBND = bond%size() / 3
  NANG = angle%size() / 4
  NDIH = dihed%size() / 5
  call fdata%generate() ; DevN = fdata%DevN()
  call ExportPRMTOP(NATM,NRES,NTYPE,NBND,NANG,NDIH)
!
contains
  subroutine ExportPRMTOP(NATM,NRES,NTYPE,NBND,NANG,NDIH)
  integer,intent(in)      :: NATM,NRES,NTYPE,NBND,NANG,NDIH
  character(4)            :: AtmName(NATM),ResName(NRES),AtmType(NATM)
  real                    :: Charge(NATM),MASS(NATM)
  integer                 :: ATMICNUM(NATM),RESIDUE(NATM),RESPNT(NRES)
  integer                 :: BONDA(NBND),ATYPEIDX(NATM)
  type(vector_character)  :: wrap
  integer                 :: i,j,k,is
!
  if(this%AtmName%empty())then;  AtmName = "Xx  "
  else                        ;  AtmName = this%AtmName%lookup()
  endif
  if(this%chg%empty())then    ;  Charge = 0.0
  else                        ;  Charge = this%chg%lookup() * StdChgR
  endif
  if(this%Element%empty())then;  ATMICNUM = -1
  else                        ;  ATMICNUM = this%Element%lookup()
  endif
  if(this%mass%empty())then   ;  MASS = 0.0
  else                        ;  MASS = this%mass%lookup()
  endif
!
  if(this%Residue%empty())then;  Residue = 1
  else                        ;  Residue = rid%lookup()
  endif
!
  if(this%ResName%empty())then
    ResName = "LIG"
  else
    j=0 ; k=0
    do i=1,NATM
      if(j<Residue(i))then
        k = k + 1
        ResName(k) = rname%at(i)
        RESPNT(k) = i
        j=Residue(i)
      endif
    enddo
  endif
!
  do i=1,NATM
    AtmType(i) = atype%at(i)
    ATYPEIDX(i) = MAXLOC([(1,j=1,NTYPE)],1,AtmType(i)==uniqtype%lookup())
  enddo
!
  write(devn,'(A,72X)',iostat=is)"%VERSION"
  write(devn,'(A,69X,/,A,67X)',iostat=is)"%FLAG TITLE",                  "%FORMAT(20a4)"
  call wrap%textwrap(this%title,80) ; write(devn,'(A)',iostat=is)wrap%lookup()
  write(devn,'(A,66X,/,A,67X)',iostat=is)"%FLAG POINTERS",               "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)NATM,NTYPE,0,NBND,0,0,0,0,0,0, 0,NRES,NBND,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0
  write(devn,'(A,65X,/,A,67X)',iostat=is)"%FLAG ATOM_NAME",                 "%FORMAT(20a4)"
  write(DevN,'(20A4)',IOSTAT=is)AtmName
  write(devn,'(A,68X,/,A,65X)',iostat=is)"%FLAG CHARGE",                    "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is)Charge
  write(devn,'(A,61X,/,A,67X)',iostat=is)"%FLAG ATOMIC_NUMBER",             "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)ATMICNUM
  write(devn,'(A,70X,/,A,65X)',iostat=is)"%FLAG MASS",                      "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is)MASS
  write(devn,'(A,59X,/,A,67X)',iostat=is)"%FLAG ATOM_TYPE_INDEX",           "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)ATYPEIDX
  write(devn,'(A,53X,/,A,67X)',iostat=is)"%FLAG NUMBER_EXCLUDED_ATOMS",     "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,54X,/,A,67X)',iostat=is)"%FLAG NONBONDED_PARM_INDEX",      "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,61X,/,A,67X)',iostat=is)"%FLAG RESIDUE_LABEL",             "%FORMAT(20a4)"
  write(DevN,'(20A4)',IOSTAT=is)ResName
  write(devn,'(A,55X,/,A,67X)',iostat=is)"%FLAG RESIDUE_POINTER",           "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)ResPnt
  write(devn,'(A,55X,/,A,65X)',iostat=is)"%FLAG BOND_FORCE_CONSTANT",       "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 1.0
  write(devn,'(A,58X,/,A,65X)',iostat=is)"%FLAG BOND_EQUIL_VALUE",          "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 1.4
  write(devn,'(A,54X,/,A,65X)',iostat=is)"%FLAG ANGLE_FORCE_CONSTANT",      "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 100.0
  write(devn,'(A,57X,/,A,65X)',iostat=is)"%FLAG ANGLE_EQUIL_VALUE",         "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 2.094396
  write(devn,'(A,51X,/,A,65X)',iostat=is)"%FLAG DIHEDRAL_FORCE_CONSTANT",   "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 1.0
  write(devn,'(A,54X,/,A,65X)',iostat=is)"%FLAG DIHEDRAL_PERIODICITY",      "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 1.0
  write(devn,'(A,60X,/,A,65X)',iostat=is)"%FLAG DIHEDRAL_PHASE",            "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is) 0.0
  write(devn,'(A,69X,/,A,65X)',iostat=is)"%FLAG SOLTY",                     "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is)
  write(devn,'(A,56X,/,A,65X)',iostat=is)"%FLAG RENARD_JONES_ACOEF",        "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is)
  write(devn,'(A,56X,/,A,65X)',iostat=is)"%FLAG RENARD_JONES_BCOEF",        "%FORMAT(5E16.8)"
  write(DevN,'(5E16.8)',IOSTAT=is)
  write(devn,'(A,56X,/,A,67X)',iostat=is)"%FLAG BONDS_INC_HYDROGEN",        "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,52X,/,A,67X)',iostat=is)"%FLAG BONDS_WITHOUT_HYDROGEN",    "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)bond%lookup()
  write(devn,'(A,55X,/,A,67X)',iostat=is)"%FLAG ANGLES_INC_HYDROGEN",       "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,51X,/,A,67X)',iostat=is)"%FLAG ANGLES_WITHOUT_HYDROGEN",   "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)angle%lookup()
  write(devn,'(A,52X,/,A,67X)',iostat=is)"%FLAG DIHEDRALS_INC_HYDROGEN",    "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,48X,/,A,67X)',iostat=is)"%FLAG DIHEDRALS_WITHOUT_HYDROGEN","%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)dihed%lookup()
  write(devn,'(A,55X,/,A,67X)',iostat=is)"%FLAG EXCLUDED_ATOMS_LIST",       "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,63X,/,A,67X)',iostat=is)"%FLAG HBOND_ACOEF",               "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,63X,/,A,67X)',iostat=is)"%FLAG HBOND_BCOEF",               "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,69X,/,A,67X)',iostat=is)"%FLAG HBCUT",                     "%FORMAT(10I8)"
  write(DevN,'(10I8)',IOSTAT=is)
  write(devn,'(A,59X,/,A,67X)',iostat=is)"%FLAG AMBER_ATOM_TYPE",           "%FORMAT(20a4)"
  write(DevN,'(20A4)',IOSTAT=is)AtmType
  res = is/=0 ; RETURN
  RETURN
  end subroutine ExportPRMTOP
end function MolExportPRMTOP
