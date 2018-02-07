module periodic_tables
  use spur_vector
  use spur_io
  implicit none
  private
  public :: periodic_table
!
  integer,parameter                  :: LenAtomSymbol = 5
  integer,parameter                  :: LenAtomName   = 20
  integer,parameter                  :: LenAtomType   = 5
  integer,parameter                  :: LenOrbital    = 5
  character(LenAtomSymbol),parameter :: DummySymbol   = 'Xx   '
  character(LenAtomName),parameter   :: DummyName     = 'XXXXX               '
  character(LenAtomType),parameter   :: DummyType     = 'xx   '
  character(LenOrbital),parameter    :: DummyOrbit    = 'sp3  '
  double precision,parameter         :: DummyRadii    = 0.d0
  double precision,parameter         :: DummyMass     = HUGE(0.d0)
  double precision,parameter         :: DummyChg      = 0.d0
!
  type ElementType
    integer                            :: length = 0, stack = -1
    character(LenAtomType),allocatable :: Name(:)
    character(LenOrbital),allocatable  :: orbit(:)
    double precision,allocatable       :: rdi(:), ms(:),chg(:)
  contains
    final     :: ElDestractor
  end type ElementType
!
  type periodic_table
    private
    integer                              :: length = 0, stack = -1
    character(LenAtomSymbol),allocatable :: symbol(:)
    type(vector_character),allocatable   :: name(:)
    double precision,allocatable         :: ms(:),rdi(:)
    type(ElementType),allocatable        :: type(:)
  contains
    procedure         :: load               => LoadElementData
    procedure         :: export             => ExportElementData
    procedure,private :: PtAs
    procedure,private :: PtAsfromN
    generic           :: atomsymbol         => PtAs,PtAsfromN
    procedure         :: atomname           => PtNmfromN
    procedure,private :: PtMs
    procedure,private :: PtMsfromN
    generic           :: mass               =>  PtMs,PtMsfromN
    procedure,private :: PtRdi
    procedure,private :: PtRdifromN
    generic           :: radii              =>  PtRdi,PtRdifromN
    final             :: PtDestractor
  end type periodic_table
!
contains
  subroutine LoadElementData(this,path)
  class(periodic_table),intent(inout)  :: this
  character(*),intent(in)              :: path
  type(fileio)                         :: fdata
  character(:),allocatable             :: Line, key, val
  type(vector_integer)                 :: ind
  integer                              :: lind,inest,mode,comment
  integer                              :: coron,atmidx,typidx
    allocate(character(0)::Line,key,val) ; fdata = path
    ind = 0 ; inest = 1 ; mode = 0
!
    do
      Line = fdata%gets() ; if(fdata%isAbnormal())EXIT
      comment = index(Line,'#') ; if(comment>0)Line = Line(:comment-1)
      coron = index(Line,':')
      lind = CountIndent()
      key  = adjustl(Line(:coron-1)) ; val = trim(adjustl(Line(coron+1:)))
      if(key=='')CYCLE
      if(lind>ind%at(inest))then
        inest = inest + 1 ; call ind%push(lind)
      elseif(lind<ind%at(inest))then
        do
          if(ind%size()==0)RETURN
          if(ind%pop()<=lind)EXIT
        enddo
      endif
      select case(inest)
      case(1) ; call parseAtomSymbol(atmidx)
      case(2) ; call parseAtomProp(mode)
      case(3)
        select case(mode)
        case(1) ; call parseAtomType(typidx)
        end select
      case(4)
        select case(mode)
        case(1) ; call parseTypeProp()
        end select
      end select
    enddo
  contains
    integer function CountIndent() result(res)
    integer :: i
      do i = 1,len(Line)
        if(Line(i:i)==' ')CYCLE ; res =  i - 1 ; RETURN
      enddo
      res = 0 ; RETURN
    end function CountIndent
!
    subroutine parseAtomSymbol(atmidx)
    integer,intent(out) :: atmidx
    integer             :: i
      atmidx = MAXLOC([(1,i=1,this%length+1)],1,[key==this%symbol(1:this%length),.TRUE.])
      if(atmidx<=this%length)RETURN
      this%length = this%length + 1 ; call ElementExtention(this) ; this%Symbol(this%length) = key
      call SetDummy(this,this%length)
    end subroutine parseAtomSymbol
!
    subroutine parseAtomProp(mode)
    use spur_string
    integer,intent(out) :: mode
      mode = 0 ; if(atmidx<=0)RETURN
      select case(large(key))
      case('NAME')         ; call this%name(atmidx)%push(val)
      case('MASS')         ; read(val,*,err=100)this%ms(atmidx)
      case('RADII')        ; read(val,*,err=100)this%rdi(atmidx)
      case('TYPES')        ; mode = 1
      end select
100   continue
    end subroutine parseAtomProp
!
    subroutine parseAtomType(typidx)
    integer,intent(out) :: typidx
    integer             :: i
      if(atmidx<=0)RETURN
      typidx = MAXLOC([(1,i=1,this%type(atmidx)%length+1)],1,[key==this%type(atmidx)%name(1:this%type(atmidx)%length),.TRUE.])
      if(typidx<=this%type(atmidx)%length)RETURN
      this%type(atmidx)%length = this%type(atmidx)%length + 1
      call TypeExtention(this%type(atmidx)) ; this%type(atmidx)%name(this%type(atmidx)%length) = key
      call SetDummyType(this%type(atmidx),this%type(atmidx)%length)
    end subroutine parseAtomType
!
    subroutine parseTypeProp()
    use spur_string
      select case(large(key))
      case('ORBITAL')      ; this%type(atmidx)%orbit(typidx) = val
      case('MASS')         ; read(val,*,err=100)this%type(atmidx)%ms(typidx)
      case('RADII')        ; read(val,*,err=100)this%type(atmidx)%rdi(typidx)
      case('CHARGE')       ; read(val,*,err=100)this%type(atmidx)%chg(typidx)
      end select
100   continue
    end subroutine parseTypeProp
  end subroutine LoadElementData
!
  pure subroutine SetDummy(this,idx)
  class(periodic_table),intent(inout) :: this
  integer,intent(in)                  :: idx
    this%ms(idx)  = DummyMass
    this%rdi(idx) = DummyRadii
    RETURN
  end subroutine SetDummy
!
  pure subroutine SetDummyType(this,idx)
  class(ElementType),intent(inout)    :: this
  integer,intent(in)                  :: idx
    this%orbit(idx) = DummyOrbit
    this%ms(idx)  = DummyMass
    this%rdi(idx) = DummyRadii
    this%chg(idx)   = DummyChg
    RETURN
  end subroutine SetDummyType
!
  subroutine ExportElementData(this,path)
  class(periodic_table),intent(inout)  :: this
  character(*),intent(in),optional     :: path
  type(fileio)                         :: fdata
  character(80)                        :: Line
  logical                              :: ioerr,nerr
  integer                              :: i, j, is
    if(present(path)) call fdata%generate(path,STAT=is)
    do i=1,this%length
      call fdata%puts(trim(this%Symbol(i))//":")
      do j=1,this%name(i)%size()
        call fdata%puts("  NAME:     "//this%name(i)%at(j))
      enddo
      if(this%ms(i)<1.0E10)then
        write(line,'(f9.3)')this%ms(i) ; call fdata%puts("  MASS:     "//trim(adjustl(line)))
      endif
      if(this%rdi(i)>1.0E-2)then
        write(line,'(f9.3)')this%rdi(i) ; call fdata%puts("  RADII:    "//trim(adjustl(line)))
      endif
!
      if(this%type(i)%length>0)call fdata%puts("  TYPES:")
      do j=1,this%type(i)%length
        call fdata%puts("    "//trim(this%type(i)%name(j))//":")
        if(this%type(i)%ms(j)<1.0E10)then
          write(line,'(f9.3)')this%type(i)%ms(j)  ; call fdata%puts("      MASS:     "//trim(adjustl(line)))
        endif
        if(this%type(i)%rdi(j)>1.0E-2)then
          write(line,'(f9.3)')this%type(i)%rdi(j) ; call fdata%puts("      RADII:    "//trim(adjustl(line)))
        endif
        write(line,'(f9.3)')this%type(i)%chg(j)     ; call fdata%puts("      CHARGE:   "//trim(adjustl(line)))
        call fdata%puts("      ORBITAL:  "//this%type(i)%orbit(j))
      enddo
    enddo
    call fdata%puts("---")
    call fdata%quit()
    RETURN
  end subroutine ExportElementData
!
  pure function PtNmfromN(this,N) result(res)
  class(periodic_table),intent(in)   :: this
  character(LenAtomName),allocatable :: res(:)
  integer,intent(in)                 :: N
    if(this%length<N)then
      res = DummySymbol ; RETURN
    endif
    res = this%name(N)%lookup()
  end function PtNmfromN
!
  pure function PtAs(this) result(res)
  class(periodic_table),intent(in)     :: this
  character(LenAtomSymbol),allocatable :: res(:)
    allocate(res(1:this%length)) ; res = this%symbol(1:this%length)
  end function PtAs
!
  elemental function PtAsfromN(this,N) result(res)
  class(periodic_table),intent(in)  :: this
  integer,intent(in)                :: N
  character(LenAtomSymbol)          :: res
    if(this%length<N)then
      res = DummySymbol ; RETURN
    endif
    res = this%symbol(N)
  end function PtAsfromN
!
  pure function PtMs(this) result(res)
  class(periodic_table),intent(in)     :: this
  double precision,allocatable         :: res(:)
    allocate(res(1:this%length)) ; res = this%ms(1:this%length)
  end function PtMs
!
  elemental function PtMsfromN(this,N) result(res)
  class(periodic_table),intent(in) :: this
  integer,intent(in)               :: N
  double precision                 :: res
    if(this%length<N)then
      res = DummyMass ; RETURN
    endif
    res = this%ms(N)
  end function PtMsfromN
!
  pure function PtRdi(this) result(res)
  class(periodic_table),intent(in)     :: this
  double precision,allocatable         :: res(:)
    allocate(res(1:this%length)) ; res = this%rdi(1:this%length)
  end function PtRdi
!
  elemental function PtRdifromN(this,N) result(res)
  class(periodic_table),intent(in) :: this
  integer,intent(in)               :: N
  double precision                 :: res
    if(this%length<N)then
      res = DummyRadii ; RETURN
    endif
    res = this%rdi(N)
  end function PtRdifromN
!
  elemental function AtmNameFromAtomicNum(AtmicNum) result(res)
  integer,intent(in) :: AtmicNum
  character(2)       :: res
    select case(AtmicNum)
    case(1)      ; res = " H"
    case(2)      ; res = "He"
    case(3)      ; res = "Li"
    case(4)      ; res = "Be"
    case(5)      ; res = " B"
    case(6)      ; res = " C"
    case(7)      ; res = " N"
    case(8)      ; res = " O"
    case(9)      ; res = " F"
    case(10)     ; res = "Ne"
    case(11)     ; res = "Na"
    case(12)     ; res = "Mg"
    case(13)     ; res = "Al"
    case(14)     ; res = "Si"
    case(15)     ; res = " P"
    case(16)     ; res = " S"
    case(17)     ; res = "Cl"
    case(18)     ; res = "Ar"
    case(19)     ; res = " K"
    case(20)     ; res = "Ca"
    case(21)     ; res = "Sc"
    case(22)     ; res = "Ti"
    case(23)     ; res = " V"
    case(24)     ; res = "Cr"
    case(25)     ; res = "Mn"
    case(26)     ; res = "Fe"
    case(27)     ; res = "Co"
    case(28)     ; res = "Ni"
    case(29)     ; res = "Cu"
    case(30)     ; res = "Zn"
    case(31)     ; res = "Ga"
    case(32)     ; res = "Ge"
    case(33)     ; res = "As"
    case(34)     ; res = "Se"
    case(35)     ; res = "Br"
    case(36)     ; res = "Kr"
    case(53)     ; res = " I"
    case(54)     ; res = "Xe"
    case default ; res = "Xx"
    end select
    RETURN
end function AtmNameFromAtomicNum

elemental function MassFromAtomicNum(AtmicNum) result(res)
integer,intent(in) :: AtmicNum
double precision   :: res
  select case(AtmicNum)
  case(1)      ; res =  1.0d0
  case(2)      ; res =  4.0d0
  case(3)      ; res =  7.0d0
  case(4)      ; res =  9.0d0
  case(5)      ; res = 11.0d0
  case(6)      ; res =  1.0d0
  case(7)      ; res =  1.0d0
  case(8)      ; res =  1.0d0
  case(9)      ; res =  1.0d0
  case(10)     ; res =  1.0d0
  case(11)     ; res =  1.0d0
  case(12)     ; res =  1.0d0
  case(13)     ; res =  1.0d0
  case(14)     ; res =  1.0d0
  case(15)     ; res =  1.0d0
  case(16)     ; res =  1.0d0
  case(17)     ; res =  1.0d0
  case(18)     ; res =  1.0d0
  case(19)     ; res =  1.0d0
  case(20)     ; res =  1.0d0
  case(21)     ; res =  1.0d0
  case(22)     ; res =  1.0d0
  case(23)     ; res =  1.0d0
  case(24)     ; res =  1.0d0
  case(25)     ; res =  1.0d0
  case(26)     ; res =  1.0d0
  case(27)     ; res =  1.0d0
  case(28)     ; res =  1.0d0
  case(29)     ; res =  1.0d0
  case(30)     ; res =  1.0d0
  case(31)     ; res =  1.0d0
  case(32)     ; res =  1.0d0
  case(33)     ; res =  1.0d0
  case(34)     ; res =  1.0d0
  case(35)     ; res =  1.0d0
  case(36)     ; res =  1.0d0
  case(53)     ; res =  1.0d0
  case(38)     ; res =  1.0d0
  case default ; res =  0.0d0
  end select
  RETURN
end function MassFromAtomicNum

!subroutine GetPropertyFromAtomicNumber(this,ATM,TYPES,MASS,VDW,ELEM)
!class(molinfo),intent(in)         :: this
!character(LenAtmName),intent(out) :: ATM(this%NAtm)
!character(LenAtmType),intent(out) :: TYPES(this%Natm)
!real,intent(out)                  :: MASS(this%Natm),VDW(this%Natm)
!integer,intent(out)               :: ELEM(this%Natm)
!integer                           :: i
!  do i = 1,this%NATM
!    if(this%Element%At(i)==1)then     ; call SetAtmH(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==2)then ;  call SetAtmHe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==3)then ;  call SetAtmLi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==4)then ;  call SetAtmBe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==5)then ;  call SetAtmB(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==6)then ;  call SetAtmC(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==7)then ;  call SetAtmN(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==8)then ;  call SetAtmO(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==9)then ;  call SetAtmF(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==10)then ; call SetAtmNe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==11)then ; call SetAtmNa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==12)then ; call SetAtmMg(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==13)then ; call SetAtmAl(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==14)then ; call SetAtmSi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==15)then ; call SetAtmP(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==16)then ; call SetAtmS(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==17)then ; call SetAtmCl(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==18)then ; call SetAtmAr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==19)then ; call SetAtmK(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    elseif(this%Element%At(i)==20)then ; call SetAtmCa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==21)then ; call SetAtmSc(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==22)then ; call SetAtmTi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==23)then ; call SetAtmV(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==24)then ; call SetAtmCr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==25)then ; call SetAtmMn(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==26)then ; call SetAtmFe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==27)then ; call SetAtmCo(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==28)then ; call SetAtmNi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==29)then ; call SetAtmCu(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==30)then ; call SetAtmZn(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==31)then ; call SetAtmGa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==32)then ; call SetAtmGe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==33)then ; call SetAtmAs(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==34)then ; call SetAtmSe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==35)then ; call SetAtmBr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==36)then ; call SetAtmKr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==53)then ; call SetAtmI(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!   elseif(this%Element%At(i)==54)then ; call SetAtmXe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    else ; call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    endif
!  enddo
!  RETURN
!end subroutine GetPropertyFromAtomicNumber
!!
!subroutine GetPropertyFromAtmName(this,ATM,TYPES,MASS,VDW,ELEM)
!class(molinfo),intent(in)         :: this
!character(LenAtmName),intent(out) :: ATM(this%NAtm)
!character(LenAtmType),intent(out) :: TYPES(this%Natm)
!real,intent(out)                  :: MASS(this%Natm),VDW(this%Natm)
!integer,intent(out)               :: ELEM(this%Natm)
!integer                           :: i
!character(2)                      :: AtmName
!  do i = 1,this%NATM
!    AtmName = this%AtmName%large(i)
!    select case(AtmName(1:1))
!    case("A")
!        select case(AtmName(2:2))
!        case("L")    ; call SetAtmAl(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("R")    ; call SetAtmAr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("B")
!        select case(AtmName(2:2))
!        case("E")    ; call SetAtmBe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmB(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("C")
!        select case(AtmName(2:2))
!!       case("A")    ; call SetAtmCa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("L")    ; call SetAtmCl(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!!       case("O")    ; call SetAtmCo(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("R")    ; call SetAtmCr(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmC(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("F")
!        select case(AtmName(2:2))
!        case("E")    ; call SetAtmFe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmF(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("H")
!        select case(AtmName(2:2))
!        case("E")    ; call SetAtmHe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!       !case("G")    ; call SetAtmHg(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmH(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("K")
!        select case(AtmName(2:2))
!        case("R")    ; call SetAtmHe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmK(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("L")
!        select case(AtmName(2:2))
!        case("I")    ; call SetAtmLi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!       !case("A")    ; call SetAtmLa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("M")
!        select case(AtmName(2:2))
!        case("G")    ; call SetAtmMg(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("N")    ; call SetAtmMn(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("N")
!        select case(AtmName(2:2))
!        case("A")    ; call SetAtmNa(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("E")    ; call SetAtmNe(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("I")    ; call SetAtmNi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmN(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("O")
!        select case(AtmName(2:2))
!        case default ; call SetAtmO(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("P")
!        select case(AtmName(2:2))
!        case default ; call SetAtmP(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!    case("S")
!        select case(AtmName(2:2))
!        case("C")    ; call SetAtmSc(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case("I")    ; call SetAtmSi(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        case default ; call SetAtmS(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!        end select
!   case("T")
!       select case(AtmName(2:2))
!       case("I")    ; call SetAtmTiATM((i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!       case default ; call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!       end select
!   case("V")
!       select case(AtmName(2:2))
!        case default ; call SetAtmV(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!       end select
!    case default
!        call SetAtmEx(ATM(i),TYPES(i),MASS(i),VDW(i),ELEM(i))
!    end select
!  enddo
!  RETURN
!end subroutine GetPropertyFromAtmName
!!
  pure subroutine ElementExtention(this)
  class(periodic_table),intent(inout)  :: this
  integer                              :: OldStack
    if(this%Stack>=this%length.and.this%Stack>0)RETURN
    if(this%Stack<=0)this%Stack = 1
    OldStack = this%Stack
    do while(this%Stack<this%length)
      this%Stack = this%Stack * 2
    enddo
!
    call move_chr(this%symbol,this%Stack,OldStack)
    call move_vchr(this%name,this%Stack,OldStack)
    call move_double(this%ms,this%Stack,OldStack)
    call move_double(this%rdi,this%Stack,OldStack)
    call move_type(this%type,this%Stack,OldStack)
    RETURN
  end subroutine ElementExtention
!
  pure subroutine TypeExtention(this)
  type(ElementType),intent(inout)  :: this
  integer                          :: OldStack
    if(this%Stack>=this%length.and.this%Stack>0)RETURN
    if(this%Stack<=0)this%Stack = 1
    OldStack = this%Stack
    do while(this%Stack<this%length)
      this%Stack = this%Stack * 2
    enddo
!
    call move_chr(this%name,this%Stack,OldStack)
    call move_chr(this%orbit,this%Stack,OldStack)
    call move_double(this%ms,this%Stack,OldStack)
    call move_double(this%rdi,this%Stack,OldStack)
    call move_double(this%chg,this%Stack,OldStack)
    RETURN
  end subroutine TypeExtention
!
  pure subroutine move_chr(v,new,old)
  character(*),intent(inout),allocatable :: v(:)
  integer,intent(in)                     :: new,old
  character(len(v)),allocatable          :: Tmp(:)
    allocate(Tmp(new)) ; if(allocated(v))Tmp(1:old) = v(1:old) ; call move_alloc(Tmp,v)
  end subroutine move_chr
!
  pure subroutine move_vchr(v,new,old)
  type(vector_character),intent(inout),allocatable :: v(:)
  integer,intent(in)                               :: new,old
  type(vector_character),allocatable               :: Tmp(:)
    allocate(Tmp(new)) ; if(allocated(v))Tmp(1:old) = v(1:old) ; call move_alloc(Tmp,v)
  end subroutine move_vchr
!
  pure subroutine move_int(v,new,old)
  integer,intent(inout),allocatable :: v(:)
  integer,intent(in)                :: new,old
  integer,allocatable               :: Tmp(:)
    allocate(Tmp(new)) ; if(allocated(v))Tmp(1:old) = v(1:old) ; call move_alloc(Tmp,v)
  end subroutine move_int
!
  pure subroutine move_double(v,new,old)
  double precision,intent(inout),allocatable :: v(:)
  integer,intent(in)                         :: new,old
  double precision,allocatable               :: Tmp(:)
    allocate(Tmp(new)) ; if(allocated(v))Tmp(1:old) = v(1:old) ; call move_alloc(Tmp,v)
  end subroutine move_double
!
  pure subroutine move_type(v,new,old)
  type(ElementType),intent(inout),allocatable :: v(:)
  integer,intent(in)                          :: new,old
  type(ElementType),allocatable               :: Tmp(:)
    allocate(Tmp(new))
    if(allocated(v)) call mvtype(v(1:old),Tmp(1:old))
    call move_alloc(Tmp,v)
  contains
    elemental subroutine mvtype(from,to)
    type(ElementType),intent(inout) :: from,to
      call move_alloc(from%name,to%name)
      call move_alloc(from%orbit,to%orbit)
      call move_alloc(from%rdi,to%rdi)
      call move_alloc(from%ms,to%ms)
      call move_alloc(from%chg,to%chg)
    end subroutine mvtype
  end subroutine move_type
!
  pure subroutine PtDestractor(this)
  type(periodic_table),intent(inout)  :: this
    this%length = 0 ; this%stack = -1
    if(allocated(this%symbol))deallocate(this%symbol)
    if(allocated(this%name))deallocate(this%name)
    if(allocated(this%rdi))deallocate(this%rdi)
    if(allocated(this%ms))deallocate(this%ms)
    if(allocated(this%type))call ElDestractor(this%Type(:))
    RETURN
  end subroutine PtDestractor

  elemental subroutine ElDestractor(this)
  type(ElementType),intent(inout)  :: this
    if(allocated(this%name))deallocate(this%name)
    if(allocated(this%orbit))deallocate(this%orbit)
    if(allocated(this%rdi))deallocate(this%rdi)
    if(allocated(this%ms))deallocate(this%ms)
    if(allocated(this%chg))deallocate(this%chg)
    RETURN
  end subroutine ElDestractor
end module periodic_tables
