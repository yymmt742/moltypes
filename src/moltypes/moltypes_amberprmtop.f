module moltypes_amberprmtop
  use moltypes_errorhandler
  use spur_vector
  use spur_pathname
  use spur_stdio
  implicit none
  private
  public :: AmberPrmtop
!
  integer,parameter       :: NLINE     = 80
  integer,parameter       :: NWLEN     =  4
!
  integer,parameter       :: NTITLE    = 20
  integer,parameter       :: NRADSET   =  1
  integer,parameter       :: NSOLPTR   =  3
  integer,parameter       :: NOLDDIM   =  4
  integer,parameter       :: NNATCAP   =  5
  integer,parameter       :: NCUTCAP   =  4
!
  integer,parameter       :: NNULL     = -1
!
  integer,parameter       :: NPOINTERS = 31
  integer,parameter       :: NATOM     = 1,  NTYPES    = 2
  integer,parameter       :: NBONH     = 3,  MBONA     = 4
  integer,parameter       :: NTHETH    = 5,  MTHETA    = 6
  integer,parameter       :: NPHIH     = 7,  MPHIA     = 8
  integer,parameter       :: NHPARM    = 9,  NPARM     = 10
  integer,parameter       :: NNB       = 11, NRES      = 12
  integer,parameter       :: NBONA     = 13, NTHETA    = 14
  integer,parameter       :: NPHIA     = 15, NUMBND    = 16
  integer,parameter       :: NUMANG    = 17, NPTRA     = 18
  integer,parameter       :: NATYP     = 19, NPHB      = 20
  integer,parameter       :: IFPERT    = 21, NBPER     = 22
  integer,parameter       :: NGPER     = 23, NDPER     = 24
  integer,parameter       :: MBPER     = 25, MGPER     = 26
  integer,parameter       :: MDPER     = 27, IFBOX     = 28
  integer,parameter       :: NMXRS     = 29, IFCAP     = 30
  integer,parameter       :: NUMEXTRA  = 31, NCOPY     = 32
!
  integer,parameter       :: NSOLPTRS  = 3
  integer,parameter       :: IPTRES    = 1,  NSPM      = 2
  integer,parameter       :: NSPSOL    = 3
!
  integer,parameter       :: NIFPOL    = 1
  integer,parameter       :: IFPOL     = 1
!
  type :: AmberPrmtop
    type(pathname)                      :: path
    integer                             :: stat  = MOLTYPES_NOERR
    logical,public                      :: terminates_at_abnormal = terminates_default
    character(:),allocatable,public     :: VERSION
    character(NWLEN),allocatable,public :: TITLE(:),IGRAPH(:),LBRES(:),LABRES(:)
    character(NWLEN),allocatable,public :: IGRPER(:),ISMPER(:),ALMPER(:)
    character(NWLEN),allocatable,public :: ISYMBL(:),ITREE(:)
    character(NLINE),allocatable,public :: TYPE(:)
    double precision,allocatable,public :: AMASS(:),CHARGE(:),RBORN(:)
    double precision,allocatable,public :: RK(:),REQ(:),TK(:),TEQ(:)
    double precision,allocatable,public :: PK(:),PN(:),PHASE(:)
    double precision,allocatable,public :: ONE_SCEE(:),ONE_SCNB(:)
    double precision,allocatable,public :: SOLTY(:),ASOL(:),BSOL(:)
    double precision,allocatable,public :: CN1(:),CN2(:),HBCUT(:),FS(:)
    double precision,allocatable,public :: CGPER(:),ATPOL(:),ATPOL1(:)
    double precision,allocatable,public :: OLDBETA(:),CUTCAP(:)
    integer,allocatable,public          :: POINTERS(:),SOLPTRS(:),IPOL(:)
    integer,allocatable,public          :: ATNUM(:),RESIDUE(:)
    integer,allocatable,public          :: IAC(:),NUMEX(:),ICO(:),IPRES(:)
    integer,allocatable,public          :: ICBH(:),ICB(:),ICTH(:),ICT(:)
    integer,allocatable,public          :: ICPH(:),ICP(:),INB(:)
    integer,allocatable,public          :: JOIN(:),IROTAT(:),NSP(:),NATCAP(:)
    integer,allocatable,public          :: BPER(:),ICBPER(:),PTER(:),ICTPER(:)
    integer,allocatable,public          :: PPER(:),ICPPER(:),IAPER(:),IACPER(:)
  contains
    procedure         :: load         => LoadAmberPrmtop
    procedure         :: export       => ExportAmberPrmtop
    procedure         :: natoms       => AmbPrmNatoms
    procedure         :: ntypes       => AmbPrmNtypes
    procedure         :: nresidues    => AmbPrmNresidues
    procedure         :: resid        => AmbPrmResid
    procedure         :: resname      => AmbPrmResname
    procedure         :: isErr        => AmbPrmIsErr
    procedure         :: clear        => AmbPrmClear
    final             :: AmbPrmDestractor
  end type AmberPrmtop
!
  contains
    subroutine LoadAmberPrmtop(this,path)
    use spur_string
    class(AmberPrmtop),intent(inout)  :: this
    character(*),intent(in)           :: path
    type(stdio)                       :: fdata
    character(NLINE)                  :: line,FLAG,CFMT
    integer                           :: i,j,is,NNONBOND
      call AmbPrmDestractor(this) ; call this%path%fetch(path)
      call RoutineNameIs('LOAD_AMBER_PRMTOP')
      if(CheckAmbPrm(this,this%path%isnotExist(),IO_NOTEXIST)) RETURN
!
      fdata%terminates_at_abnormal = this%terminates_at_abnormal
      call fdata%fetch(path) ; call fdata%connect()
      if(CheckAmbPrm(this,fdata%iserr(),IO_CANNOTOPEN)) RETURN
!
      allocate(character(NLINE)::this%VERSION) ; this%VERSION  = fdata%streamseek("%VERSION")
!
      line = fdata%streamseek("%FLAG POINTERS")
      line = fdata%streamseek("%FORMAT") ; CFMT = trim(line(8:))
      allocate(this%POINTERS(NPOINTERS))
      read(fdata%devn(),CFMT,err=101) this%POINTERS
      NNONBOND = this%POINTERS(NTYPES)*(this%POINTERS(NTYPES)+1)/2
!
      if(this%POINTERS(IFBOX)>0)then
        line = fdata%streamseek("%FLAG SOLVENT_POINTERS")
        line = fdata%streamseek("%FORMAT") ; CFMT = trim(line(8:))
        allocate(this%SOLPTRS(NSOLPTRS))
        read(fdata%devn(),CFMT,err=101) this%SOLPTRS
      endif
!
      line = fdata%streamseek("%FLAG IPOL")
      if(line/='')then
        line = fdata%streamseek("%FORMAT") ; CFMT = trim(line(8:))
        allocate(this%IPOL(NIFPOL))
        read(fdata%devn(),CFMT,err=101) this%IPOL
      endif
!
      call fdata%quit() ; call fdata%connect()
!
      do
        line = fdata%streamseek("%FLAG")   ; FLAG = LARGE(line(7:))
        if(FLAG=='')EXIT
        line = fdata%streamseek("%FORMAT") ; CFMT = trim(line(8:))
        select case(FLAG)
          case("TITLE")                      ; call ReadPrmA(this%TITLE,    NTITLE)
          case("ATOM_NAME")                  ; call ReadPrmA(this%IGRAPH,   this%POINTERS(NATOM))
          case("CHARGE")                     ; call ReadPrmF(this%CHARGE,   this%POINTERS(NATOM))
          case("ATOMIC_NUMBER")              ; call ReadPrmI(this%ATNUM,    this%POINTERS(NATOM))
          case("MASS")                       ; call ReadPrmF(this%AMASS,    this%POINTERS(NATOM))
          case("ATOM_TYPE_INDEX")            ; call ReadPrmI(this%IAC,      this%POINTERS(NATOM))
          case("NUMBER_EXCLUDED_ATOMS")      ; call ReadPrmI(this%NUMEX,    this%POINTERS(NATOM))
          case("NONBONDED_PARM_INDEX")       ; call ReadPrmI(this%ICO,      this%POINTERS(NTYPES)**2)
          case("RESIDUE_LABEL")              ; call ReadPrmA(this%LBRES,    this%POINTERS(NRES))
          case("RESIDUE_POINTER")            ; call ReadPrmI(this%IPRES,    this%POINTERS(NRES))
          case("BOND_FORCE_CONSTANT")        ; call ReadPrmF(this%RK,       this%POINTERS(NUMBND))
          case("BOND_EQUIL_VALUE")           ; call ReadPrmF(this%REQ,      this%POINTERS(NUMBND))
          case("ANGLE_FORCE_CONSTANT")       ; call ReadPrmF(this%TK,       this%POINTERS(NUMANG))
          case("ANGLE_EQUIL_VALUE")          ; call ReadPrmF(this%TEQ,      this%POINTERS(NUMANG))
          case("DIHEDRAL_FORCE_CONSTANT")    ; call ReadPrmF(this%PK,       this%POINTERS(NPTRA))
          case("DIHEDRAL_PERIODICITY")       ; call ReadPrmF(this%PN,       this%POINTERS(NPTRA))
          case("DIHEDRAL_PHASE")             ; call ReadPrmF(this%PHASE,    this%POINTERS(NPTRA))
          case("SCEE_SCALE_FACTOR")          ; call ReadPrmF(this%ONE_SCEE, this%POINTERS(NPTRA))
          case("SCNB_SCALE_FACTOR")          ; call ReadPrmF(this%ONE_SCNB, this%POINTERS(NPTRA))
          case("SOLTY")                      ; call ReadPrmF(this%SOLTY,    this%POINTERS(NATYP))
          case("LENNARD_JONES_ACOEF")        ; call ReadPrmF(this%CN1,      NNONBOND)
          case("LENNARD_JONES_BCOEF")        ; call ReadPrmF(this%CN2,      NNONBOND)
          case("BONDS_INC_HYDROGEN")         ; call ReadPrmI(this%ICBH,     this%POINTERS(NBONH)*3)
          case("BONDS_WITHOUT_HYDROGEN")     ; call ReadPrmI(this%ICB,      this%POINTERS(NBONA)*3)
          case("ANGLES_INC_HYDROGEN")        ; call ReadPrmI(this%ICTH,     this%POINTERS(NTHETH)*4)
          case("ANGLES_WITHOUT_HYDROGEN")    ; call ReadPrmI(this%ICT,      this%POINTERS(NTHETA)*4)
          case("DIHEDRALS_INC_HYDROGEN")     ; call ReadPrmI(this%ICPH,     this%POINTERS(NPHIH)*5)
          case("DIHEDRALS_WITHOUT_HYDROGEN") ; call ReadPrmI(this%ICP,      this%POINTERS(NPHIA)*5)
          case("EXCLUDED_ATOMS_LIST")        ; call ReadPrmI(this%INB,      this%POINTERS(NNB))
          case("HBOND_ACOEF")                ; call ReadPrmF(this%ASOL,     this%POINTERS(NPHB))
          case("HBOND_BCOEF")                ; call ReadPrmF(this%BSOL,     this%POINTERS(NPHB))
          case("HBCUT")                      ; call ReadPrmF(this%HBCUT,    this%POINTERS(NPHB))
          case("AMBER_ATOM_TYPE")            ; call ReadPrmA(this%ISYMBL,   this%POINTERS(NATOM))
          case("TREE_CHAIN_CLASSIFICATION")  ; call ReadPrmA(this%ITREE,    this%POINTERS(NATOM))
          case("JOIN_ARRAY")                 ; call ReadPrmI(this%JOIN,     this%POINTERS(NATOM))
          case("IROTAT")                     ; call ReadPrmI(this%IROTAT,   this%POINTERS(NATOM))
          case("RADIUS_SET")                 ; call ReadPrmA(this%TYPE,     NRADSET)
          case("RADII")                      ; call ReadPrmF(this%RBORN,    this%POINTERS(NATOM))
          case("SCREEN")                     ; call ReadPrmF(this%FS,       this%POINTERS(NATOM))
          case("ATOMS_PER_MOLECULE")         ; call ReadPrmI(this%NSP,      this%POINTERS(NSPM))
          case("BOX_DIMENSIONS")             ; call ReadPrmF(this%OLDBETA,  NOLDDIM)
          case("CAP_INFO")                   ; call ReadPrmI(this%NATCAP,   NNATCAP)
          case("CAP_INFO2")                  ; call ReadPrmF(this%CUTCAP,   NCUTCAP)
          case("POLARIZABILITY")             ; call ReadPrmF(this%ATPOL,    this%POINTERS(NATOM))
          case default
        end select
100     if(CheckAmbPrm(this,is>0,IO_READERR)) EXIT
      enddo
101   call fdata%quit()
  contains
    subroutine ReadPrmI(A,N)
    integer,intent(inout),allocatable :: A(:)
    integer,intent(in)                :: N
      if(N<=0)RETURN ; if(allocated(A))deallocate(A) ; allocate(A(N))
      read(fdata%devn(),CFMT,iostat=is) A
    end subroutine ReadPrmI
!
    subroutine ReadPrmF(A,N)
    double precision,intent(inout),allocatable :: A(:)
    integer,intent(in)                         :: N
      if(N<=0)RETURN ; if(allocated(A))deallocate(A) ; allocate(A(N))
      read(fdata%devn(),CFMT,iostat=is) A
    end subroutine ReadPrmF
!
    subroutine ReadPrmA(A,N)
    character(*),intent(inout),allocatable :: A(:)
    integer,intent(in)                     :: N
      if(N<=0)RETURN ; if(allocated(A))deallocate(A) ; allocate(A(N))
      read(fdata%devn(),CFMT,iostat=is) A
    end subroutine ReadPrmA
  end subroutine LoadAmberPrmtop
!
  pure integer function AmbPrmNatoms(this) result(res)
  class(AmberPrmtop),intent(in) :: this
    if(allocated(this%POINTERS))then ; res = this%POINTERS(NATOM)
    else ; res = NNULL ; endif
  end function AmbPrmNatoms
!
  pure integer function AmbPrmNtypes(this) result(res)
  class(AmberPrmtop),intent(in) :: this
    if(allocated(this%POINTERS))then ; res = this%POINTERS(NTYPES)
    else ; res = NNULL ; endif
  end function AmbPrmNtypes
!
  pure integer function AmbPrmNresidues(this) result(res)
  class(AmberPrmtop),intent(in) :: this
    if(allocated(this%POINTERS))then ; res = this%POINTERS(NRES)
    else ; res = NNULL ; endif
  end function AmbPrmNresidues
!
  pure function AmbPrmResid(this) result(res)
  class(AmberPrmtop),intent(in) :: this
  integer,allocatable           :: res(:)
  integer                       :: i
    if(.not.allocated(this%IPRES))then
      allocate(res(0)) ; res = 0 ; RETURN
    else
      allocate(res(this%natoms()))
    endif
    if(.not.allocated(this%POINTERS)) RETURN
    do i=1,this%nresidues()-1
      res(this%IPRES(i):this%IPRES(i+1)-1) = i
    enddo
    res(this%IPRES(this%nresidues()):) = this%nresidues()
  end function AmbPrmResid
!
  pure function AmbPrmResname(this) result(res)
  class(AmberPrmtop),intent(in) :: this
  character(4),allocatable      :: res(:)
  integer                       :: i
    if(.not.allocated(this%IPRES))then
      allocate(res(0)) ; res = '' ; RETURN
    else
      allocate(res(this%natoms()))
    endif
    do i=1,this%POINTERS(NRES)-1
      res(this%IPRES(i):this%IPRES(i+1)-1) = this%LBRES(i)
    enddo
    res(this%IPRES(this%POINTERS(NRES)):) = this%LBRES(this%POINTERS(NRES))
  end function AmbPrmResname
!
  subroutine ExportAmberPrmtop(this,fname,mask)
  use spur_shapeshifter
  use spur_string
  class(AmberPrmtop),intent(inout)  :: this
  character(*),intent(in)           :: fname
  logical,intent(in),optional       :: mask(:)
  type(stdio)                       :: fdata
  character(80)                     :: line
  logical,allocatable               :: lmask(:)
  integer                           :: pointers(NPOINTERS)
  integer                           :: ckey(this%natoms()),date(8)
  type(vector_character)            :: rname,rlabel
  type(vector_integer)              :: rid,rpnt,apm,nex,iac,ico,uac
  type(vector_integer)              :: bonh,bona,angh,anga,dihh,diha
  integer                           :: i,j,k,is
    call RoutineNameIs('EXPORT_AMBERPERMTOP')
    if(CheckAmbPrm(this,this%natoms()==0,IO_EMPTYATOM)) RETURN
    allocate(lmask(this%natoms())) ; lmask = .TRUE.
!
    if(present(mask)) lmask = CompleteMask(mask,this%natoms())
!
    pointers = this%POINTERS
!
    pointers(NATOM) = count(lmask)
    if(CheckAmbPrm(this,pointers(NATOM)==0,IO_NATOMERR)) RETURN
!
    rid   = pack(this%Resid(),lmask)
    rname = pack(this%resname(),lmask)
!
    pointers(NRES) = size(rid%uniq())
!
    j = 0 ; k = 1
    do i=1,pointers(NATOM)
      if(rid%at(i)>j)then
        j = rid%at(i)
        call rpnt%push(i)
        call rlabel%push(rname%at(i))
        if(k<i) call apm%push(i-k)
        k = i
      endif
    enddo
    call apm%push(pointers(NATOM)-k+1)
    pointers(NMXRS) = maxval(apm%lookup(),1)
!
    iac = pack(this%IAC,lmask)
    uac = iac%uniq()
    pointers(NTYPES) = uac%size()
    nex   = pack(this%NUMEX,lmask)
!
    do j=1,pointers(NTYPES)
      do i=1,pointers(NTYPES)
        call ico%push(this%ICO(pointers(NTYPES)*(uac%at(i)-1)+uac%at(j)))
      enddo
    enddo
!
    j = 0
    do i=1,this%natoms()
      if(lmask(i))then ; ckey(i) = j ; j = j + 1
      else             ; ckey(i)=0 ; endif
    enddo
!
    do i=1,this%POINTERS(NBONH)*3,3
      if(lmask(this%ICBH(i)/3+1).and.lmask(this%ICBH(i+1)/3+1))then
        call bonh%push(ToNewid(ckey,this%ICBH(i)))
        call bonh%push(ToNewid(ckey,this%ICBH(i+1)))
        call bonh%push(this%ICBH(i+2))
      endif
    enddo
!
    do i=1,this%POINTERS(NBONA)*3,3
      if(lmask(this%ICB(i)/3+1).and.lmask(this%ICB(i+1)/3+1))then
        call bona%push(ToNewid(ckey,this%ICB(i)))
        call bona%push(ToNewid(ckey,this%ICB(i+1)))
        call bona%push(this%ICB(i+2))
      endif
    enddo
!
    do i=1,this%POINTERS(NTHETH)*4,4
      if(lmask(this%ICTH(i)/3+1).and.&
        &lmask(this%ICTH(i+1)/3+1).and.&
        &lmask(this%ICTH(i+2)/3+1))then
        call angh%push(ToNewid(ckey,this%ICTH(i)))
        call angh%push(ToNewid(ckey,this%ICTH(i+1)))
        call angh%push(ToNewid(ckey,this%ICTH(i+2)))
        call angh%push(this%ICTH(i+3))
      endif
    enddo
!
    do i=1,this%POINTERS(NTHETA)*4,4
      if(lmask(this%ICT(i)/3+1).and.&
        &lmask(this%ICT(i+1)/3+1).and.&
        &lmask(this%ICT(i+2)/3+1))then
        call anga%push(ToNewid(ckey,this%ICT(i)))
        call anga%push(ToNewid(ckey,this%ICT(i+1)))
        call anga%push(ToNewid(ckey,this%ICT(i+2)))
        call anga%push(this%ICT(i+3))
      endif
    enddo
!
    do i=1,this%POINTERS(NPHIH)*5,5
      if(lmask(abs(this%ICPH(i))/3+1).and.&
        &lmask(abs(this%ICPH(i+1))/3+1).and.&
        &lmask(abs(this%ICPH(i+2))/3+1).and.&
        &lmask(abs(this%ICPH(i+3))/3+1))then
        call dihh%push(ToNewid(ckey,this%ICPH(i)))
        call dihh%push(ToNewid(ckey,this%ICPH(i+1)))
        call dihh%push(ToNewid(ckey,this%ICPH(i+2)))
        call dihh%push(ToNewid(ckey,this%ICPH(i+3)))
        call dihh%push(this%ICPH(i+4))
      endif
    enddo
!
    do i=1,this%POINTERS(NPHIA)*5,5
      if(lmask(abs(this%ICP(i))/3+1).and.&
        &lmask(abs(this%ICP(i+1))/3+1).and.&
        &lmask(abs(this%ICP(i+2))/3+1).and.&
        &lmask(abs(this%ICP(i+3))/3+1))then
        call diha%push(ToNewid(ckey,this%ICP(i)))
        call diha%push(ToNewid(ckey,this%ICP(i+1)))
        call diha%push(ToNewid(ckey,this%ICP(i+2)))
        call diha%push(ToNewid(ckey,this%ICP(i+3)))
        call diha%push(this%ICP(i+4))
      endif
    enddo
!
    pointers(NBONH)  = bonh%size()/3 ; pointers(MBONA)  = bona%size()/3 ; pointers(NBONA)  = bona%size()/3
    pointers(NTHETH) = angh%size()/4 ; pointers(MTHETA) = anga%size()/4 ; pointers(NTHETA) = anga%size()/4
    pointers(NPHIH)  = dihh%size()/5 ; pointers(MPHIA)  = diha%size()/5 ; pointers(NPHIA)  = diha%size()/5
    call date_and_time(values=date)
!
    call fdata%fetch(fname)
    if(fdata%is()/='') call fdata%generate()
!
    write(line,'(A,I2.2,A,I2.2,A,I2.2,X,I2.2,A,I2.2,A,I2.2)')&
          &'%VERSION  VERSION_STAMP = V0001.000  DATE = ',&
          &date(3),'/',date(2),'/',date(1)-2000,date(5),':',&
          &date(6),':',date(7)
    call fdata%puts(line)
    call WritePrmA(this%TITLE,"TITLE")
!
    line = "%FLAG POINTERS"      ; call fdata%puts(line)
    line = "%FORMAT(10I8)"       ; call fdata%puts(line)
!
    write(fdata%devn(),'(10I8)',iostat=is) pointers
!
    call WritePrmA(pack(this%IGRAPH,lmask), "ATOM_NAME")
    call WritePrmF(pack(this%CHARGE,lmask), "CHARGE")
    call WritePrmI(pack(this%ATNUM,lmask),  "ATOMIC_NUMBER")
    call WritePrmF(pack(this%AMASS,lmask),  "MASS")
    call WritePrmI(iac%lookup(),            "ATOM_TYPE_INDEX")
    call WritePrmI(nex%lookup(),            "NUMBER_EXCLUDED_ATOMS")
    call WritePrmI(ico%lookup(),            "NONBONDED_PARM_INDEX")
    call WritePrmA(rlabel%lookup(),         "RESIDUE_LABEL")
    call WritePrmI(rpnt%lookup(),           "RESIDUE_POINTER")
    call WritePrmF(this%RK,                 "BOND_FORCE_CONSTANT")
    call WritePrmF(this%REQ,                "BOND_EQUIL_VALUE")
    call WritePrmF(this%TK,                 "ANGLE_FORCE_CONSTANT")
    call WritePrmF(this%TEQ,                "ANGLE_EQUIL_VALUE")
    call WritePrmF(this%PK,                 "DIHEDRAL_FORCE_CONSTANT")
    call WritePrmF(this%PN,                 "DIHEDRAL_PERIODICITY")
    call WritePrmF(this%PHASE,              "DIHEDRAL_PHASE")
    call WritePrmF(this%ONE_SCEE,           "SCEE_SCALE_FACTOR")
    call WritePrmF(this%ONE_SCNB,           "SCNB_SCALE_FACTOR" )
    call WritePrmF(this%SOLTY,              "SOLTY")
    call WritePrmF(this%CN1,                "LENNARD_JONES_ACOEF")
    call WritePrmF(this%CN2,                "LENNARD_JONES_BCOEF")
    call WritePrmI(bonh%lookup(),           "BONDS_INC_HYDROGEN")
    call WritePrmI(bona%lookup(),           "BONDS_WITHOUT_HYDROGEN")
    call WritePrmI(angh%lookup(),           "ANGLES_INC_HYDROGEN")
    call WritePrmI(anga%lookup(),           "ANGLES_WITHOUT_HYDROGEN")
    call WritePrmI(dihh%lookup(),           "DIHEDRALS_INC_HYDROGEN")
    call WritePrmI(diha%lookup(),           "DIHEDRALS_WITHOUT_HYDROGEN")
    call WritePrmI(this%INB,                "EXCLUDED_ATOMS_LIST")
    call WritePrmF(this%ASOL,               "HBOND_ACOEF")
    call WritePrmF(this%BSOL,               "HBOND_BCOEF")
    call WritePrmF(this%HBCUT,              "HBCUT")
    call WritePrmA(pack(this%ISYMBL,lmask), "AMBER_ATOM_TYPE")
    call WritePrmA(this%ITREE,              "TREE_CHAIN_CLASSIFICATION")
    call WritePrmI(this%JOIN,               "JOIN_ARRAY")
    call WritePrmI(this%IROTAT,             "IROTAT")
!
    if(this%POINTERS(IFBOX)>0.and.allocated(this%SOLPTRS))then
      call WritePrmI(this%SOLPTRS,  "SOLVENT_POINTERS")
      call WritePrmI(apm%lookup(),  "ATOMS_PER_MOLECULE")
      call WritePrmF(this%OLDBETA,  "BOX_DIMENSIONS")
    endif
!
    call WritePrmA(this%TYPE,     "RADIUS_SET")
    call WritePrmF(this%RBORN,    "RADII")
    call WritePrmF(this%FS,       "SCREEN")
!
    if(allocated(this%IPOL))then
      call WritePrmI(this%NATCAP,   "CAP_INFO")
      call WritePrmF(this%CUTCAP,   "CAP_INFO2")
      line = "%FLAG IPOL"             ; call fdata%puts(line)
      line = "%FORMAT(1I8)"           ; call fdata%puts(line)
      write(fdata%devn(),'(1I8)',iostat=is) this%IPOL(IFPOL)
      if(this%IPOL(IFPOL)==1) call WritePrmF(this%ATPOL,    "POLARIZABILITY")
    endif
    call fdata%quit()
  contains
    subroutine WritePrmI(A,FLAG)
    integer,intent(in)      :: A(:)
    character(*),intent(in) :: FLAG
    integer                 :: is
      line = "%FLAG "//FLAG        ; call fdata%puts(line)
      line = "%FORMAT(10I8)"       ; call fdata%puts(line)
      write(fdata%devn(),'(10I8)',iostat=is) A
      if(CheckAmbPrm(this,is/=0,IO_NOTEXIST)) RETURN
    end subroutine WritePrmI
!
    subroutine WritePrmF(A,FLAG)
    double precision,intent(in) :: A(:)
    character(*),intent(in)     :: FLAG
    integer                     :: is
      line = "%FLAG "//FLAG        ; call fdata%puts(line)
      line = "%FORMAT(5E16.8)"     ; call fdata%puts(line)
      write(fdata%devn(),'(5(1PE16.8))',iostat=is) A
      if(CheckAmbPrm(this,is/=0,IO_NOTEXIST)) RETURN
    end subroutine WritePrmF
!
    subroutine WritePrmA(A,FLAG)
    character(*),intent(in) :: A(:)
    character(*),intent(in) :: FLAG
    character(12)           :: CFMT
    integer                 :: is
      if(len(A)==80)then
        CFMT = '(1a80)'
      else
        CFMT = '(20a4)'
      endif
      line = "%FLAG "//FLAG        ; call fdata%puts(line)
      line = "%FORMAT"//trim(CFMT) ; call fdata%puts(line)
      write(fdata%devn(),CFMT,iostat=is) A
      if(CheckAmbPrm(this,is/=0,IO_NOTEXIST)) RETURN
    end subroutine WritePrmA
!
    pure integer function ToNewid(ckey,n) result(res)
    integer,intent(in) :: ckey(:),n
      res = ckey(abs(n)/3+1)*3
      if(n/=0) res = res*abs(n)/n
    end function ToNewid
  end subroutine ExportAmberPrmtop
!
  pure logical function AmbPrmIsErr(this)
  class(AmberPrmtop),intent(in)  :: this
    AmbPrmIsErr = this%stat/=MOLTYPES_NOERR
  end function AmbPrmIsErr
!
  logical function CheckAmbPrm(this,test,ierr) result(res)
  class(AmberPrmtop),intent(inout) :: this
  logical,intent(in)               :: test
  integer,intent(in)               :: ierr
    res = test ; if(.not.res) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call AmbPrmDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end function CheckAmbPrm
!
  pure subroutine AmbPrmClear(this)
  class(AmberPrmtop),intent(inout)  :: this
    call this%path%PathNameClear()
    this%stat     = MOLTYPES_NOERR
    this%terminates_at_abnormal = terminates_default
    if(allocated(this%VERSION))  deallocate(this%VERSION)
    if(allocated(this%TITLE))    deallocate(this%TITLE)    ; if(allocated(this%TYPE))     deallocate(this%TYPE)
    if(allocated(this%IGRAPH))   deallocate(this%IGRAPH)   ; if(allocated(this%LBRES))    deallocate(this%LBRES)
    if(allocated(this%LABRES))   deallocate(this%LABRES)   ; if(allocated(this%IGRPER))   deallocate(this%IGRPER)
    if(allocated(this%ISMPER))   deallocate(this%ISMPER)   ; if(allocated(this%ALMPER))   deallocate(this%ALMPER)
    if(allocated(this%ISYMBL))   deallocate(this%ISYMBL)   ; if(allocated(this%ITREE))    deallocate(this%ITREE)
    if(allocated(this%AMASS))    deallocate(this%AMASS)    ; if(allocated(this%CHARGE))   deallocate(this%CHARGE)
    if(allocated(this%RBORN))    deallocate(this%RBORN)    ; if(allocated(this%RK))       deallocate(this%RK)
    if(allocated(this%REQ))      deallocate(this%REQ)      ; if(allocated(this%TK))       deallocate(this%TK)
    if(allocated(this%TEQ))      deallocate(this%TEQ)      ; if(allocated(this%PK))       deallocate(this%PK)
    if(allocated(this%PN))       deallocate(this%PN)       ; if(allocated(this%PHASE))    deallocate(this%PHASE)
    if(allocated(this%ONE_SCEE)) deallocate(this%ONE_SCEE) ; if(allocated(this%ONE_SCNB)) deallocate(this%ONE_SCNB)
    if(allocated(this%SOLTY))    deallocate(this%SOLTY)    ; if(allocated(this%ASOL))     deallocate(this%ASOL)
    if(allocated(this%BSOL))     deallocate(this%BSOL)     ; if(allocated(this%CN1))      deallocate(this%CN1)
    if(allocated(this%HBCUT))    deallocate(this%HBCUT)    ; if(allocated(this%FS))       deallocate(this%FS)
    if(allocated(this%CGPER))    deallocate(this%CGPER)    ; if(allocated(this%ATPOL))    deallocate(this%ATPOL)
    if(allocated(this%ATPOL1))   deallocate(this%ATPOL1)   ; if(allocated(this%ATNUM))    deallocate(this%ATNUM)
    if(allocated(this%IAC))      deallocate(this%IAC)      ; if(allocated(this%NUMEX))    deallocate(this%NUMEX)
    if(allocated(this%ICO))      deallocate(this%ICO)      ; if(allocated(this%IPRES))    deallocate(this%IPRES)
    if(allocated(this%ICBH))     deallocate(this%ICBH)     ; if(allocated(this%ICB))      deallocate(this%ICB)
    if(allocated(this%ICTH))     deallocate(this%ICTH)     ; if(allocated(this%ICT))      deallocate(this%ICT)
    if(allocated(this%ICPH))     deallocate(this%ICPH)     ; if(allocated(this%ICP))      deallocate(this%ICP)
    if(allocated(this%INB))      deallocate(this%INB)      ; if(allocated(this%JOIN))     deallocate(this%JOIN)
    if(allocated(this%IROTAT))   deallocate(this%IROTAT)   ; if(allocated(this%NSP))      deallocate(this%NSP)
    if(allocated(this%NATCAP))   deallocate(this%NATCAP)   ; if(allocated(this%BPER))     deallocate(this%BPER)
    if(allocated(this%ICBPER))   deallocate(this%ICBPER)   ; if(allocated(this%PTER))     deallocate(this%PTER)
    if(allocated(this%ICTPER))   deallocate(this%ICTPER)   ; if(allocated(this%PPER))     deallocate(this%PPER)
    if(allocated(this%ICPPER))   deallocate(this%ICPPER)   ; if(allocated(this%IAPER))    deallocate(this%IAPER)
    if(allocated(this%IACPER))   deallocate(this%IACPER)   ; if(allocated(this%POINTERS)) deallocate(this%POINTERS)
    if(allocated(this%SOLPTRS))  deallocate(this%SOLPTRS)  ; if(allocated(this%IPOL))     deallocate(this%IPOL)
  end subroutine AmbPrmClear
!
  pure subroutine AmbPrmDestractor(this)
  type(AmberPrmtop),intent(inout)  :: this
    call this%clear()
  end subroutine AmbPrmDestractor
end module moltypes_amberprmtop
