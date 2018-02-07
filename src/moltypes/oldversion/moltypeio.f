  include "xyzio.f"
!  include "pdbio.f"
  include "prmtopio.f"
  include "rst7io.f"
  include "mdcrdio.f"
  include "chgio.f"
!
  subroutine TrjAssignFiles(this,FILE)
  class(trajectory),intent(inout)         :: this
  character(*),intent(in)                 :: FILE(:)
    call TrjDestractor(this) ; call TrjLoadSeq(this,FILE) ; RETURN
  end subroutine TrjAssignFiles
!
  subroutine TrjAssignFile(this,FILE)
  class(trajectory),intent(inout)         :: this
  character(*),intent(in)                 :: FILE
    call TrjDestractor(this) ; call TrjLoadOne(this,FILE) ; RETURN
  end subroutine TrjAssignFile
!
  subroutine TrjLoadSeq(this,FILE,LB,UB,STRIDE,MASK,FROM,INFO)
  class(trajectory),intent(inout)         :: this
  character(*),intent(in)                 :: FILE(:)
  integer,intent(in),optional             :: LB,UB,STRIDE,FROM
  character(*),intent(in),optional        :: MASK
  integer,intent(out),optional            :: INFO
  character(:),allocatable                :: Lmask
  type(fileio)                            :: fdata
  integer                                 :: i,lus(1:3)
  logical                                 :: ioerr
    this%top%ERR = "TRAJECTORY%LOAD"
    allocate(character(0)::Lmask) ; if(present(MASK)) Lmask = MASK
!
    do i=lbound(FILE,1),ubound(FILE,1)
      fdata = FILE(i)
      call this%top%ERR%check(ALL(fdata%ExtIs()/=MOLFMTS),"Unknown format             "//fdata%ExtIs())
      call MolinfoLoad(this,fdata,Lmask)
    enddo
!
    lus = [1,NFRAME_MAX,1]
    if(present(LB))     lus(1)  = maxval([LB,lus(1)],1)
    if(present(UB))     lus(2)  = UB
    if(present(STRIDE)) lus(3)  = maxval([STRIDE,lus(3)],1)
! 
    if(this%NTRJ<0)this%NTRJ=0
    if(present(FROM))this%NTRJ = minval([this%NTRJ,maxval([FROM-1,0],1)],1)
!
    do i=lbound(FILE,1),ubound(FILE,1)
      fdata = FILE(i) ; call TrjLoad(this,fdata,lus)
    enddo
!
    if(present(INFO))then ; INFO = this%top%ERR%punch()    ; RETURN
    else                  ; call this%top%ERR%punchabort()
    endif
  end subroutine TrjLoadSeq
!
  subroutine TrjLoadOne(this,FILE,LB,UB,STRIDE,MASK,FROM,MOLFMT,INFO)
  class(trajectory),intent(inout)         :: this
  character(*),intent(in)                 :: FILE
  integer,intent(in),optional             :: LB,UB,STRIDE,FROM
  character(*),intent(in),optional        :: MASK,MOLFMT
  integer,intent(out),optional            :: INFO
  type(fileio)                            :: fdata
  character(:),allocatable                :: Lmask
  integer                                 :: lus(3)
  logical                                 :: ioerr
    this%top%ERR = "TRAJECTORY%LOAD"
    allocate(character(0)::Lmask) ; if(present(MASK)) Lmask = MASK
    fdata = FILE ; if(present(MOLFMT)) call fdata%ChangeExt(MOLFMT)
!
    call this%top%ERR%check(ALL(fdata%ExtIs()/=MOLFMTS),"Unknown format             "//fdata%ExtIs())
!
    if(this%top%NATM<=0) call MolinfoLoad(this,fdata,Lmask)
    if(this%NTRJ<0)this%NTRJ=0
    if(present(FROM))this%NTRJ = minval([this%NTRJ,maxval([FROM-1,0],1)],1)
! 
    lus = [1,NFRAME_MAX,1]
    if(present(LB))     lus(1)  = maxval([LB,lus(1)],1)
    if(present(UB))     lus(2)  = UB
    if(present(STRIDE)) lus(3)  = maxval([STRIDE,lus(3)],1)
!
    call TrjLoad(this,fdata,lus)
!
    if(present(INFO))then ; INFO = this%top%ERR%punch()    ; RETURN
    else                  ; call this%top%ERR%punchabort()
    endif
  end subroutine TrjLoadOne
!
  subroutine MolinfoLoad(this,fdata,mask)
  class(trajectory),intent(inout) :: this
  type(fileio),intent(inout)      :: fdata
  character(*),intent(in)         :: MASK
  type(vector_integer)            :: UniqResidue
  logical                         :: ioerr
  integer                         :: i, is
    select case(fdata%ExtIs())
    case("xyz")                 ; ioerr = MolLoadXYZ(this%top,fdata)
    case('prmtop','top')        ; ioerr = MolLoadPRMTOP(this%top,fdata)
    case('charge','chg')        ; ioerr = MolLoadCHARGE(this%top,fdata)
    case default                ; RETURN
    end select
    call fdata%quit() ; if(this%top%NATM<=0)RETURN
    if(allocated(this%top%mask))RETURN
!
    if(.not.this%top%AtmName%empty()) this%top%UniqATM = this%top%AtmName%Uniq()
    if(.not.this%top%ResName%empty()) this%top%UniqRes = this%top%ResName%Uniq()
    if(.not.this%top%AtmType%empty()) this%top%UniqTyp = this%top%AtmType%Uniq()
!
    allocate(this%top%mask(this%top%NATM)) ; this%top%mask = .TRUE.
    this%top%mask = ReadMask(this%top,MASK)
!
    this%top%idx = pack([(i,i=1,this%top%NATM)],this%top%mask)
    if(.not.this%top%AtmName%empty()) this%top%AtmName = pack(this%top%AtmName%lookup(),this%top%mask)
    if(.not.this%top%ResName%empty()) this%top%ResName = pack(this%top%ResName%lookup(),this%top%mask)
    if(.not.this%top%AtmType%empty()) this%top%AtmType = pack(this%top%AtmType%lookup(),this%top%mask)
!
    if(.not.this%top%chg%empty())     this%top%chg     = pack(this%top%chg%lookup(),    this%top%mask)
    if(.not.this%top%mass%empty())    this%top%mass    = pack(this%top%mass%lookup(),   this%top%mask)
    if(.not.this%top%vdw%empty())     this%top%vdw     = pack(this%top%vdw%lookup(),    this%top%mask)
    if(.not.this%top%element%empty()) this%top%element = pack(this%top%element%lookup(),this%top%mask)
!
    if(.not.this%top%Residue%empty())then
      this%top%Residue = pack(this%top%Residue%lookup(),this%top%mask)
      UniqResidue      = this%top%Residue%Uniq()
    endif
    this%top%NRES = maxval([UniqResidue%size(),1],1)
  end subroutine MolinfoLoad
!
  subroutine TrjLoad(this,fdata,lus)
  class(trajectory),intent(inout) :: this
  type(fileio),intent(inout)      :: fdata
  integer,intent(in)              :: lus(3)
  logical                         :: ioerr
    select case(fdata%ExtIs())
    case("xyz")                 ; ioerr = TrjLoadXYZ(this,fdata,lus)
    case("rst7","restrt","rst") ; ioerr = TrjLoadRST(this,fdata)
    case("mdcrd","crd","nc")    ; ioerr = TrjLoadMDCRD(this,fdata,lus)
    case('charge','chg')        ; ioerr = MolLoadCHARGE(this%top,fdata)
    case default                ; ioerr = TrjCheckNatm(this,this%top%NATM)
    end select
    call this%top%ERR%check(ioerr, "Trajectory load failed in "//fdata%NameIs())
  end subroutine TrjLoad
!
  subroutine TrjExportOne(this,FILE,MOLFMT,MASK,APPEND,INFO)
  class(trajectory),intent(in)            :: this
  character(*),intent(in),optional        :: FILE,MOLFMT,MASK
  logical,intent(in),optional             :: APPEND
  integer,intent(out),optional            :: INFO
  type(trajectory)                        :: trj
  type(fileio)                            :: fdata
  logical                                 :: ioerr,fmterr,nerr
  integer                                 :: fmode
    fmode = 0  !!! 0 : generate new file / 1 : append file
    if(present(APPEND))then
      if(APPEND)fmode = 1
    endif
!
    if(present(FILE))fdata = FILE
    if(present(MOLFMT)) call fdata%ChangeExt(MOLFMT)
!
    trj = this
    trj%top%ERR = "TRAJECTORY%EXPORT"
!
    if(present(MASK)) trj%selection = ReadMask(trj%top,MASK)
    call TrjPack(trj)
!
    nerr = trj%top%NATM<=0
    fmterr = ALL(fdata%ExtIs()/=MOLFMTS)
!
    call trj%top%ERR%check(fmterr,"Unknown format                 "//fdata%ExtIs())
!
    if(.not.nerr.or.fmterr)call TrjExport(trj,fdata,fmode)
!
    if(present(INFO))then ; INFO = trj%top%ERR%punch()  ; RETURN
    else                  ; call trj%top%ERR%punchabort()
    endif
  end subroutine TrjExportOne
!
  subroutine TrjExportSeq(this,FILE,MOLFMT,MASK,APPEND,INFO)
  class(trajectory),intent(in)            :: this
  character(*),intent(in)                 :: FILE(:)
  character(*),intent(in),optional        :: MOLFMT,MASK
  logical,intent(in),optional             :: APPEND
  integer,intent(out),optional            :: INFO
  type(trajectory)                        :: trj
  type(fileio)                            :: fdata
  logical                                 :: fmterr,nerr
  integer                                 :: i, is, fmode
  character(:),allocatable                :: lfmt
    fmode = 0  !!! 0 : generate new file / 1 : append file
    if(present(APPEND))then
      if(APPEND)fmode = 1
    endif
!
    trj = this
    trj%top%ERR = "TRAJECTORY%EXPORT"
!
    if(present(MASK)) trj%selection = ReadMask(trj%top,MASK)
    call TrjPack(trj)
!
    if(present(MOLFMT)) call fdata%ChangeExt(MOLFMT)
    nerr = trj%top%NATM<=0
!
    do i=lbound(FILE,1),ubound(FILE,1)
      fdata = FILE(i)
      fmterr = ALL(fdata%ExtIs()/=MOLFMTS)
!
      call trj%top%ERR%check(fmterr,"Unknown format                 "//fdata%ExtIs())
      call trj%top%ERR%check(nerr,  "This atomdata is empty.        "//fdata%NameIs())
!
      if(.not.nerr.or.fmterr)call TrjExport(trj,fdata,fmode)
    enddo
!
    if(present(INFO))then ; INFO = trj%top%ERR%punch()  ; RETURN
    else                  ; call trj%top%ERR%punchabort()
    endif
  end subroutine TrjExportSeq
!
  subroutine TrjExport(this,fdata,fmode)
  class(trajectory),intent(inout)         :: this
  type(fileio),intent(inout)              :: fdata
  integer,intent(in)                      :: fmode
  logical                                 :: ioerr
    select case(fdata%ExtIs())
    case("","xyz")              ; ioerr = TrjExportXYZ(this,fdata,fmode)
    case("rst7","restrt","rst") ; ioerr = TrjExportRST(this,fdata)
    case("mdcrd","crd")         ; ioerr = TrjExportMDCRD(this,fdata,fmode)
    case("netcdf","nc")         ; ioerr = TrjExportNetCDF(this,fdata,fmode)
    case("prmtop")              ; ioerr = MolExportPRMTOP(this%top,fdata)
    case default
      call this%top%ERR%warn(.TRUE.,"This format is not support yet. "//fdata%ExtIs())
    end select
    call this%top%ERR%check(ioerr, "Could not write file "//fdata%NameIs())
  end subroutine TrjExport
!
  pure subroutine TrjReserve(this,NATM,NTRJ)
  class(Trajectory),intent(inout)  :: this
  integer,intent(in)               :: NATM,NTRJ
    call TrjDestractor(this)
    this%NATM = maxval([NATM,1],1)
    this%Stack = maxval([NTRJ,1],1)
    this%NTRJ = 0
    this%top%NATM = this%NATM
    allocate(this%xyz(3,this%NATM,this%Stack))
    allocate(this%box(3,this%Stack))
    allocate(this%top%mask(this%NATM)) ; this%top%mask = .TRUE.
    allocate(this%selection(this%NATM)) ; this%selection = .TRUE.
    RETURN
  end subroutine TrjReserve
!
  logical function TrjCheckNatm(this,NATM) result(res)
  class(trajectory),intent(inout)  :: this
  integer,intent(in)               :: NATM
    res = NATM<=0 ; if(res) RETURN
    if(this%top%NATM<=0)then
      if(allocated(this%top%mask))deallocate(this%top%mask)
      allocate(this%top%mask(NATM)) ; this%top%mask = .TRUE.
      this%top%NATM = NATM
    endif
    if(this%NATM<=0)then
      this%NATM = count(this%top%mask)
      if(allocated(this%selection))deallocate(this%selection)
      allocate(this%selection(this%NATM)) ; this%selection = .TRUE.
      RETURN
    endif
    res = NATM/=this%top%NATM ; RETURN
  end function TrjCheckNatm
