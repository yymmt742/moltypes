module moltypes
  use moltypes_errorhandler
  use moltypes_amberprmtop
  use moltypes_ambernetcdf
  use moltypes_amberrst7
  use moltypes_xyz
  use moltypes_readmask
  implicit none
  private
  public :: moltype
!
  integer,parameter      :: N_ALLOC   =  1
  integer,parameter      :: MODE_NULL = -1
  integer,parameter      :: N_NULL    = -1
!
  integer,parameter      :: MODE_RST7   =  1
  integer,parameter      :: MODE_NETCDF =  2
  integer,parameter      :: MODE_XYZ    =  3
!
  character(4),parameter :: KEY_NAME    = 'name'
  character(4),parameter :: KEY_TYPE    = 'type'
  character(5),parameter :: KEY_RESID   = 'resid'
  character(6),parameter :: KEY_CHARGE  = 'charge'
  character(7),parameter :: KEY_ELEMENT = 'element'
  character(4),parameter :: KEY_MASS    = 'mass'
  character(7),parameter :: KEY_RESNAME = 'resname'
!
  double precision,parameter :: StdChgR = 18.2223                  ! For Amber Unit
  double precision,parameter :: StdChg  = 1.0/StdChgR              !    1.0/18.2223
  double precision,parameter :: StdVelR = 20.4550                  ! For Amber Unit
  double precision,parameter :: StdVel  = 1.0/StdVelR              !   1.d0/20.455d0
!
  type moltype
  private
    integer                       :: stat = MOLTYPES_NOERR
    integer,allocatable           :: mode(:)
    type(readmask),allocatable    :: mp(:)
    type(amberprmtop),allocatable :: prmtop(:)
    type(ambernetcdf),allocatable :: nc(:)
    type(amberrst7),allocatable   :: rst7(:)
    type(xyzfmt),allocatable      :: xyzfmt(:)
    logical,allocatable           :: mask(:)
    logical,public                :: terminates_at_abnormal = terminates_default
  contains
    procedure,private :: MoltypeFetch_Single
    procedure,private :: MoltypeFetch_Array
    procedure,private :: MoltypeFetch
    generic           :: fetch   => MoltypeFetch_Single,MoltypeFetch_Array,MoltypeFetch
    procedure         :: load          => Moltypeload
    procedure         :: natoms        => MoltypeNatoms
    procedure         :: nresidues     => MoltypeNresidues
    procedure         :: nframes       => MoltypeNframes
    procedure,private :: MoltypeInq_chr
    procedure,private :: MoltypeInq_int
    procedure,private :: MoltypeInq_real
    generic           :: inq     => MoltypeInq_chr,MoltypeInq_int,MoltypeInq_real
    procedure,private :: MoltypeXYZ_all
    procedure,private :: MoltypeXYZ_idx
    generic           :: xyz     => MoltypeXYZ_all, MoltypeXYZ_idx
    procedure,private :: MoltypeBox_all
    procedure,private :: MoltypeBox_idx
    generic           :: box     => MoltypeBox_all, MoltypeBox_idx
    procedure,private :: MoltypeBoxAng_all
    procedure,private :: MoltypeBoxAng_idx
    generic           :: boxang  => MoltypeBoxAng_all, MoltypeBoxAng_idx
    procedure,private :: MoltypeAtomSelect
    procedure,private :: MoltypeAtomSelectArray
    generic           :: atomselect    => MoltypeAtomSelect,MoltypeAtomSelectArray
    procedure,private :: MoltypeGetMask
    procedure,private :: MoltypeGetMaskND
    generic           :: getmask => MoltypeGetMask, MoltypeGetMaskND
    procedure         :: putxyz  => MoltypePutXYZ
    procedure         :: clear         => MoltypeClear
    procedure         :: iserr         => MoltypeisErr
    final     :: moltypeDestractor
   end type moltype
contains
  subroutine MoltypeMPinit(this,natom)
  class(moltype),intent(inout) :: this
  integer,intent(in)           :: natom
    if(allocated(this%mp)) deallocate(this%mp) ; allocate(this%mp(N_ALLOC))
    this%mp(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
    call this%mp(N_ALLOC)%init(natom)
    if(allocated(this%mask)) deallocate(this%mask)
    allocate(this%mask(natom)) ; this%mask = .TRUE.
  end subroutine MoltypeMPinit
!
  subroutine MoltypeAtomSelect(this,mask)
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: mask
    if(allocated(this%mp)) this%mask = this%mp(N_ALLOC)%parse(mask)
  end subroutine MoltypeAtomSelect
!
  subroutine MoltypeAtomSelectArray(this,mask)
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: mask(:)
  integer                      :: i
    this%mask = .FALSE.
    if(allocated(this%mp))then
      do i=lbound(mask,1),ubound(mask,1)
        this%mask = IOR(this%mask,this%mp(N_ALLOC)%parse(mask(i)))
      enddo
    endif
  end subroutine MoltypeAtomSelectArray
!
  subroutine MoltypeFetch_Single(this,path)
  use spur_pathname
  class(moltype),intent(inout)            :: this
  character(*),intent(in)                 :: path
  type(pathname)                          :: fpath
    call fpath%fetch(path) ; call MoltypeFetch(this,fpath)
  end subroutine MoltypeFetch_Single
!
  subroutine MoltypeFetch_Array(this,path)
  use spur_pathname
  class(moltype),intent(inout)            :: this
  character(*),intent(in)                 :: path(:)
  type(pathname)                          :: fpath
  integer                                 :: i
    do i = lbound(path,1),ubound(path,1)
      call fpath%fetch(path(i)) ; call MoltypeFetch(this,fpath)
    enddo
  end subroutine MoltypeFetch_Array
!
  subroutine MoltypeFetch(this,fpath)
  use spur_pathname
  class(moltype),intent(inout) :: this
  type(pathname) ,intent(in)   :: fpath
    call RoutineNameIs('MOLTYPES_FETCH')
!
    if(CheckMoltype(this,fpath%isnotExist(),IO_NOTEXIST))       RETURN
    if(CheckMoltype(this,fpath%isnotReadable(),IO_NOTREADABLE)) RETURN
!
    select case(fpath%extension())
    case('prmtop','top')
      if(.not.allocated(this%prmtop)) allocate(this%prmtop(N_ALLOC))
      this%prmtop(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%prmtop(N_ALLOC)%load(fpath%is())
      if(CheckMoltype(this,this%prmtop(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%prmtop(N_ALLOC)%Natoms())
      if(CheckMoltype(this,size(this%mask)/=this%prmtop(N_ALLOC)%Natoms(),IO_NATOMERR)) RETURN
!
      call this%mp(N_ALLOC)%def_keyword(KEY_NAME,   this%prmtop(N_ALLOC)%IGRAPH)
      call this%mp(N_ALLOC)%def_keyword(KEY_TYPE,   this%prmtop(N_ALLOC)%ISYMBL)
      call this%mp(N_ALLOC)%def_keyword(KEY_RESNAME,this%prmtop(N_ALLOC)%resname())
      call this%mp(N_ALLOC)%def_keyword(KEY_RESID,  this%prmtop(N_ALLOC)%resid())
      call this%mp(N_ALLOC)%def_keyword(KEY_ELEMENT,this%prmtop(N_ALLOC)%ATNUM)
      call this%mp(N_ALLOC)%def_keyword(KEY_CHARGE, real(this%prmtop(N_ALLOC)%CHARGE*StdChg))
      call this%mp(N_ALLOC)%def_keyword(KEY_MASS,   real(this%prmtop(N_ALLOC)%AMASS))
    case('netcdf','nc')
      if(.not.allocated(this%nc)) allocate(this%nc(N_ALLOC))
      this%nc(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%nc(N_ALLOC)%fetch(fpath%is())
      if(CheckMoltype(this,this%nc(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%nc(N_ALLOC)%natoms())
      if(CheckMoltype(this,size(this%mask)/=this%nc(N_ALLOC)%natoms(),IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_NETCDF)
    case('rst7','restrt')
      if(.not.allocated(this%rst7)) allocate(this%rst7(N_ALLOC))
      this%rst7(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%rst7(N_ALLOC)%fetch(fpath%is())
      if(CheckMoltype(this,this%rst7(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%rst7(N_ALLOC)%natoms())
!
      if(CheckMoltype(this,size(this%mask)/=this%rst7(N_ALLOC)%natoms(),IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_RST7)
    case('xyz')
      if(.not.allocated(this%xyzfmt)) allocate(this%xyzfmt(N_ALLOC))
      this%xyzfmt(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%xyzfmt(N_ALLOC)%fetch(fpath%is())
      if(CheckMoltype(this,this%xyzfmt(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%xyzfmt(N_ALLOC)%natoms())
!
      if(CheckMoltype(this,size(this%mask)/=this%xyzfmt(N_ALLOC)%natoms(),IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_XYZ)
!
      call this%mp(N_ALLOC)%def_keyword(KEY_NAME,this%xyzfmt(N_ALLOC)%atoms())
    end select
  end subroutine MoltypeFetch
!
  subroutine MoltypeLoad(this,lb,ub,stride,mask)
  class(moltype),intent(inout)     :: this
  integer,intent(in),optional      :: lb,ub,stride
  character(*),intent(in),optional :: mask
  integer                          :: llb,lub,linc
    call RoutineNameIs('MOLTYPES_LOAD')
    if(.not.allocated(this%mp)) RETURN
    llb  =  1 ; if(present(lb)) llb = lb
    lub  = -1 ; if(present(ub)) lub = ub
    linc =  1 ; if(present(stride)) linc = stride
    if(present(mask)) this%mask = this%mp(N_ALLOC)%parse(mask)
!
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      call this%nc(N_ALLOC)%load(lb=llb,ub=lub,inc=linc,mask=this%mask)
      if(CheckMoltype(this,this%nc(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
    case(MODE_RST7)
      call this%rst7(N_ALLOC)%load(lb=llb,ub=lub,inc=linc,mask=this%mask)
      if(CheckMoltype(this,this%rst7(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
    case(MODE_XYZ)
      call this%xyzfmt(N_ALLOC)%load()
      if(CheckMoltype(this,this%xyzfmt(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
    end select
  end subroutine MoltypeLoad
!
  pure subroutine MoltypeSetMode(this,MODE)
  class(moltype),intent(inout)     :: this
  integer,intent(in)               :: MODE
    if(.not.allocated(this%mode)) allocate(this%mode(N_ALLOC))
    this%mode(N_ALLOC) = MODE
  end subroutine MoltypeSetMode
!
  pure integer function MoltypeMode(this) result(res)
  class(moltype),intent(in)        :: this
    if(.not.allocated(this%mode))then
      res = MODE_NULL
    else
      res = this%mode(N_ALLOC)
    endif
  end function MoltypeMode
!
  pure integer function MoltypeNatoms(this) result(res)
  class(moltype),intent(in)        :: this
    if(allocated(this%mask))then
      res = count(this%mask)
    else
      res = 0
    endif
  end function MoltypeNatoms
!
  pure integer function MoltypeNresidues(this) result(res)
  use spur_vector
  class(moltype),intent(in) :: this
  type(vector_integer)      :: resid,iuniq
    res = 0
    if(.not.allocated(this%mp)) RETURN
    resid = pack(this%mp(N_ALLOC)%showint(KEY_RESID),this%mask)
    iuniq = resid%uniq()
    res = iuniq%size()
  end function MoltypeNresidues
!
  pure integer function MoltypeNframes(this) result(res)
  class(moltype),intent(in)     :: this
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      res = this%nc(N_ALLOC)%nframes()
    case(MODE_RST7)
      res = this%rst7(N_ALLOC)%nframes()
    case(MODE_XYZ)
      res = this%xyzfmt(N_ALLOC)%nframes()
    case default
      res = 0
    end select
    if(res<0) res = 0
  end function MoltypeNframes
!
  pure function MoltypeInq_chr(this,key,dumm) result(res)
  class(moltype),intent(in)        :: this
  character(*),intent(in)          :: key,dumm
  character(len(dumm)),allocatable :: res(:)
    allocate(res(this%natoms()))
    if(.not.allocated(this%mp)) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'c'))then
      res = pack(this%mp(N_ALLOC)%showchr(key),this%mask)
    else
      res = dumm
    endif
  end function MoltypeInq_chr
!
  pure function MoltypeInq_int(this,key,dumm) result(res)
  class(moltype),intent(in)        :: this
  character(*),intent(in)          :: key
  integer,intent(in)               :: dumm
  integer,allocatable              :: res(:)
    allocate(res(this%natoms()))
    if(.not.allocated(this%mp)) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'i'))then
      res = pack(this%mp(N_ALLOC)%showint(key),this%mask)
    else
      res = dumm
    endif
  end function MoltypeInq_int
!
  pure function MoltypeInq_real(this,key,dumm) result(res)
  class(moltype),intent(in)        :: this
  character(*),intent(in)          :: key
  real,intent(in)                  :: dumm
  real,allocatable                 :: res(:)
    allocate(res(this%natoms()))
    if(.not.allocated(this%mp)) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'r'))then
      res = pack(this%mp(N_ALLOC)%showreal(key),this%mask)
    else
      res = dumm
    endif
  end function MoltypeInq_real
!
  pure function MoltypeXYZ_all(this) result(res)
  class(moltype),intent(in)        :: this
  real,allocatable                 :: res(:,:,:)
    allocate(res(3,this%natoms(),this%nframes())) ; res = 0.0
    if(.not.allocated(this%mp)) RETURN
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(allocated(this%nc(N_ALLOC)%xyz))    res = this%nc(N_ALLOC)%xyz(1:3,1:this%natoms(),1:this%nframes())
    case(MODE_RST7)
      if(allocated(this%rst7(N_ALLOC)%xyz))  res = this%rst7(N_ALLOC)%xyz(1:3,1:this%natoms(),1:this%nframes())
    case(MODE_XYZ)
      if(allocated(this%xyzfmt(N_ALLOC)%xyz))res = this%xyzfmt(N_ALLOC)%xyz(1:3,1:this%natoms(),1:this%nframes())
    case default
      res = 0.0
    end select
  end function MoltypeXYZ_all
!
  pure function MoltypeXYZ_idx(this,idx) result(res)
  class(moltype),intent(in)        :: this
  integer,intent(in)               :: idx
  real,allocatable                 :: res(:,:)
  integer                          :: at
    allocate(res(3,this%natoms())) ; res = 0.0
    if(.not.allocated(this%mp)) RETURN
    at = PeriodicBound(this%nframes(),idx)
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(allocated(this%nc(N_ALLOC)%xyz))    res = this%nc(N_ALLOC)%xyz(1:3,1:this%natoms(),at)
    case(MODE_RST7)
      if(allocated(this%rst7(N_ALLOC)%xyz))  res = this%rst7(N_ALLOC)%xyz(1:3,1:this%natoms(),at)
    case(MODE_XYZ)
      if(allocated(this%xyzfmt(N_ALLOC)%xyz))res = this%xyzfmt(N_ALLOC)%xyz(1:3,1:this%natoms(),at)
    end select
  end function MoltypeXYZ_idx
!
  pure function MoltypeBox_all(this) result(res)
  class(moltype),intent(in)        :: this
  real,allocatable                 :: res(:,:)
    allocate(res(3,this%nframes())) ; res = 0.0
    if(.not.allocated(this%mp)) RETURN
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(allocated(this%nc(N_ALLOC)%box))    res = this%nc(N_ALLOC)%box(1:3,1:this%nframes())
    case(MODE_RST7)
      if(allocated(this%rst7(N_ALLOC)%box))  res = this%rst7(N_ALLOC)%box(1:3,1:this%nframes())
    end select
  end function MoltypeBox_all
!
  pure function MoltypeBox_idx(this,idx) result(res)
  class(moltype),intent(in)        :: this
  integer,intent(in)               :: idx
  real                             :: res(3)
  integer                          :: at
    at = PeriodicBound(this%nframes(),idx)
    res = 0.0
    if(.not.allocated(this%mp)) RETURN
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(allocated(this%nc(N_ALLOC)%box))    res = this%nc(N_ALLOC)%box(1:3,at)
    case(MODE_RST7)
      if(allocated(this%rst7(N_ALLOC)%box))  res = this%rst7(N_ALLOC)%box(1:3,at)
    end select
  end function MoltypeBox_idx
!
  pure function MoltypeBoxAng_all(this) result(res)
  class(moltype),intent(in)        :: this
  real,allocatable                 :: res(:,:)
    allocate(res(3,this%nframes())) ; res = 90.0
    if(.not.allocated(this%mp)) RETURN
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(allocated(this%nc(N_ALLOC)%ang))    res = this%nc(N_ALLOC)%ang(1:3,1:this%nframes())
    case(MODE_RST7)
      if(allocated(this%rst7(N_ALLOC)%ang))  res = this%rst7(N_ALLOC)%ang(1:3,1:this%nframes())
    end select
  end function MoltypeBoxAng_all
!
  pure function MoltypeBoxAng_idx(this,idx) result(res)
  class(moltype),intent(in)        :: this
  integer,intent(in)               :: idx
  real                             :: res(3)
  integer                          :: at
    at = PeriodicBound(this%nframes(),idx)
    res = 90.0
    if(.not.allocated(this%mp)) RETURN
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
     if(allocated(this%nc(N_ALLOC)%ang))     res = this%nc(N_ALLOC)%box(1:3,at)
    case(MODE_RST7)
     if(allocated(this%rst7(N_ALLOC)%ang))   res = this%rst7(N_ALLOC)%box(1:3,at)
    end select
  end function MoltypeBoxAng_idx
!
  function MoltypeGetMask(this,mask) result(res)
  class(moltype),intent(inout)     :: this
  character(*),intent(in)          :: mask
  logical,allocatable              :: res(:)
  integer                          :: at
    allocate(res(this%natoms())) ; if(.not.allocated(this%mp)) RETURN
    res = pack(this%mp(N_ALLOC)%parse(mask),this%mask)
  end function MoltypeGetMask
!
  function MoltypeGetMaskND(this,mask,dms) result(res)
  class(moltype),intent(inout)     :: this
  character(*),intent(in)          :: mask
  integer,intent(in)               :: dms
  logical,allocatable              :: res(:,:)
  integer                          :: at
    allocate(res(dms,this%natoms())) ; if(.not.allocated(this%mp)) RETURN
    res = maskexpand(pack(this%mp(N_ALLOC)%parse(mask),this%mask),dms,this%natoms())
  contains
    pure function maskexpand(mask,dms,natm) result(res)
    logical,intent(in) :: mask(natm)
    integer,intent(in) :: dms,natm
    logical            :: res(dms,natm)
    integer            :: i
      do concurrent (i=1:natm) ; block
        res(:,i) = mask(i)
      end block ; enddo
    end function maskexpand
  end function MoltypeGetMaskND
!
  subroutine MoltypePutXYZ(this,xyz)
  class(moltype),intent(inout) :: this
  real,intent(in)              :: xyz(:,:,:)
  integer                      :: natom,nframe
    if(.not.allocated(this%mp)) RETURN
    natom = this%natoms() ; nframe = this%nframes()
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      this%nc(N_ALLOC)%xyz(1:3,1:natom,1:nframe)     = xyz(1:3,1:natom,1:nframe)
    case(MODE_RST7)
      this%rst7(N_ALLOC)%xyz(1:3,1:natom,1:nframe)   = xyz(1:3,1:natom,1:nframe)
    case(MODE_XYZ)
      this%xyzfmt(N_ALLOC)%xyz(1:3,1:natom,1:nframe) = xyz(1:3,1:natom,1:nframe)
    end select
  end subroutine MoltypePutXYZ
!
  pure integer function PeriodicBound(length,idx) result(res)
  integer,intent(in)                  :: length,idx
    if(idx>length)then
      res = modulo(idx,length) ; if(res==0) res = length
    elseif(idx<=0)then
      res = modulo(idx,length) + 1
    else
      res = idx
    endif
  end function PeriodicBound
!
  pure logical function MoltypeisErr(this) result(res)
  class(moltype),intent(in)     :: this
    res = this%stat /= MOLTYPES_NOERR
  end function MoltypeisErr
!
  logical function CheckMoltype(this,test,ierr) result(res)
  class(moltype),intent(inout)     :: this
  logical,intent(in)               :: test
  integer,intent(in)               :: ierr
    res = test ; if(.not.res) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call MoltypeDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end function CheckMoltype
!
  subroutine MoltypeClear(this)
  class(moltype),intent(inout)  :: this
    this%stat   = MOLTYPES_NOERR
    if(allocated(this%mode))  deallocate(this%mode)
    if(allocated(this%mp))    deallocate(this%mp)
    if(allocated(this%prmtop))deallocate(this%prmtop)
    if(allocated(this%nc))    deallocate(this%nc)
    if(allocated(this%rst7))  deallocate(this%rst7)
    if(allocated(this%mask))  deallocate(this%mask)
    this%terminates_at_abnormal = terminates_default
  end subroutine MoltypeClear
!
  subroutine MoltypeDestractor(this)
  type(moltype),intent(inout)  :: this
    call this%clear()
  end subroutine MoltypeDestractor
end module moltypes
