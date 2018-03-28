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
  integer,parameter      :: N_NULL    =  0
!
  integer,parameter      :: MODE_RST7   =  1
  integer,parameter      :: MODE_NETCDF =  2
  integer,parameter      :: MODE_XYZ    =  3
!
  integer,parameter      :: spatial_def = 3
!
  character(4),parameter :: KEY_NAME    = 'name'
  character(4),parameter :: KEY_TYPE    = 'type'
  character(5),parameter :: KEY_RESID   = 'resid'
  character(6),parameter :: KEY_CHARGE  = 'charge'
  character(7),parameter :: KEY_ELEMENT = 'element'
  character(4),parameter :: KEY_MASS    = 'mass'
  character(7),parameter :: KEY_RESNAME = 'resname'
!
  double precision,parameter :: StdChgR = 18.2223d0                ! For Amber Unit
  double precision,parameter :: StdChg  = 1.d0/StdChgR
  double precision,parameter :: StdVelR = 20.455d0                 ! For Amber Unit
  double precision,parameter :: StdVel  = 1.d0/StdVelR
!
  type moltype
    private
    real,allocatable,public       :: xyz(:,:,:),vel(:,:,:),frc(:,:,:)
    real,allocatable,public       :: box(:,:),ang(:,:),time(:)
    logical,public                :: load_xyz  = .TRUE.
    logical,public                :: load_vel  = .FALSE.
    logical,public                :: load_frc  = .FALSE.
    logical,public                :: load_box  = .TRUE.
    logical,public                :: load_ang  = .FALSE.
    logical,public                :: load_time = .FALSE.
    logical,public                :: terminates_at_abnormal = terminates_default
    integer                       :: natm   = N_NULL
    integer                       :: nframe = N_NULL
    integer,allocatable           :: mode(:)
    type(readmask),allocatable    :: mp(:)
    type(amberprmtop),allocatable :: prmtop(:)
    type(ambernetcdf),allocatable :: nc(:)
    type(amberrst7),allocatable   :: rst7(:)
    type(xyzfmt),allocatable      :: xyzfmt(:)
    logical,allocatable           :: mask(:)
    integer                       :: stat    = MOLTYPES_NOERR
  contains
    procedure,private :: MtFetch_Single
    procedure,private :: MtFetch_Array
    procedure,private :: MtFetch
    generic           :: fetch       => MtFetch_Single,MtFetch_Array,MtFetch
    procedure,private :: MtExport_Single
    procedure,private :: MtExport_Array
    procedure,private :: MtExport
    generic           :: export      => MtExport_Single,MtExport_Array,MtExport
    procedure,private :: MtInq_chr
    procedure,private :: MtInq_int
    procedure,private :: MtInq_real
    generic           :: inq         => MtInq_chr,MtInq_int,MtInq_real
    procedure,private :: MtAtomSelect
    procedure,private :: MtAtomSelect1d
    generic           :: atomselect  => MtAtomSelect,MtAtomSelect1d
    procedure,private :: MtAtomMask
    procedure,private :: MtAtomMask_x
    procedure,private :: MtAtomMask_1d
    procedure,private :: MtAtomMask_1dx
    generic           :: atommask    => MtAtomMask,MtAtomMask_x,MtAtomMask_1d,MtAtomMask_1dx
    procedure         :: load         => Moltypeload
    procedure         :: natoms       => MtNatoms
    procedure         :: nframes      => MtNframes
    procedure         :: nfetchatoms  => MtNfetchatoms
    procedure         :: nfetchframes => MtNfetchframes
    procedure         :: nresidues    => MtNresidues
    procedure         :: is_err       => MtIsErr
    procedure         :: clear        => MtClear
    procedure,private :: MtOut_of_range
    procedure,private :: MtOut_of_range_1d
    generic           :: out_of_range => MtOut_of_range,MtOut_of_range_1d
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
  pure subroutine MtAtomSelect(this,mask)
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: mask
    if(allocated(this%mp)) this%mask = this%mp(N_ALLOC)%parse(mask)
  end subroutine MtAtomSelect
!
  pure subroutine MtAtomSelect1d(this,mask)
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: mask(:)
  integer                      :: i
    this%mask = .FALSE.
    if(allocated(this%mp))then
      do i=lbound(mask,1),ubound(mask,1)
        this%mask = IOR(this%mask,this%mp(N_ALLOC)%parse(mask(i)))
      enddo
    endif
  end subroutine MtAtomSelect1d
!
  pure function MtAtomMask(this,mask) result(res)
  class(moltype),intent(in)    :: this
  character(*),intent(in)      :: mask
  logical                      :: res(maxval([0,this%natm],1))
    res = .FALSE.
    if(.not.allocated(this%mp)) RETURN
    res = pack(this%mp(N_ALLOC)%parse(mask),this%mask)
  end function MtAtomMask
!
  pure function MtAtomMask_x(this,mask,dim) result(res)
  use spur_shapeshifter
  class(moltype),intent(in)    :: this
  character(*),intent(in)      :: mask
  integer,intent(in)           :: dim
  logical                      :: res(maxval([0,dim],1),maxval([0,this%natm],1))
    res = .FALSE.
    if(.not.allocated(this%mp).or.this%natm<1) RETURN
    res = SpreadMask(pack(this%mp(N_ALLOC)%parse(mask),this%mask),this%natm,dim)
  end function MtAtomMask_x
!
  pure function MtAtomMask_1d(this,mask) result(res)
  class(moltype),intent(in)    :: this
  character(*),intent(in)      :: mask(:)
  logical                      :: res(maxval([0,this%natm],1))
  integer                      :: i
    res = .FALSE.
    if(.not.allocated(this%mp)) RETURN
    res = pack(this%mp(N_ALLOC)%parse(mask),this%mask)
  end function MtAtomMask_1d
!
  pure function MtAtomMask_1dx(this,mask,dim) result(res)
  use spur_shapeshifter
  class(moltype),intent(in)    :: this
  character(*),intent(in)      :: mask(:)
  integer,intent(in)           :: dim
  logical                      :: res(maxval([0,dim],1),maxval([0,this%natm],1))
  integer                      :: i
    res = .FALSE.
    if(.not.allocated(this%mp).or.this%natm<1) RETURN
    res = IOR(res,SpreadMask(pack(this%mp(N_ALLOC)%parse(mask),this%mask),this%natm,dim))
  end function MtAtomMask_1dx
!
  subroutine MtFetch_Single(this,path)
  use spur_pathname
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: path
  type(pathname)               :: fpath
    call fpath%fetch(path) ; call MtFetch(this,fpath)
  end subroutine MtFetch_Single
!
  subroutine MtFetch_Array(this,path)
  use spur_pathname
  class(moltype),intent(inout)       :: this
  character(*),intent(in)            :: path(:)
  type(pathname)                     :: fpath
  integer                            :: i
    do i = lbound(path,1),ubound(path,1)
      call fpath%fetch(path(i)) ; call MtFetch(this,fpath)
    enddo
  end subroutine MtFetch_Array
!
  subroutine MtFetch(this,fpath)
  use spur_pathname
  class(moltype),intent(inout) :: this
  type(pathname) ,intent(in)   :: fpath
    call RoutineNameIs('MOLTYPES_FETCH')
!
    if(CheckMt(this,fpath%isnotExist(),IO_NOTEXIST))       RETURN
    if(CheckMt(this,fpath%isnotReadable(),IO_NOTREADABLE)) RETURN
!
    select case(fpath%extension())
    case('prmtop','top')
      if(.not.allocated(this%prmtop)) allocate(this%prmtop(N_ALLOC))
      this%prmtop(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%prmtop(N_ALLOC)%load(fpath%is())
      if(CheckMt(this,this%prmtop(N_ALLOC)%iserr(),IO_FMTERR))       RETURN
      if(CheckMt(this,this%prmtop(N_ALLOC)%Natoms()<1,IO_NATOMERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%prmtop(N_ALLOC)%natoms())
      if(CheckMt(this,size(this%mask)/=this%prmtop(N_ALLOC)%natoms(),IO_NATOMERR)) RETURN
!
      if(allocated(this%prmtop(N_ALLOC)%IGRAPH)) call this%mp(N_ALLOC)%def_keyword(KEY_NAME,   this%prmtop(N_ALLOC)%IGRAPH)
      if(allocated(this%prmtop(N_ALLOC)%ISYMBL)) call this%mp(N_ALLOC)%def_keyword(KEY_TYPE,   this%prmtop(N_ALLOC)%ISYMBL)
      if(allocated(this%prmtop(N_ALLOC)%ATNUM))  call this%mp(N_ALLOC)%def_keyword(KEY_ELEMENT,this%prmtop(N_ALLOC)%ATNUM)
      if(allocated(this%prmtop(N_ALLOC)%CHARGE)) call this%mp(N_ALLOC)%def_keyword(KEY_CHARGE, real(this%prmtop(N_ALLOC)%CHARGE*StdChg))
      if(allocated(this%prmtop(N_ALLOC)%AMASS))  call this%mp(N_ALLOC)%def_keyword(KEY_MASS,   real(this%prmtop(N_ALLOC)%AMASS))
      call this%mp(N_ALLOC)%def_keyword(KEY_RESNAME,this%prmtop(N_ALLOC)%resname())
      call this%mp(N_ALLOC)%def_keyword(KEY_RESID,  this%prmtop(N_ALLOC)%resid())
    case('netcdf','nc')
      if(.not.allocated(this%nc)) allocate(this%nc(N_ALLOC))
      this%nc(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%nc(N_ALLOC)%fetch(fpath%is())
      if(CheckMt(this,this%nc(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%nc(N_ALLOC)%natoms)
      if(CheckMt(this,size(this%mask)/=this%nc(N_ALLOC)%natoms,IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_NETCDF)
    case('rst7','restrt')
      if(.not.allocated(this%rst7)) allocate(this%rst7(N_ALLOC))
      this%rst7(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%rst7(N_ALLOC)%fetch(fpath%is())
      if(CheckMt(this,this%rst7(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%rst7(N_ALLOC)%natoms)
      if(CheckMt(this,size(this%mask)/=this%rst7(N_ALLOC)%natoms,IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_RST7)
    case('xyz')
      if(.not.allocated(this%xyzfmt)) allocate(this%xyzfmt(N_ALLOC))
      this%xyzfmt(N_ALLOC)%terminates_at_abnormal = this%terminates_at_abnormal
!
      call this%xyzfmt(N_ALLOC)%fetch(fpath%is())
      if(CheckMt(this,this%xyzfmt(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
!
      if(.not.allocated(this%mp)) call MoltypeMPinit(this,this%xyzfmt(N_ALLOC)%natoms)
!
      if(CheckMt(this,size(this%mask)/=this%xyzfmt(N_ALLOC)%natoms,IO_NATOMERR)) RETURN
      call MoltypeSetMode(this,MODE_XYZ)
!
      call this%mp(N_ALLOC)%def_keyword(KEY_NAME,this%xyzfmt(N_ALLOC)%atoms())
    end select
  end subroutine MtFetch
!
  subroutine MtExport_Single(this,path,overwrite)
  use spur_pathname
  class(moltype),intent(inout)            :: this
  character(*),intent(in)                 :: path
  logical,intent(in),optional             :: overwrite
  type(pathname)                          :: fpath
    call fpath%fetch(path)
    if(CheckAbort(fpath%isnotWritable(),IO_NOTWRITABLE,path)) RETURN
    if(present(overwrite))then
      call MtExport(this,fpath,overwrite)
    else
      call MtExport(this,fpath)
    endif
  end subroutine MtExport_Single
!
  subroutine MtExport_Array(this,path,overwrite)
  use spur_pathname
  class(moltype),intent(inout) :: this
  character(*),intent(in)      :: path(:)
  logical,intent(in),optional  :: overwrite
  type(pathname)               :: fpath
  integer                      :: i
    do i = lbound(path,1),ubound(path,1)
      call fpath%fetch(path(i))
      if(CheckAbort(fpath%isnotWritable(),IO_NOTWRITABLE,path(i))) CYCLE
      if(present(overwrite))then
        call MtExport(this,fpath,overwrite)
      else
        call MtExport(this,fpath)
      endif
    enddo
  end subroutine MtExport_Array
!
  subroutine MtExport(this,fpath,overwrite)
  use spur_pathname
  use moltypes_export
  class(moltype),intent(inout) :: this
  type(pathname) ,intent(in)   :: fpath
  logical,intent(in),optional  :: overwrite
  logical                      :: lo
    if(this%natm<1) RETURN
    call RoutineNameIs('MOLTYPES_EXPORT')
    lo = .TRUE. ; if(present(overwrite)) lo = overwrite
    select case(fpath%extension())
    case('netcdf','nc')
      if(fpath%isnotExist().or.lo)then
        call GenerateAmberNetcdf(fpath%is(),this%natm, &
           &                     allocated(this%xyz),allocated(this%vel), &
           &                     allocated(this%frc),allocated(this%box), &
           &                     allocated(this%ang),allocated(this%time))
      endif
      call ExportAmberNetcdf(fpath%is(),this%natm,this%nframe,    &
         &                   this%xyz,this%vel,this%frc,             &
         &                   dble(this%box),dble(this%ang),this%time)
!   case('mdcrd','crd')
!     call ExportMdcrd(fpath%is(),this%natm(),this%nframe(),          &
!    &                 this%xyz(),this%box(),this%boxang(),overwrite=lo)
!   case('rst7','restrt')
!     call ExportRST7(fpath%is(),this%natm(),                          &
!    &                this%xyz(this%nframe()),this%box(this%nframe()), &
!    &                this%boxang(this%nframe()))
    case default
      if(allocated(this%xyz)) call ExportXYZ(fpath%is(),this%natm,this%nframe,this%xyz,this%inq('name','XX  '),overwrite=lo)
    end select
  end subroutine MtExport
!
  subroutine MoltypeLoad(this,lb,ub,stride,mask)
  class(moltype),intent(inout)     :: this
  integer,intent(in),optional      :: lb,ub,stride
  character(*),intent(in),optional :: mask
  integer                          :: llb,lub,linc
  logical                          :: umask
    call RoutineNameIs('MOLTYPES_LOAD')
    if(.not.allocated(this%mp)) RETURN
    llb  =  1 ; if(present(lb)) llb = lb
    lub  = -1 ; if(present(ub)) lub = ub
    linc =  1 ; if(present(stride)) linc = stride
    if(present(mask)) this%mask = this%mp(N_ALLOC)%parse(mask)
!
    umask = count(this%mask)/=size(this%mask)
    this%natm  = N_NULL ; this%nframe = N_NULL
!
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      if(this%load_xyz)then
        if(umask)then
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,xyz=this%xyz,lb=llb,ub=lub,inc=linc,mask=this%mask)
        else
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,xyz=this%xyz,lb=llb,ub=lub,inc=linc)
        endif
      endif
      if(this%load_vel)then
        if(umask)then
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,vel=this%vel,lb=llb,ub=lub,inc=linc,mask=this%mask)
        else
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,vel=this%vel,lb=llb,ub=lub,inc=linc)
        endif
      endif
      if(this%load_frc)then
        if(umask)then
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,frc=this%frc,lb=llb,ub=lub,inc=linc,mask=this%mask)
        else
          call this%nc(N_ALLOC)%load(atm=this%natm,frame=this%nframe,frc=this%frc,lb=llb,ub=lub,inc=linc)
        endif
      endif
      if(this%load_box)  call this%nc(N_ALLOC)%load(box=this%box,lb=llb,ub=lub,inc=linc)
      if(this%load_ang)  call this%nc(N_ALLOC)%load(ang=this%ang,lb=llb,ub=lub,inc=linc)
      if(this%load_time) call this%nc(N_ALLOC)%load(time=this%time,lb=llb,ub=lub,inc=linc)
      if(CheckMt(this,this%nc(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
    case(MODE_RST7)
      call this%rst7(N_ALLOC)%load(xyz=this%xyz,vel=this%vel,box=this%box, &
         &                        ang=this%ang,time=this%time,             &
         &                        lb=llb,ub=lub,inc=linc,mask=this%mask)
      if(CheckMt(this,this%rst7(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
      if(allocated(this%xyz))then
        this%natm  = size(this%xyz,2) ; this%nframe = size(this%xyz,3)
      endif
    case(MODE_XYZ)
      call this%xyzfmt(N_ALLOC)%load(xyz=this%xyz,lb=llb,ub=lub,inc=linc)
      if(CheckMt(this,this%xyzfmt(N_ALLOC)%iserr(),IO_FMTERR)) RETURN
      if(allocated(this%xyz))then
        this%natm  = size(this%xyz,2) ; this%nframe = size(this%xyz,3)
      endif
    end select
  end subroutine MoltypeLoad
!
  pure subroutine MoltypeSetMode(this,MODE)
  class(moltype),intent(inout) :: this
  integer,intent(in)           :: MODE
    if(.not.allocated(this%mode)) allocate(this%mode(N_ALLOC))
    this%mode(N_ALLOC) = MODE
  end subroutine MoltypeSetMode
!
  pure integer function MoltypeMode(this) result(res)
  class(moltype),intent(in)   :: this
    if(.not.allocated(this%mode))then
      res = MODE_NULL
    else
      res = this%mode(N_ALLOC)
    endif
  end function MoltypeMode
!
  pure integer function MtNresidues(this) result(res)
  use spur_vector_int4
  class(moltype),intent(in) :: this
  type(vector_int4)         :: resid
    res = 0
    if(.not.allocated(this%mp)) RETURN
    resid = pack(this%mp(N_ALLOC)%showint(KEY_RESID),this%mask)
    call resid%uniq() ; res = resid%size()
  end function MtNresidues
!
  pure elemental integer function MtNatoms(this) result(res)
  class(moltype),intent(in) :: this
    res = maxval([this%natm,0],1)
  end function MtNatoms
!
  pure elemental integer function MtNfetchatoms(this) result(res)
  class(moltype),intent(in) :: this
    if(allocated(this%mask))then
      res = count(this%mask)
    else
      res = 0
    endif
  end function MtNfetchatoms
!
  pure elemental integer function MtNframes(this) result(res)
  class(moltype),intent(in) :: this
    res = maxval([this%nframe,0],1)
  end function MtNframes
!
  pure elemental integer function MtNfetchframes(this) result(res)
  class(moltype),intent(in) :: this
    select case(MoltypeMode(this))
    case(MODE_NETCDF)
      res = this%nc(N_ALLOC)%nframes
    case(MODE_RST7)
      res = this%rst7(N_ALLOC)%nframes
    case(MODE_XYZ)
      res = this%xyzfmt(N_ALLOC)%nframes
    case default
      res = 0
    end select
    if(res<0) res = 0
  end function MtNfetchframes
!
  pure function MtInq_chr(this,key,dumm) result(res)
  class(moltype),intent(in)        :: this
  character(*),intent(in)          :: key,dumm
  character(len(dumm)),allocatable :: res(:)
    allocate(res(this%nfetchatoms())) ; res = dumm
    if(.not.allocated(this%mp).or.this%nfetchatoms()<1) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'c'))then
      res = pack(this%mp(N_ALLOC)%showchr(key),this%mask)
    endif
  end function MtInq_chr
!
  pure function MtInq_int(this,key,dumm) result(res)
  class(moltype),intent(in) :: this
  character(*),intent(in)   :: key
  integer,intent(in)        :: dumm
  integer,allocatable       :: res(:)
    allocate(res(this%nfetchatoms()))
    if(.not.allocated(this%mp).or.this%nfetchatoms()<1) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'i'))then
      res = pack(this%mp(N_ALLOC)%showint(key),this%mask)
    else
      res = dumm
    endif
  end function MtInq_int
!
  pure function MtInq_real(this,key,dumm) result(res)
  class(moltype),intent(in) :: this
  character(*),intent(in)   :: key
  real,intent(in)           :: dumm
  real,allocatable          :: res(:)
    allocate(res(this%nfetchatoms()))
    if(.not.allocated(this%mp).or.this%nfetchatoms()<1) RETURN
    if(this%mp(N_ALLOC)%inq_key(key,'r'))then
      res = pack(this%mp(N_ALLOC)%showreal(key),this%mask)
    else
      res = dumm
    endif
  end function MtInq_real
!
  pure logical function MtOut_of_range(this,idx) result(res)
  class(moltype),intent(in)   :: this
  integer,intent(in),optional :: idx
    res = this%natm<1.or.this%nframe<1
    if(present(idx)) res = res.or.idx<1.or.this%nframe<idx
  end function MtOut_of_range
!
  pure logical function MtOut_of_range_1d(this,idx) result(res)
  class(moltype),intent(in) :: this
  integer,intent(in)            :: idx(:)
    res = this%natm<1.or.this%nframe<1
    res = res.or.any(idx<1).or.any(this%nframe<idx)
  end function MtOut_of_range_1d
!
  pure logical function MtIsErr(this) result(res)
  class(moltype),intent(in) :: this
    res = this%stat /= MOLTYPES_NOERR
  end function MtIsErr
!
  logical function CheckMt(this,test,ierr) result(res)
  class(moltype),intent(inout) :: this
  logical,intent(in)           :: test
  integer,intent(in)           :: ierr
    res = test ; if(.not.res) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call MoltypeDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end function CheckMt
!
  subroutine MtClear(this)
  class(moltype),intent(inout)  :: this
    this%stat    = MOLTYPES_NOERR
    this%natm  = N_NULL ; this%nframe = N_NULL
    if(allocated(this%mode))  deallocate(this%mode)
    if(allocated(this%mp))    deallocate(this%mp)
    if(allocated(this%prmtop))deallocate(this%prmtop)
    if(allocated(this%nc))    deallocate(this%nc)
    if(allocated(this%rst7))  deallocate(this%rst7)
    if(allocated(this%xyzfmt))deallocate(this%xyzfmt)
    if(allocated(this%mask))  deallocate(this%mask)
    this%terminates_at_abnormal = terminates_default
  end subroutine MtClear
!
  subroutine MoltypeDestractor(this)
  type(moltype),intent(inout) :: this
    call this%clear()
  end subroutine MoltypeDestractor
end module moltypes
