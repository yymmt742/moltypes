module moltypes_ambermdcrd
  use moltypes_errorhandler
  use spur_vector
  use spur_pathname
  use spur_stdio
  implicit none
  private
  public :: AmberMdcrd
!
  integer,parameter       :: node_def         = 0
  integer,parameter       :: frame_def        =-1
  integer,parameter       :: spatial_def      = 3
  integer,parameter       :: atom_def         =-1
!
  integer,save            :: using          = 0
!
  type AmberMdcrd
    private
    type(pathname),allocatable          :: node(:)
    integer                             :: nnode = node_def, stack  = node_def
    integer                             :: natom = atom_def, nframe = frame_def
    integer                             :: stat  = MOLTYPES_NOERR
    real,allocatable,public             :: xyz(:,:,:),box(:,:),ang(:,:)
    logical,public                      :: terminates_at_abnormal = .FALSE.
    logical,public                      :: load_xyz  = .TRUE.
    logical,public                      :: load_box  = .TRUE.
    logical,public                      :: load_ang  = .FALSE.
  contains
    procedure         :: set_natom          => AmdSetNatom
    procedure         :: fetch              => AmdFetch
    procedure         :: load               => AmdLoad
    procedure         :: nframes            => AmdNframes
    procedure         :: natoms             => AmdNatoms
    procedure         :: nnodes             => AmdNnodes
    procedure         :: isErr              => AmdIsErr
    procedure         :: export             => AmdExport
    procedure         :: clear              => AmdClear
    final             :: AmdDestractor
  end type AmberMdcrd
contains
  subroutine AmdSetNatom(this,natom)
  class(AmberMdcrd),intent(inout) :: this
  integer,intent(in)              :: natom
    call CheckAmd(this,natom<=0,IO_NATOMERR)
    this%natom = natom
  end subroutine AmdSetNatom
!
  subroutine AmdFetch(this,path)
  class(AmberMdcrd),intent(inout)  :: this
  character(*),intent(in)          :: path
  type(stdio)                      :: baff
    call CheckAmd(this,this%natom<=0,IO_NATOMERR)
    this%nnode = this%nnode + 1 ; using = this%nnode
    call stackExtention(this) ; this%node(using) = path
    call CheckAmd(this,this%node(using)%isnotExist(),IO_NOTEXIST)
    call CheckAmd(this,this%node(using)%isnotReadable(),IO_NOTREADABLE)
    call baff%fetch(path)
    call this%node(using)%put_caption(baff%gets())
    call CheckAmd(this,baff%iserr(),IO_FMTERR)
    if(this%isErr()) this%nnode = this%nnode - 1
  end subroutine AmdFetch
!
  subroutine StackExtention(this)
  class(AmberMdcrd),intent(inout)  :: this
  integer                          :: old
  type(pathname),allocatable       :: swp(:)
    if(this%Stack>=this%nnode.and.this%stack>0)RETURN
    if(this%Stack<=0)this%Stack = 1
    old = this%stack
    do while(this%stack<this%nnode)
      this%stack = this%stack * 2
    enddo
!
    allocate(swp(this%stack))
    if(allocated(this%node)) swp(1:old) = this%node(1:old)
    call move_alloc(from=swp,to=this%node)
  end subroutine StackExtention
!
  subroutine AmdLoad(this)
  class(AmberMdcrd),intent(inout) :: this
  integer                         :: is,nstack
    if(this%isErr().or.this%nnode<=0)RETURN
    nstack = 1 ; this%nframe = 0
    if(allocated(this%xyz)) deallocate(this%xyz)
    if(allocated(this%box)) deallocate(this%box)
    if(allocated(this%ang)) deallocate(this%ang)

    do using=1,this%nnode
      call getmdcrd(this,nstack)
    enddo
  contains
    subroutine getmdcrd(this,nstack)
    class(AmberMdcrd),intent(inout) :: this
    integer,intent(inout)           :: nstack
    type(stdio)                     :: baff
    real                            :: tmp(spatial_def,this%natom),boxang(spatial_def*2)
      call baff%fetch(this%node(using)%is()) ; call baff%GoForward()
      do
        read(baff%devn(),'(10F8.3)',err=101,end=100)tmp
        read(baff%devn(),'(6f8.3)',err=101,end=100)boxang
        this%nframe = this%nframe + 1 ; call TrjExpand(this,nstack)
        if(this%load_xyz) this%xyz(:,:,this%nframe) = tmp
        if(this%load_box) this%box(:,this%nframe) = boxang(:spatial_def)
        if(this%load_ang) this%ang(:,this%nframe) = boxang(spatial_def+1:)
      enddo
100   RETURN
101   call CheckAmd(this,.TRUE.,IO_FMTERR)
    end subroutine getmdcrd
!
    pure subroutine TrjExpand(this,nstack)
    class(AmberMdcrd),intent(inout) :: this
    integer,intent(inout)           :: nstack
    real,allocatable                :: tmpxyz(:,:,:),tmp(:,:)
    integer                         :: old
      if(nstack>this%nframe)return
      old = nstack
      do while(this%nframe>nstack)
        nstack = nstack * 2
      enddo
!
      if(this%load_xyz)then
        allocate(tmpxyz(spatial_def,this%natom,nstack))
        if(allocated(this%xyz)) tmpxyz(:,:,:old) = this%xyz(:,:,:old)
        call move_alloc(from=tmpxyz,to=this%xyz)
      endif
      if(this%load_box)then
        allocate(tmp(spatial_def,nstack))
        if(allocated(this%box)) tmp(:,:old) = this%box(:,:old)
        call move_alloc(from=tmp,to=this%box)
      endif
      if(this%load_ang)then
        allocate(tmp(spatial_def,nstack))
        if(allocated(this%ang)) tmp(:,:old) = this%ang(:,:old)
        call move_alloc(from=tmp,to=this%ang)
      endif
    end subroutine TrjExpand
  end subroutine AmdLoad
!
  subroutine AmdExport(this,path)
  use spur_string, only : ToStr
  class(AmberMdcrd),intent(inout)  :: this
  character(*),intent(in)           :: path
  integer ::i
  end subroutine AmdExport
!
  pure integer function AmdNatoms(this) result(res)
  class(AmberMdcrd),intent(in) :: this
    res = this%natom
  end function AmdNatoms
!
  pure integer function AmdNframes(this) result(res)
  class(AmberMdcrd),intent(in) :: this
    res = this%nframe
  end function AmdNframes
!
  pure integer function AmdNnodes(this) result(res)
  class(AmberMdcrd),intent(in) :: this
    res = this%nnode
  end function AmdNnodes
!
  subroutine CheckAmd(this,test,ierr)
  class(AmberMdcrd),intent(inout) :: this
  logical,intent(in)               :: test
  integer,intent(in)               :: ierr
    if(.not.test) RETURN
    if(this%terminates_at_abnormal)then
      if(using>0)then
        call moltypes_echo_errmsg(ierr,this%node(using)%is())
      else
        call moltypes_echo_errmsg(ierr)
      endif
      call AmdDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end subroutine CheckAmd
!
  pure logical function AmdIsERR(this) result (res)
  class(AmberMdcrd),intent(in) :: this
    res = this%stat /= MOLTYPES_NOERR
  end function AmdIsERR
!
  subroutine AmdClear(this)
  class(AmberMdcrd),intent(inout)  :: this
    this%nnode = node_def ; this%stack  = node_def
    this%natom = atom_def ; this%nframe = frame_def
    this%stat  = MOLTYPES_NOERR
    if(allocated(this%Node)) deallocate(this%Node)
    if(allocated(this%xyz))  deallocate(this%xyz)
    if(allocated(this%box))  deallocate(this%box)
    if(allocated(this%ang))  deallocate(this%ang)
  end subroutine AmdClear
!
  subroutine AmdDestractor(this)
  type(AmberMdcrd),intent(inout)  :: this
    call this%clear()
  end subroutine AmdDestractor
end module moltypes_ambermdcrd
