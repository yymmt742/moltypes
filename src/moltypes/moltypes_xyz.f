module moltypes_xyz
  use moltypes_errorhandler
  use spur_vector
  use spur_pathname
  use spur_stdio
  implicit none
  private
  public :: XYZFMT
!
  integer,parameter       :: atom_def         = -1
  integer,parameter       :: frame_def        =  0
  integer,parameter       :: spatial_def      =  3
  integer,parameter       :: node_def         =  0
!
  type XYZFMT
    private
    integer                             :: natom = atom_def, nnode  = node_def
    integer                             :: nmask = atom_def, nframe = frame_def
    integer                             :: stat  = MOLTYPES_NOERR, stack  = node_def
    type(pathname),allocatable          :: node(:)
    type(vector_character)              :: atm
    real,allocatable,public             :: xyz(:,:,:)
    logical,public                      :: terminates_at_abnormal = terminates_default
  contains
    procedure         :: fetch        => XyzFetch
    procedure         :: load         => XyzLoad
    procedure         :: atoms        => XyzAtoms
    procedure         :: nframes      => XyzNframes
    procedure         :: natoms       => XyzNatoms
    procedure         :: nmasks       => XyzNmasks
    procedure         :: nnodes       => XyzNnodes
    procedure         :: isErr        => XyzIsErr
    procedure         :: clear        => XyzClear
    final             :: XyzDestractor
  end type XYZFMT
!
contains
  subroutine XyzFetch(this,path)
  class(XYZFMT),intent(inout)  :: this
  character(*),intent(in)      :: path
  type(stdio)                  :: baff
  type(vector_character)       :: words
  character(256)               :: dumatm
  real                         :: dum(spatial_def)
  integer                      :: i,atom,using
    call RoutineNameIs('XYZFMT_FETCH')
    baff%terminates_at_abnormal = this%terminates_at_abnormal
    this%nnode = this%nnode + 1 ; using = this%nnode
    call stackExtention(this) ; this%node(using) = path
    call CheckXyz(this,this%node(using)%isnotExist(),IO_NOTEXIST)
    call CheckXyz(this,this%node(using)%isnotReadable(),IO_NOTREADABLE)
!
    call baff%fetch(path)
    call words%split(baff%gets()) ; atom = words%ToInt(1)
    call this%node(using)%put_caption(baff%gets())
    call CheckXyz(this,baff%iserr(),IO_FMTERR)
!
    if(this%natom==atom_def)then
        call this%atm%clear()
      call this%atm%reserve(atom)
      do i=1,atom
        read(baff%devn(),'(A,3F)',err=101,end=100) dumatm,dum
        call this%atm%push(trim(dumatm))
      enddo
100   CONTINUE
      this%natom = this%atm%size()
    endif
!
    call CheckXyz(this,this%natom/=atom.or.atom<=0,IO_NATOMERR)
    if(.not.this%isErr()) RETURN
101 this%nnode = this%nnode - 1
  end subroutine XyzFetch
!
  subroutine StackExtention(this)
  class(XYZFMT),intent(inout)  :: this
  integer                      :: old
  type(pathname),allocatable   :: Swp(:)
    if(this%stack>=this%nnode.and.this%stack>0)return
    if(this%stack<=0)this%stack = 1 ; old = this%stack
    do while(this%stack<this%nnode)
      this%stack = this%stack * 2
    enddo
    allocate(swp(this%stack))
    if(allocated(this%node)) swp(1:old) = this%node(1:old)
    call move_alloc(from=swp,to=this%node)
  end subroutine StackExtention
!
  subroutine XYZLoad(this)
  class(XYZFMT),intent(inout)  :: this
  integer                      :: is,nstack,using
    call RoutineNameIs('XYZFMT_LOAD')
    if(this%isErr().or.this%nnode<=0)RETURN
    nstack = 1 ; this%nframe = 0
    if(allocated(this%xyz)) deallocate(this%xyz)
    do using=1,this%nnode
      call getxyz(this,nstack)
    enddo
  contains
    subroutine getxyz(this,nstack)
    class(XYZFMT),intent(inout)  :: this
    integer,intent(inout)        :: nstack
    type(stdio)                  :: baff
    real                         :: tmp(spatial_def,this%natom)
    type(vector_character)       :: words
    character(80)                :: line
    integer                      :: i,j,atom
      do
        read(baff%devn(),'(I0/A)',err=101,end=100) atom,line
        call CheckXyz(this,this%natom/=atom.or.atom<=0,IO_NATOMERR)
        do i=1,this%natom
          read(baff%devn(),'(A,3F)',err=101,end=100)line,tmp(:,i)
        enddo
        this%nframe = this%nframe + 1 ; call TrjExpand(this,nstack)
        this%xyz(:,:,this%nframe) = tmp
      enddo
100   RETURN
101   call CheckXyz(this,.TRUE.,IO_FMTERR)
    end subroutine getxyz
!
    pure subroutine TrjExpand(this,nstack)
    class(XYZFMT),intent(inout)  :: this
    integer,intent(inout)        :: nstack
    real,allocatable             :: tmp(:,:,:)
    integer                      :: old
      if(nstack>this%nframe)return
      old = nstack
      do while(this%nframe>nstack)
        nstack = nstack * 2
      enddo
!
      allocate(tmp(spatial_def,this%natom,nstack))
      if(allocated(this%xyz)) tmp(:,:,:old) = this%xyz(:,:,:old)
      call move_alloc(from=tmp,to=this%xyz)
    end subroutine TrjExpand
  end subroutine XyzLoad
!
  pure function XyzAtoms(this) result(res)
  class(XYZFMT),intent(in)                 :: this
  character(this%atm%maxlength()),allocatable :: res(:)
    allocate(res(this%natoms())) ; res = this%atm%lookup()
  end function XyzAtoms
!
  pure integer function XyzNatoms(this) result(res)
  class(XYZFMT),intent(in)  :: this
    res = maxval([this%natom,0],1)
  end function XyzNatoms
!
  pure integer function XyzNframes(this) result(res)
  class(XYZFMT),intent(in)  :: this
    res = this%nframe
  end function XyzNframes
!
  pure integer function XyzNmasks(this) result(res)
  class(XYZFMT),intent(in)  :: this
    res = maxval([this%nmask,0],1)
  end function XyzNmasks
!
  pure integer function XyzNnodes(this) result(res)
  class(XYZFMT),intent(in)  :: this
    res = this%nnode
  end function XyzNnodes
!
  pure logical function XyzIsERR(this) result (res)
  class(XYZFMT),intent(in)  :: this
    res = this%stat /= MOLTYPES_NOERR
  end function XyzIsERR
!
  subroutine CheckXyz(this,test,ierr)
  class(XYZFMT),intent(inout)  :: this
  logical,intent(in)           :: test
  integer,intent(in)           :: ierr
    if(.not.test) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call XyzDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end subroutine CheckXyz
!
  subroutine XyzClear(this)
  class(XYZFMT),intent(inout)  :: this
    this%natom = atom_def ; this%nnode  = node_def
    this%nmask = atom_def ; this%nframe = frame_def
    this%stat  = MOLTYPES_NOERR ; this%stack  = node_def
    if(allocated(this%node)) deallocate(this%node)
    call this%atm%clear()
    if(allocated(this%xyz))  deallocate(this%xyz)
  end subroutine XyzClear
!
  subroutine XyzDestractor(this)
  type(XYZFMT),intent(inout)  :: this
    call this%clear()
  end subroutine XyzDestractor
end module moltypes_xyz
