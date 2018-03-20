module moltypes_ambernetcdf
  use moltypes_errorhandler
  use spur_vector
  use spur_ncio
  implicit none
  private
  public :: AmberNetcdf
!
  integer,parameter  :: node_def         =-1
  integer,parameter  :: frame_def        =-1
  integer,parameter  :: spatial_def      =3
  integer,parameter  :: atom_def         =-1
  integer,parameter  :: cell_spatial_def =3
  integer,parameter  :: label_def        =5
  integer,parameter  :: cell_angular_def =3
!
  type AmberNetcdfNode
    type(ncio)               :: path
    character(:),allocatable :: title,app,pro,prover,con,conver
    integer                  :: frame        = frame_def
    integer                  :: spatial      = spatial_def
    integer                  :: atom         = atom_def
    integer                  :: cell_spatial = cell_spatial_def
    integer                  :: label        = label_def
    integer                  :: cell_angular = cell_angular_def
  contains
    final             :: NodeDestractor
  end type AmberNetcdfNode
!
  type AmberNetcdf
    private
    type(AmberNetcdfNode),allocatable   :: Node(:)
    integer                             :: nnode = node_def, stack  = node_def
    integer                             :: natom = atom_def, nframe = frame_def
    integer                             :: nmask = atom_def, total = frame_def
    integer                             :: stat  = MOLTYPES_NOERR
    integer,allocatable                 :: head(:)
    real,allocatable,public             :: xyz(:,:,:),vel(:,:,:)
    real,allocatable,public             :: frc(:,:,:),time(:)
    double precision,allocatable,public :: box(:,:),ang(:,:)
    logical,public                      :: terminates_at_abnormal = terminates_default
    logical,public                      :: load_time = .FALSE.
    logical,public                      :: load_xyz  = .TRUE.
    logical,public                      :: load_vel  = .FALSE.
    logical,public                      :: load_frc  = .FALSE.
    logical,public                      :: load_box  = .TRUE.
    logical,public                      :: load_ang  = .FALSE.
  contains
    procedure,private :: AncFmtWrite
    generic           :: WRITE(formatted)   => AncFmtWrite
    procedure         :: fetch              => AncFetch
    procedure         :: load               => AncLoad
    procedure         :: nnodes             => AncNnodes
    procedure         :: nframes            => AncNframes
    procedure         :: natoms             => AncNatoms
    procedure         :: isErr              => AncIsErr
    procedure         :: clear              => AncClear
    final             :: AncDestractor
  end type AmberNetcdf
contains
  subroutine AncFmtWrite(this,unit,iotype,v_list,iostat,iomsg)
  use spur_string
  class(AmberNetcdf),intent(in):: this
  integer,intent(in)           :: unit
  character(*),intent(in)      :: iotype
  integer,intent(in)           :: v_list(:)
  integer,intent(out)          :: iostat
  character(*),intent(inout)   :: iomsg
  character(:),allocatable     :: space
  integer                      :: i,dig
    iostat = 0
    if(this%nnode<=0)then
      write(unit,'(a)',iostat=iostat,iomsg=iomsg) 'HERE IS EMPTY CONTAINER' ; RETURN
    endif
!
    write(unit,'(a,i0,a,i0,a,/)',iostat=iostat,iomsg=iomsg) 'HERE CONTAINS ',this%natom,' ATOMS / ',this%total,' FRAMES'
    dig = digit(this%nnode)
    allocate(character(dig+3)::space) ; space(:) = ''
    do i=1,this%nnode
      if(iostat/=0) RETURN
      write(unit,'(2a,/)',iostat=iostat,iomsg=iomsg) '['//padding(i,dig)//'] ',this%node(i)%path%is()
      write(unit,'(2a,i0)',iostat=iostat,iomsg=iomsg)   space,'NATOM  = ',this%node(i)%atom
      write(unit,'(2a,i0,/)',iostat=iostat,iomsg=iomsg) space,'NFRAME = ',this%node(i)%frame
    enddo
  end subroutine AncFmtWrite
!
  subroutine AncFetch(this,path)
  class(AmberNetcdf),intent(inout)  :: this
  character(*),intent(in)           :: path
  integer                           :: using
    call RoutineNameIs('AMBERNETCDF_FETCH')
    if(this%nnode<0) this%nnode = 0
    this%nnode = this%nnode + 1 ; using = this%nnode
    call stackExtention(this)
!
    call this%Node(using)%path%fetch(path)
    if(CheckNode(this%node(using),IO_NOTEXIST,this%node(using)%path%isnotExist())) RETURN
    if(CheckNode(this%node(using),IO_NOTREADABLE,this%node(using)%path%isnotReadable())) RETURN
!
    call this%Node(using)%path%Loadheader()
    this%Node(using)%frame        = this%Node(using)%path%dim_length('frame')
    this%Node(using)%spatial      = this%Node(using)%path%dim_length('spatial')
    this%Node(using)%atom         = this%Node(using)%path%dim_length('atom')
    this%Node(using)%cell_spatial = this%Node(using)%path%dim_length('cell_spatial')
    this%Node(using)%label        = this%Node(using)%path%dim_length('label')
    this%Node(using)%cell_angular = this%Node(using)%path%dim_length('cell_angular')
!
    if(CheckAnc(this,Invalid(this%Node(using)),IO_NCFMTERR))then
      this%nnode = this%nnode - 1 ; using = this%nnode ; RETURN
    endif
!
    if(this%natom==atom_def)then
      this%natom = this%Node(using)%atom ; this%nmask = this%natom
    endif
    if(this%Node(using)%frame==0) RETURN
    if(this%total==frame_def) this%total = 0
    this%head(using) = this%total
    this%total  = this%total + this%Node(using)%frame
    this%nframe = this%total
  contains
    logical function Invalid(Node) result(res)
      type(AmberNetcdfNode) :: Node
      res = ANY([Node%spatial/=spatial_def,&
          &      Node%cell_spatial/=cell_spatial_def,&
          &      Node%cell_angular/=cell_angular_def,&
          &      Node%label/=label_def,&
          &      Node%atom<=0,Node%frame<0])
      if(this%natom==atom_def) RETURN
      res = res.or.this%natom/=Node%atom
    end function Invalid
!
    pure subroutine StackExtention(this)
    class(AmberNetcdf),intent(inout)  :: this
    integer                           :: i,old
    type(AmberNetcdfNode),allocatable :: Swp(:)
    integer,allocatable               :: swph(:)
      if(this%Stack>=this%nnode.and.this%stack>0)RETURN
      if(this%Stack<=0)this%Stack = 1
      old = this%stack
      do while(this%stack<this%nnode)
        this%stack = this%stack * 2
      enddo
      allocate(swp(this%stack),swph(this%stack))
      if(allocated(this%node)) swp(1:old)  = this%node(1:old)
      if(allocated(this%head)) swph(1:old) = this%head(1:old)
      call move_alloc(from=swp, to=this%node)
      call move_alloc(from=swph,to=this%head)
      do i=old+1,this%stack
        this%node(i)%path%terminates_at_abnormal = this%terminates_at_abnormal
      enddo
    end subroutine StackExtention
  end subroutine AncFetch
!
  pure integer function AncNnodes(this) result(res)
  class(AmberNetcdf),intent(in)  :: this
    res = this%nnode
  end function AncNnodes
!
  pure integer function AncNatoms(this) result(res)
  class(AmberNetcdf),intent(in)  :: this
    res = this%nmask
  end function AncNatoms
!
  pure integer function AncNframes(this) result(res)
  class(AmberNetcdf),intent(in)  :: this
    res = this%nframe
  end function AncNframes
!
  subroutine AncLoad(this,lb,ub,inc,mask)
  use spur_itertools, only : IterScope
  use spur_shapeshifter
  class(AmberNetcdf),intent(inout)  :: this
  integer,intent(in),optional       :: lb,ub,inc
  logical,intent(in),optional       :: mask(:)
  logical,allocatable               :: lmask(:,:)
  logical                           :: usemask
  integer                           :: llb,lub,linc
  integer                           :: i,j,k,using
    if(this%total<=0)RETURN
    call RoutineNameIs('LOAD_AMBERNetCDF')
!
    llb  =  1 ; if(present(lb))  llb  = lb
    lub  = -1 ; if(present(ub))  lub  = ub
    linc =  1 ; if(present(inc)) linc = inc
    call IterScope(this%total,llb,lub,linc,this%nframe)
!
    allocate(lmask(spatial_def,this%natom)) ; lmask = .TRUE.
    if(present(mask)) lmask = SpreadMask(mask,this%natom,spatial_def)
!
    this%nmask = count(lmask)/3
    usemask = present(mask).and.this%nmask/=this%natom
!
    if(LT_shape(shape(this%xyz),[spatial_def,this%nmask,this%nframe]))then
      if(allocated(this%xyz)) deallocate(this%xyz)
      if(this%load_xyz) allocate(this%xyz(spatial_def,this%nmask,this%nframe))
    endif
    if(LT_shape(shape(this%vel),[spatial_def,this%nmask,this%nframe]))then
      if(allocated(this%vel)) deallocate(this%vel)
      if(this%load_vel) allocate(this%vel(spatial_def,this%nmask,this%nframe))
    endif
    if(LT_shape(shape(this%frc),[spatial_def,this%nmask,this%nframe]))then
      if(allocated(this%frc)) deallocate(this%frc)
      if(this%load_frc) allocate(this%frc(spatial_def,this%nmask,this%nframe))
    endif
!
    if(LT_shape(shape(this%time),[this%nframe]))then
      if(allocated(this%time)) deallocate(this%time)
      if(this%load_time) allocate(this%time(this%nframe))
    endif
    if(LT_shape(shape(this%box),[cell_spatial_def,this%nframe]))then
      if(allocated(this%box))  deallocate(this%box)
      if(this%load_box)  allocate(this%box(cell_spatial_def,this%nframe))
    endif
    if(LT_shape(shape(this%ang),[cell_angular_def,this%nframe]))then
      if(allocated(this%ang))  deallocate(this%ang)
      if(this%load_ang)  allocate(this%ang(cell_angular_def,this%nframe))
    endif
    if(this%nmask<=0) RETURN
!
    if(this%load_time)then
      call get1d(this,'time',llb,lub,linc,this%time)
    endif
    if(this%load_box)then
      call get2d(this,'cell_lengths',llb,lub,linc,this%box)
    endif
    if(this%load_ang)then
      call get2d(this,'cell_angles',llb,lub,linc,this%ang)
    endif
    if(this%load_xyz)then
      call get3d(this,'coordinates',llb,lub,linc,usemask,lmask,this%xyz)
    endif
    if(this%load_vel)then
      call get3d(this,'velocities',llb,lub,linc,usemask,lmask,this%vel)
    endif
    if(this%load_frc)then
      call get3d(this,'forces',llb,lub,linc,usemask,lmask,this%frc)
    endif
    do using=1,this%nnode
      if(CheckNode(this%Node(using),IO_NCFMTERR)) RETURN
    enddo
  contains
    subroutine get1d(this,var,llb,lub,linc,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    real,intent(out)                  :: val(:)
    integer                           :: at,i,j
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,val(i:i),from=[at])
        call this%node(using)%path%quit()
      enddo
    end subroutine get1d
!
    subroutine get2d(this,var,llb,lub,linc,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    double precision,intent(out)      :: val(:,:)
    integer                           :: at,i,j
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,val(:,j:j),from=[1,at])
        call this%node(using)%path%quit()
      enddo
    end subroutine get2d
!
    subroutine get3d(this,var,llb,lub,linc,usemask,mask,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    logical,intent(in)                :: usemask,mask(spatial_def,this%natom)
    real,intent(out)                  :: val(:,:,:)
    real                              :: tmp(spatial_def,this%natom,1)
    integer                           :: at,i,j
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        if(usemask)then
          call this%node(using)%path%get(var,tmp,from=[1,1,at])
          val(:,:,j) = reshape(pack(tmp(:,:,1),mask),[spatial_def,this%natom])
        else
          call this%node(using)%path%get(var,val(:,:,j:j),from=[1,1,at])
        endif
        call this%node(using)%path%quit()
      enddo
    end subroutine get3d
!
    pure integer function cued(n,header,idx) result(res)
    integer,intent(in) :: n,header(n),idx
    integer            :: i
      res = 0
      do i=1,n
        if(header(i)>=idx)EXIT ; res = res + 1
      enddo
    end function cued
  end subroutine AncLoad
!
  logical function CheckAnc(this,test,ierr) result(res)
  class(AmberNetcdf),intent(inout) :: this
  logical,intent(in)               :: test
  integer,intent(in)               :: ierr
    res = test ; if(.not.res) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call AncDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end function CheckAnc
!
  logical function CheckNode(this,ierr,test) result(res)
  type(AmberNetcdfNode),intent(inout) :: this
  integer,intent(in)                  :: ierr
  logical,intent(in),optional         :: test
    if(present(test))then
      res = test
    else
      res = this%path%iserr()
    endif
    if(.not.res) RETURN
    if(this%path%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr,this%path%is())
      call exit(ierr)
    endif
  end function CheckNode
!
  pure logical function AncIsERR(this) result (res)
  class(AmberNetcdf),intent(in) :: this
    res = this%stat /= MOLTYPES_NOERR
  end function AncIsERR
!
  subroutine AncClear(this)
  class(AmberNetcdf),intent(inout)  :: this
    this%nnode = node_def ; this%stack  = node_def
    this%natom = atom_def ; this%nframe = frame_def
    this%nmask = atom_def ; this%total = frame_def
    this%stat  = MOLTYPES_NOERR
    if(allocated(this%Node)) deallocate(this%Node)
    if(allocated(this%head)) deallocate(this%head)
    if(allocated(this%xyz))  deallocate(this%xyz)
    if(allocated(this%vel))  deallocate(this%vel)
    if(allocated(this%frc))  deallocate(this%frc)
    if(allocated(this%box))  deallocate(this%box)
    if(allocated(this%ang))  deallocate(this%ang)
  end subroutine AncClear
!
  subroutine NodeDestractor(this)
  type(AmberNetcdfNode),intent(inout) :: this
    call this%path%quit() ; call this%path%clear()
    this%frame        = frame_def
    this%spatial      = spatial_def
    this%atom         = atom_def
    this%cell_spatial = cell_spatial_def
    this%label        = label_def
    this%cell_angular = cell_angular_def
  end subroutine NodeDestractor
!
  subroutine AncDestractor(this)
  type(AmberNetcdf),intent(inout)  :: this
    call this%clear()
  end subroutine AncDestractor
end module moltypes_ambernetcdf
