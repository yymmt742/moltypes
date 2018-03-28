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
  integer,parameter  :: spatial_def      = 3
  integer,parameter  :: atom_def         =-1
  integer,parameter  :: cell_spatial_def = 3
  integer,parameter  :: label_def        = 5
  integer,parameter  :: cell_angular_def = 3
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
    integer,public                      :: nnode  = node_def, stack   = node_def
    integer,public                      :: natoms = atom_def, nframes = frame_def
    integer                             :: stat  = MOLTYPES_NOERR
    integer,allocatable                 :: head(:)
    logical,public                      :: terminates_at_abnormal = terminates_default
  contains
    procedure         :: fetch              => AncFetch
    procedure         :: load               => AncLoad
    procedure         :: isErr              => AncIsErr
    procedure         :: clear              => AncClear
    final             :: AncDestractor
  end type AmberNetcdf
contains
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
    if(this%natoms==atom_def)then
      this%natoms = this%Node(using)%atom
    endif
    if(this%Node(using)%frame==0) RETURN
    if(this%nframes==frame_def) this%nframes = 0
    this%head(using) = this%nframes
    this%nframes = this%nframes + this%Node(using)%frame
  contains
    logical function Invalid(Node) result(res)
      type(AmberNetcdfNode) :: Node
      res = ANY([Node%spatial/=spatial_def,&
          &      Node%cell_spatial/=cell_spatial_def,&
          &      Node%cell_angular/=cell_angular_def,&
          &      Node%label/=label_def,&
          &      Node%atom<=0,Node%frame<0])
      if(this%natoms==atom_def) RETURN
      res = res.or.this%natoms/=Node%atom
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
  subroutine AncLoad(this,atm,frame,xyz,vel,frc,box,ang,time,lb,ub,inc,mask)
  use spur_itertools, only : IterScope
  use spur_shapeshifter
  class(AmberNetcdf),intent(inout)        :: this
  integer,intent(inout),optional          :: atm,frame
  real,intent(inout),allocatable,optional :: xyz(:,:,:),vel(:,:,:),frc(:,:,:),time(:)
  real,intent(inout),allocatable,optional :: box(:,:),ang(:,:)
  integer,intent(in),optional             :: lb,ub,inc
  logical,intent(in),optional             :: mask(:)
  logical,allocatable                     :: lmask(:,:)
  integer                                 :: nframes,nmask
  integer                                 :: llb,lub,linc
  integer                                 :: i,j,k,using
    if(this%nframes<1)RETURN
    call RoutineNameIs('LOAD_AMBERNETCDF')
!
    llb  =  1 ; if(present(lb))  llb  = lb
    lub  = -1 ; if(present(ub))  lub  = ub
    linc =  1 ; if(present(inc)) linc = inc
!
    call IterScope(this%nframes,llb,lub,linc,nframes)
!
    allocate(lmask(spatial_def,this%natoms)) ; lmask = .TRUE.
    if(present(mask)) lmask = SpreadMask(mask,this%natoms,spatial_def)
!
    nmask = count(lmask)/3 ; if(nmask<=0) RETURN
!
    if(present(xyz))then
      if(present(mask).and.nmask/=this%natoms)then
        call get3d_mask(this,'coordinates',llb,lub,linc,spatial_def,nmask,nframes,this%natoms,lmask,xyz)
      else
        call get3d(this,'coordinates',llb,lub,linc,spatial_def,nmask,nframes,xyz)
      endif
    endif
    if(present(vel))then
      if(present(mask).and.nmask/=this%natoms)then
        call get3d_mask(this,'velocities',llb,lub,linc,spatial_def,nmask,nframes,this%natoms,lmask,vel)
      else
        call get3d(this,'velocities',llb,lub,linc,spatial_def,nmask,nframes,vel)
      endif
    endif
    if(present(frc))then
      if(present(mask).and.nmask/=this%natoms)then
        call get3d_mask(this,'forces',llb,lub,linc,spatial_def,nmask,nframes,this%natoms,lmask,frc)
      else
        call get3d(this,'forces',llb,lub,linc,spatial_def,nmask,nframes,frc)
      endif
    endif
!
    if(present(time)) call get1d(this,'time',llb,lub,linc,nframes,time)
!
    if(present(box))then
      call get2d(this,'cell_lengths',llb,lub,linc,cell_spatial_def,nframes,box)
    endif
    if(present(ang))then
      call get2d(this,'cell_angles',llb,lub,linc,cell_spatial_def,nframes,ang)
    endif
!
    do using=1,this%nnode
      if(CheckNode(this%Node(using),IO_NCFMTERR)) RETURN
    enddo
    if(present(atm))   atm   = nmask
    if(present(frame)) frame = nframes
  contains
    subroutine get1d(this,var,llb,lub,linc,s1,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    integer,intent(in)                :: s1
    real,intent(inout),allocatable    :: val(:)
    integer                           :: at,i,j
      if(size(val)<=s1)then
        if(allocated(val)) deallocate(val) ; allocate(val(s1))
      endif
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,val(i:i),from=[at])
        call this%node(using)%path%quit()
      enddo
    end subroutine get1d
!
    subroutine get2d(this,var,llb,lub,linc,s1,s2,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    integer,intent(in)                :: s1,s2
    real,intent(inout),allocatable    :: val(:,:)
    integer                           :: at,i,j
      if(LT_shape(shape(val),[s1,s2]))then
        if(allocated(val)) deallocate(val) ; allocate(val(s1,s2))
      endif
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,val(:,j:j),from=[1,at])
        call this%node(using)%path%quit()
      enddo
    end subroutine get2d
!
    subroutine get3d(this,var,llb,lub,linc,s1,s2,s3,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    integer,intent(in)                :: s1,s2,s3
    real,intent(inout),allocatable    :: val(:,:,:)
    integer                           :: at,i,j
      if(LT_shape(shape(val),[s1,s2,s3]))then
        if(allocated(val)) deallocate(val) ; allocate(val(s1,s2,s3))
      endif
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,val(:,:,j:j),from=[1,1,at])
        call this%node(using)%path%quit()
      enddo
    end subroutine get3d
!
    subroutine get3d_mask(this,var,llb,lub,linc,s1,s2,s3,natoms,mask,val)
    class(AmberNetcdf),intent(inout)  :: this
    character(*),intent(in)           :: var
    integer,intent(in)                :: llb,lub,linc
    integer,intent(in)                :: s1,s2,s3,natoms
    logical,intent(in)                :: mask(s1,natoms)
    real,intent(inout),allocatable    :: val(:,:,:)
    real                              :: tmp(s1,natoms,1)
    integer                           :: at,i,j
      if(LT_shape(shape(val),[s1,s2,s3]))then
        if(allocated(val)) deallocate(val) ; allocate(val(s1,s2,s3))
      endif
      j = 0
      do i=llb,lub,linc
        using = cued(this%nnode,this%head(1:this%nnode),i)
        at    = i - this%head(using) ; j = j + 1
        call this%node(using)%path%get(var,tmp,from=[1,1,at])
        val(:,:,j) = reshape(pack(tmp(:,:,1),mask),[spatial_def,this%natoms])
        call this%node(using)%path%quit()
      enddo
    end subroutine get3d_mask
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
  pure logical function AncIsERR(this) result (res)
  class(AmberNetcdf),intent(in) :: this
    res = this%stat /= MOLTYPES_NOERR
  end function AncIsERR
!
  subroutine AncClear(this)
  class(AmberNetcdf),intent(inout)  :: this
    this%nnode  = node_def ; this%stack   = node_def
    this%natoms = atom_def ; this%nframes = frame_def
    this%stat  = MOLTYPES_NOERR
    if(allocated(this%Node)) deallocate(this%Node)
    if(allocated(this%head)) deallocate(this%head)
  end subroutine AncClear
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
