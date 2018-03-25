module moltypes_perser
  use moltypes
  implicit none
  private
  public :: molperser
!
  integer,parameter :: spatial = 3
!
  type,extends(moltype) :: molperser
    private
  contains
    procedure,private :: center_idx
    procedure,private :: center_range
    procedure,private :: center_all
    generic           :: center_of_coordinates => center_idx, center_range, center_all
    procedure,private :: centering_all
    procedure,private :: centering_mask
    generic           :: centering_coordinates => centering_all, centering_mask
    procedure         :: unwrap_coordinates
  end type molperser
contains
  pure function center_idx(this,idx,mask) result(res)
  class(molperser),intent(in)      :: this
  integer,intent(in)               :: idx
  character(*),intent(in),optional :: mask
  logical,allocatable              :: slct(:,:)
  real                             :: res(spatial),ncnt
    res = 0.0 ; if(this%out_of_range(idx)) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      res = sum(this%xyz(:,:,idx),dim=2,mask=slct) / ncnt
    else
      res = sum(this%xyz(:,:,idx),dim=2) / real(this%natoms())
    endif
  end function center_idx
!
  pure function center_range(this,lb,ub,mask) result(res)
  class(molperser),intent(in)      :: this
  integer,intent(in)               :: lb,ub
  character(*),intent(in),optional :: mask
  real                             :: res(spatial,maxval([0,ub-lb+1],1))
  logical,allocatable              :: slct(:,:)
  real                             :: ncnt
  integer                          :: i
    res = 0.0 ; if(this%out_of_range([lb,ub])) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      ncnt = 1.0 / ncnt
      do i=lb,ub
        res(:,i) = sum(this%xyz(:,:,i),dim=2,mask=slct) * ncnt
      enddo
    else
      res = sum(this%xyz(:,:,lb:ub),dim=2) / real(this%natoms())
    endif
  end function center_range
!
  pure function center_all(this,mask) result(res)
  class(molperser),intent(in)      :: this
  character(*),intent(in),optional :: mask
  real                             :: res(spatial,this%nframes())
  logical,allocatable              :: slct(:,:)
  real                             :: ncnt
  integer                          :: i
    res = 0.0 ; if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      ncnt = 1.0 / ncnt
      do i=1,this%nframes()
        res(:,i) = sum(this%xyz(:,:,i),dim=2,mask=slct) * ncnt
      enddo
    else
      res = sum(this%xyz(:,:,1:this%nframes()),dim=2) / real(this%natoms())
    endif
  end function center_all
!
  pure elemental subroutine centering_all(this)
  class(molperser),intent(inout)   :: this
  real                             :: com(spatial)
  integer                          :: i,j
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    do j=1,this%nframes()
      com = center_idx(this,j)
      do i=1,this%natoms()
        this%xyz(:,i,j) = this%xyz(:,i,j) - com
      enddo
    enddo
  end subroutine centering_all
!
  pure elemental subroutine centering_mask(this,mask)
  class(molperser),intent(inout)   :: this
  character(*),intent(in)          :: mask
  real                             :: com(spatial,this%nframes())
  integer                          :: i,j
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    com = center_all(this,mask)
    do j=1,this%nframes()
      do i=1,this%natoms()
        this%xyz(:,i,j) = this%xyz(:,i,j) - com(:,j)
      enddo
    enddo
  end subroutine centering_mask
!
! pure function center_of_residue(this,mask) result(res)
! class(moltype),intent(in)        :: this
! character(*),intent(in),optional :: mask
! real,allocatable                 :: res(:,:,:)
! integer,allocatable              :: resid(:)
! integer                          :: i,j,natm,nres,nframe
! logical,allocatable              :: selection(:)
! real                             :: com(3),revn
!   natm  = this%natoms() ; nframe = this%nframes() ; nres  = this%nresidues()
!   allocate(res(3,nres,nframe))
!   if(natm<=0.or.nframe<=0) RETURN
!
!   allocate(resid(nres)) ; resid = this%inq('resid',1)
!   call comcalc(natm,nres,maxval(resid,1),nframe,this%xyz(),resid,res)
! contains
!   pure subroutine comcalc(natm,nres,nmax,nframe,xyz,resid,res)
!   integer,intent(in)           :: natm,nres,nmax,nframe
!   real,intent(in)              :: xyz(3,natm,nframe)
!   integer,intent(in)           :: resid(natm)
!   real,intent(out)             :: res(3,nres,nframe)
!   real                         :: num(nmax)
!   logical                      :: mask(nmax)
!   integer                      :: k
!     num = 0.0
!     do k=1,NATM
!       num(resid(k)) = num(resid(k)) + 1.0
!     enddo
!     mask = num<1.0E-10
!     do k=1,nmax
!       if(mask(k))cycle ; num(k) = 1.0 / num(k)
!     enddo
!
!     do concurrent (k=1:nframe) ; block
!       integer :: i,j,k
!       real    :: com(3,nmax)
!       com = 0.0
!       do i=1,NATM
!         com(:,resid(i)) = com(:,resid(i)) + xyz(:,i,k)
!       enddo
!       j = 0
!       do i=1,nmax
!         if(mask(i)) CYCLE
!         j = j + 1 ; res(:,j,k) = com(:,i) * num(i)
!       enddo
!     end block ; enddo
!   end subroutine comcalc
! end function center_of_residue
!
  pure subroutine unwrap_coordinates(this)
  class(molperser),intent(inout) :: this
  real                           :: offset(spatial,this%natoms())
  real                           :: prev(spatial,this%natoms())
  real                           :: width(spatial),test(spatial)
  integer                        :: i,j,k
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz).or..not.allocated(this%box)) RETURN
!
    offset = 0.0
    do k=2,this%nframes()
      prev  = this%xyz(:,:,k-1)
      width = this%box(:,k) * 0.5
      do j=1,this%natoms()
        this%xyz(:,j,k) = this%xyz(:,j,k) + offset(:,j) * this%box(:,k)
        test(:) = this%xyz(:,j,k) - prev(:,j)
        do i=1,spatial
          if(test(i)>width(i))then
            offset(i,j) = offset(i,j) - 1.d0
            this%xyz(i,j,k)  = this%xyz(i,j,k) - this%box(i,k)
          elseif(test(i)<-width(i))then
            offset(i,j) = offset(i,j) + 1.d0
            this%xyz(i,j,k) = this%xyz(i,j,k) + this%box(i,k)
          endif
        enddo
      enddo
    enddo
  end subroutine unwrap_coordinates
end module moltypes_perser
