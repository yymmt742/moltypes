program sdf
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use spur_optparse
use moltypes
use spur_io
use spur_vector
use spur_histgram
implicit none
type(vector_character),save   :: inp
character(:),allocatable,save :: sout,rout
character(:),allocatable,save :: o,x,y,d,loadmask
character(:),allocatable,save :: sym
double precision              :: rmax,rgrd,smax,sgrd
type(optparse)                :: arg
integer                       :: is
  call arg%add_option("-m",narg=4,metavar='MaskString',&
                     &help='Load mask string <from> <to>, like as vmd.')
  call arg%add_option("-o",narg=2,metavar='filename',&
                     &help='output file of rdf/sdf')
  call arg%add_option("-g",narg=4,metavar='value',def=['1.0','30 ','1.0','20 '],&
                     &help='grid of rdf/sdf, max range of rdf/sdf')
  call arg%add_option("-s",narg=1,metavar='symmetry',&
                     &help='symmetry group')
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
!
  rgrd  = arg%optargd('-g',1)
  rmax  = arg%optargd('-g',2)
  sgrd  = arg%optargd('-g',3)
  smax  = arg%optargd('-g',4)
  allocate(character(0)::sym)
  if(arg%option("-s")) sym = arg%optargs('-s',1)
!
  call inp%push(arg%args())
  allocate(character(0)::sout); sout = arg%optargs('-o',1)
  allocate(character(0)::rout); rout = arg%optargs('-o',2)
  allocate(character(0)::o)   ; o = trim(arg%optargs('-m',1))
  allocate(character(0)::x)   ; x = trim(arg%optargs('-m',2))
  allocate(character(0)::y)   ; y = trim(arg%optargs('-m',3))
  allocate(character(0)::d)   ; d = trim(arg%optargs('-m',4))
  allocate(character(0)::loadmask)
  if(o/='')loadmask = o
  if(x/='')then
    if(loadmask/='')loadmask = loadmask//' or ' ; loadmask = loadmask//x
  endif
  if(y/='')then
    if(loadmask/='')loadmask = loadmask//' or ' ; loadmask = loadmask//y
  endif
  if(d/='')then
    if(loadmask/='')loadmask = loadmask//' or ' ; loadmask = loadmask//d
  endif
!
  call mlts_compute_sdf()
!
contains
  subroutine mlts_compute_sdf()
  type(trajectory)               :: trj
  double precision,allocatable    :: origin(:,:),dest(:,:)
  double precision,allocatable    :: vxyz(:,:)
  logical,allocatable             :: SameResidue(:,:)
  integer                         :: nres,dres,ntrj
  double precision                :: rcprgrd,rcpsgrd,rcpn
  integer                         :: i,j,progres
  type(histgram_1d)               :: rdf
  type(histgram_3d)               :: sdf
    call trj%load(inp%lookup(),mask=loadmask,from=1)
!
    call read_dimension(trj,o,d,x,y,nres,dres,ntrj)
    if(nres<=0.or.dres<=0.or.ntrj<=0)RETURN
!
    allocate(origin(3,nres),vxyz(9,nres),dest(3,dres),SameResidue(dres,nres))
!
    call rdf%setup(1.d0,rmax,rgrd)
    call sdf%setup(-smax,smax,sgrd)
!
    call get_mask(trj,o,d,nres,dres,SameResidue)
!
    rcprgrd = 1.d0 / rgrd
    rcpsgrd = 1.d0 / sgrd
    rcpn   = 1.d0 / dble(ntrj*count(.not.SameResidue))
!
    write(stdout,'(a,i0,a)') 'Processing distribution... :: ',ntrj,' frames'
    write(stdout,'(a)') '|----------------------------------------|'
    write(stdout,'(a)',advance='no') '|' ; flush(stdout)
!
    progres = 0
    do i=1,ntrj
      call get_coordinate(trj,o,d,x,y,nres,dres,i,origin,dest,vxyz)
      call calc_distribution(nres,dres,rcprgrd,rcpsgrd,rcpn,origin,vxyz,dest,trj%box(:,i),SameResidue,rdf,sdf)
      call PrintCounter(ntrj,i,progres)
    enddo
    write(stdout,'(a)') '|' ; flush(stdout)
!
    call rdf%scale(0.07957747154d0,-2.d0)
!
    write(stdout,'(a)',advance='yes') 'rdf writing...'
    call rdf%export(rout)
    write(stdout,'(a)',advance='yes') 'sdf writing...'
    call sdf%symmetry(sym)
    call sdf%export_dx(sout)
!
    deallocate(origin,vxyz,dest,SameResidue)
    RETURN
  end subroutine mlts_compute_sdf
!
  subroutine read_dimension(trj,o,d,x,y,nres,dres,ntrj)
  class(trajectory),intent(inout)          :: trj
  character(*),intent(in)                  :: o,d,x,y
  integer,intent(out)                      :: nres,dres,ntrj
    call trj%atomselect(o) ; nres = trj%n_residues() ; ntrj = trj%n_frames()
    call trj%atomselect(d) ; dres = trj%n_atoms()
    call trj%atomselect(x) ; if(nres/=trj%n_residues()) nres=0
    call trj%atomselect(y) ; if(nres/=trj%n_residues()) nres=0
  end subroutine read_dimension
!
  subroutine get_mask(trj,o,d,nres,dres,mask)
  class(trajectory),intent(inout) :: trj
  character(*),intent(in)         :: o,d
  integer,intent(in)              :: nres,dres
  logical,intent(out)             :: mask(dres,nres)
  type(vector_integer)            :: uniqres
  integer                         :: oresid(nres),dresid(dres)
  integer                         :: i
    call trj%atomselect(o)
    uniqres = trj%residue_index() ; uniqres = uniqres%uniq()
    call trj%atomselect(d)
    oresid = uniqres%lookup() ; dresid = trj%residue_index()
    do i=1,nres
      mask(:,i) = dresid(:)==oresid(i)
    enddo
  end subroutine get_mask
!
  subroutine get_coordinate(trj,o,d,x,y,nres,dres,ntrj,origin,dest,vxyz)
  class(trajectory),intent(inout)  :: trj
  character(*),intent(in)          :: o,d,x,y
  integer,intent(in)               :: nres,dres,ntrj
  double precision,intent(out)     :: origin(3,nres),dest(3,dres)
  double precision,intent(out)     :: vxyz(9,nres)
  double precision                 :: vx(3,nres),vy(3,nres)
    call trj%atomselect(o) ; origin = trj%center_residue(ntrj)
    call trj%atomselect(d) ; dest   = trj%coordinates(ntrj)
    call trj%atomselect(x) ; vx     = trj%center_residue(ntrj)
    call trj%atomselect(y) ; vy     = trj%center_residue(ntrj)
    call baseset(nres,vx-origin,vy-origin,vxyz)
  end subroutine get_coordinate
!
  subroutine baseset(nres,vx,vy,vxyz)
!$ use omp_lib
  integer,intent(in)               :: nres
  double precision,intent(in)      :: vx(3,nres),vy(3,nres)
  double precision,intent(out)     :: vxyz(9,nres)
  double precision                 :: vt(3),vz(3)
  integer                          :: i
!$omp parallel do private(i,vt,vz)
    do i=1,nres
      vz(1) = vx(2,i)*vy(3,i)-vx(3,i)*vy(2,i)
      vz(2) = vx(3,i)*vy(1,i)-vx(1,i)*vy(3,i)
      vz(3) = vx(1,i)*vy(2,i)-vx(2,i)*vy(1,i)
      vt(1) = vx(2,i)*vz(3)-vx(3,i)*vz(2)
      vt(2) = vx(3,i)*vz(1)-vx(1,i)*vz(3)
      vt(3) = vx(1,i)*vz(2)-vx(2,i)*vz(1)
      vxyz(1:3,i) = vz(:) / sqrt(dot_product(vz(:),vz(:)))
      vxyz(4:6,i) = vt(:) / sqrt(dot_product(vt(:),vt(:)))
      vxyz(7:9,i) = vx(:,i) / sqrt(dot_product(vx(:,i),vx(:,i)))
    enddo
!$omp end parallel do
  end subroutine baseset
!
  subroutine calc_distribution(nres,dres,rcprgrd,rcpsgrd,rcpn,orig,vxyz,dest,box,SameResidue,rdf,sdf)
!$ use omp_lib
  integer,intent(in)              :: nres,dres
  double precision,intent(in)     :: rcprgrd,rcpsgrd,rcpn
  double precision,intent(in)     :: orig(3,nres),vxyz(9,nres),dest(3,dres),box(3)
  logical,intent(in)              :: SameResidue(dres,nres)
  type(histgram_1d),intent(inout) :: rdf
  type(histgram_3d),intent(inout) :: sdf
  double precision                :: RcpBox(3),v(3),radial(dres),space(3,dres)
  double precision                :: volr,vols
  integer                         :: i,j
    do i=1,3
      if(box(i)<=1.0E-5)then
        rcpbox(i) = 0.d0
      else
        rcpbox(i) = 1.d0/box(i)
      endif
    enddo
    volr = box(1)*box(2)*box(3)
    if(volr<=1.0E-5)then
      vols = rcpn
      volr = rcpn
    else
      vols = volr*rcpn*rcpsgrd*rcpsgrd*rcpsgrd
      volr = volr*rcpn*rcprgrd
    endif
!
    do j=1,nres
!!$omp parallel private(i,v)
!!$omp do
      do i=1,nres
        if(SameResidue(i,j))then
          radial(i) = 0.d0 ; space(:,i) = HUGE(0.d0) ; CYCLE
        endif
        v = orig(:,i) - orig(:,j)
        v = v - box * anint(v*rcpbox)
        radial(i)   = sqrt(dot_product(v,v))
        space(1,i)  = dot_product(vxyz(1:3,j),v)
        space(2,i)  = dot_product(vxyz(4:6,j),v)
        space(3,i)  = dot_product(vxyz(7:9,j),v)
      enddo 
!!$omp end do
!!$omp end parallel
      call rdf%stack(pack(radial,radial>1.d0),volr)
      call sdf%stack(pack(space,space<99999.d0),vols)
    enddo
  end subroutine calc_distribution

  subroutine PrintCounter(ntrj,counter,progres)
  integer,intent(in)  :: ntrj,counter
  integer,intent(out) :: progres
  integer             :: i,tmp
    tmp = NINT(real(40*counter)/real(ntrj))
    do i=progres+1,tmp
      write(stdout,'(a)',advance='no') '>'
      flush(stdout)
    enddo
    progres = tmp
  end subroutine PrintCounter
end program sdf
