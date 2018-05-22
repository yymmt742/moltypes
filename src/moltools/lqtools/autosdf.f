program autosdf
implicit none
  call compute()
contains
  subroutine compute()
  use spur_optparse
  use moltypes_parser
  use spur_histgram
  type(molparser)   :: trj
  type(optparse)    :: arg
  real,allocatable  :: orig(:,:,:),vecx(:,:,:),vecy(:,:,:),vecz(:,:,:)
  real,allocatable  :: rbox(:,:)
  integer           :: natm,ntrj,nres
  integer           :: i,j,k
  real              :: radial,space(3),v(3)
  double precision  :: vol,volr,vols,rcpn,rcpdr,rcpdr3
  type(histgram_1d) :: rdf
  type(histgram_3d) :: sdf
    call arg%add_option("-o",narg=1,metavar=['path'],def=['default'],help='output file.')
    call arg%add_option("-r",narg=1,metavar=['value'],def=['1.0'], help='radial bin size. default [1.0] kcal/mol')
    call arg%add_option("-R",narg=1,metavar=['value'],def=['30.0'],help='radial maxval.   default [30.0] angs.')
    call arg%add_option("-c",narg=1,metavar=['mask'],help='Load mask string, for origin.')
    call arg%add_option("-x",narg=1,metavar=['mask'],help='Load mask string, for x axis.')
    call arg%add_option("-y",narg=1,metavar=['mask'],help='Load mask string, for y axis.')
    call arg%add_option("-s",narg=1,metavar=['symmetry'],  help='symmetry group')
    call arg%add_option("--cutoff",narg=1,metavar=['Mask'],def=['999'],help='cutoff length.default [999.0] angs.')
    call arg%parser()
    if(arg%narg()==0.or..not.arg%option('-c').or..not.arg%option('-x')) call arg%call_usage()
!
    call trj%fetch(arg%args())
    if(arg%option('-m')) call trj%atomselect(mask=arg%optargs('-m',1))
    natm = trj%nfetchatoms()
    nres = trj%nresidues()
    ntrj = trj%nfetchframes()
    if(any([natm,nres,ntrj]<1)) STOP "this trajectory is empty!"
!
    allocate(vecx(3,nres,ntrj),vecy(3,nres,ntrj),vecz(3,nres,ntrj),orig(3,nres,ntrj))
    allocate(rbox(3,ntrj))
!
    call trj%load()
    rbox = 1.0/trj%box(:,:ntrj)
!
    orig = trj%center_of_residue(arg%optargs('-c',1))
    vecx = trj%center_of_residue(arg%optargs('-x',1))
    vecy = trj%center_of_residue(arg%optargs('-y',1))
!
    call set_vxyz(nres,ntrj,orig,vecx,vecy,vecz)
!
    call rdf%setup(1.d0,arg%optargd('-R',1),arg%optargd('-r',1))
    call sdf%setup(-arg%optargd('-R',1),arg%optargd('-R',1),arg%optargd('-r',1))
!
    rcpn   = 1.d0 / dble(nres*nres*ntrj)
    rcpdr  = arg%optargd('-r',1)
    rcpdr3 = rcpn /(rcpdr * rcpdr * rcpdr)
    rcpdr  = 2.d0 * rcpn / rcpdr
!
    do k=1,ntrj
      vol  = trj%box(1,k)*trj%box(2,k)*trj%box(3,k)
      volr = vol  * rcpdr
      vols = vol  * rcpdr3
      do j=1,nres
        do i=1,nres
          if(i==j)CYCLE
          v = orig(:,i,k) - orig(:,j,k)
          v = v - trj%box(:,k) * anint(v*rbox(:,k))
          radial    = sqrt(dot_product(v,v))
          space(1)  = dot_product(vecx(:,i,k),v)
          space(2)  = dot_product(vecy(:,i,k),v)
          space(3)  = dot_product(vecz(:,i,k),v)
!          call sdf%stack(dble(space),vols)
          if(i<j)CYCLE
          call rdf%stack(dble(radial),volr)
        enddo 
      enddo
    enddo
    deallocate(orig,vecx,vecy,vecz,rbox)
!
    call rdf%scale(0.07957747154d0,-2.d0)
    call rdf%export(trim(arg%optargs('-o',1))//'.rdf')
return
    call sdf%symmetry(arg%optargs('-s',1))
    call sdf%export_dx(trim(arg%optargs('-o',1))//'.dx',cutoff=arg%optargd('--cutoff',1),cutval=1.d0)
  end subroutine compute
!
  pure subroutine set_vxyz(nres,nframe,orig,vx,vy,vz)
  integer,intent(in)                :: nres,nframe
  real,intent(in)                   :: orig(3,nres,nframe)
  real,intent(inout)                :: vx(3,nres,nframe),vy(3,nres,nframe),vz(3,nres,nframe)
  integer                           :: i,j
    do j=1,nframe
      do i=1,nres
        vx(:,i,j) = vx(:,i,j) - orig(:,i,j)
        vx(:,i,j) = vx(:,i,j) / sqrt(dot_product(vx(:,i,j),vx(:,i,j)))
      enddo
    enddo
    do j=1,nframe
      do i=1,nres
        vy(:,i,j) = vy(:,i,j) - orig(:,i,j)
        vy(:,i,j) = vy(:,i,j) / sqrt(dot_product(vy(:,i,j),vy(:,i,j)))
      enddo
    enddo
    do j=1,nframe
      do i=1,nres
        vz(1,i,j) = vx(2,i,j)*vy(3,i,j)-vx(3,i,j)*vy(2,i,j)
        vz(2,i,j) = vx(3,i,j)*vy(1,i,j)-vx(1,i,j)*vy(3,i,j)
        vz(3,i,j) = vx(1,i,j)*vy(2,i,j)-vx(2,i,j)*vy(1,i,j)
        vz(:,i,j) = vz(:,i,j) / sqrt(dot_product(vz(:,i,j),vz(:,i,j)))
        vy(1,i,j) = vx(2,i,j)*vz(3,i,j)-vx(3,i,j)*vz(2,i,j)
        vy(2,i,j) = vx(3,i,j)*vz(1,i,j)-vx(1,i,j)*vz(3,i,j)
        vy(3,i,j) = vx(1,i,j)*vz(2,i,j)-vx(2,i,j)*vz(1,i,j)
      enddo
    enddo
  end subroutine set_vxyz
end program autosdf
