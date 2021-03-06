module moltypes_export
  use moltypes_errorhandler
  implicit none
  private
  public :: ExportXYZ,ExportMdcrd,ExportRST7
  public :: GenerateAmberNetcdf,ExportAmberNetcdf
!
  character(12),parameter :: DEFAULT_NAME = 'default_name'
!
  integer,parameter       :: node             = -1
  integer,parameter       :: frame            = -1
  integer,parameter       :: spatial          =  3
  integer,parameter       :: atom             = -1
  integer,parameter       :: cell_spatial     =  3
  integer,parameter       :: label            =  5
  integer,parameter       :: cell_angular     =  3
contains
  subroutine ExportXYZ(path,natm,nframe,xyz,atm,title,ffmt,overwrite)
  use spur_stdio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm,nframe
  real,intent(in)                      :: xyz(spatial,natm,nframe)
  character(*),intent(in)              :: atm(natm)
  character(*),intent(in),optional     :: title,ffmt
  logical,intent(in),optional          :: overwrite
  character(:),allocatable             :: lffmt,ltitle
  type(stdio)                          :: fout
  integer                              :: i,j
    call RoutineNameIs('EXPORT_XYZFORMAT')
    if(CheckAbort(natm<1,IO_NATOMERR)) RETURN
    call fout%fetch(path)
    if(present(overwrite))then
      if(overwrite.and.path/='') call fout%generate()
    endif
    if(path/='') call fout%append()
 
    allocate(character(0)::lffmt,ltitle)
    if(present(ffmt))then  ; lffmt  = '(A,3'//ffmt//')'
    else ; lffmt  = '(A,3F16.9)'  ; endif
    if(present(title))then ; ltitle = title
    else ; ltitle = DEFAULT_NAME ; endif
    do j=1,nframe
      write(fout%devn(),'(I0,/,a)',ERR=100) natm,ltitle
      do i=1,natm
        write(fout%devn(),lffmt,ERR=100) atm(i),xyz(:,i,j)
      enddo
    enddo
    call fout%quit() ; RETURN
100 if(CheckAbort(.TRUE.,IO_WRITERR))RETURN
  end subroutine ExportXYZ
!
  subroutine ExportMdcrd(path,natm,nframe,xyz,box,ang,title,overwrite)
  use spur_stdio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm,nframe
  real,intent(in)                      :: xyz(spatial,natm,nframe),box(spatial,nframe)
  real,intent(in),optional             :: ang(spatial,nframe)
  character(*),intent(in),optional     :: title
  logical,intent(in),optional          :: overwrite
  character(:),allocatable             :: ltitle
  real                                 :: lang(spatial)
  type(stdio)                          :: fout
  integer                              :: i
    call RoutineNameIs('EXPORT_AMBERMDCRD')
    fout%terminates_at_abnormal = .TRUE.
    if(CheckAbort(natm<=0,IO_NATOMERR))RETURN
!
    allocate(character(0)::ltitle)
    if(present(title))then ; ltitle = title
    else ; ltitle = DEFAULT_NAME ; endif
!
    call fout%fetch(path)
!
    if(present(overwrite))then
      if(overwrite) call fout%delete()
    endif
    if(fout%isnotExist())then
      call fout%generate() ; call fout%puts(ltitle)
    endif
!
    call fout%append()
    lang = 90.0
    do i=1,nframe
      if(present(ang)) lang = ang(:,i)
      write(fout%devn(),'(10F8.3)',ERR=100) xyz(:,:,i)
      write(fout%devn(),'(6F8.3)', ERR=100) box(:,i),lang
    enddo
    call fout%quit() ; RETURN
100 if(CheckAbort(.TRUE.,IO_WRITERR)) RETURN
   end subroutine ExportMdcrd
!
  subroutine ExportRST7(path,natm,nframe,xyz,vel,box,ang,time,title)
  use spur_stdio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm,nframe
  real,intent(in)                      :: xyz(:,:,:)
  real,intent(in),optional             :: vel(:,:,:),box(:,:),ang(:,:),time(:)
  character(*),intent(in),optional     :: title
  character(:),allocatable             :: ltitle
  real                                 :: ltime,lboxang(2*spatial)
  type(stdio)                          :: fout
    call RoutineNameIs('EXPORT_AMBERRST7')
    if(CheckAbort(natm<1,IO_NATOMERR))RETURN
    if(all(shape(xyz)<[spatial,natm,nframe])) RETURN
!
    call fout%fetch(path)
    if(fout%is()/='') call fout%generate()
!
    allocate(character(0)::ltitle)
    if(present(title))then ; ltitle = title
    else ; ltitle = DEFAULT_NAME ; endif
!
    ltime = -1.0
    if(present(vel))then
      if(present(time))then
        if(all(shape(vel)>=[spatial,natm,nframe])) ltime = time(nframe)
      else
        ltime = 0.0
      endif
    endif
!
    lboxang = [0.0,0.0,0.0,90.0,90.0,90.0]
    if(present(box))then
      if(all(shape(box)>=[spatial,nframe])) lboxang(:spatial)   = box(:,nframe)
    endif
    if(present(ang))then
      if(all(shape(ang)>=[spatial,nframe])) lboxang(spatial+1:) = ang(:,nframe)
    endif
!
    call fout%puts(ltitle)
    if(ltime>=0.0)then
      write(fout%devn(),'(I5,5E15.7)',ERR=100) natm,ltime
    else
      write(fout%devn(),'(I5)',ERR=100) natm
    endif
    write(fout%devn(),'(6F12.7)',ERR=100) xyz(:,:,nframe)
    if(present(vel))then
      if(all(shape(vel)>=[spatial,natm,nframe])) write(fout%devn(),'(6F12.7)',ERR=100) vel(:,:,nframe)
    endif
    write(fout%devn(),'(6F12.7)',ERR=100) lboxang
    call fout%quit() ; RETURN
100 if(CheckAbort(.TRUE.,IO_WRITERR)) RETURN
  end subroutine ExportRST7
!
  subroutine GenerateAmberNetcdf(path,natm,xyz,vel,frc,box,time)
  use spur_ncio
  use spur_string, only : tostr
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm
  logical,intent(in),optional          :: xyz,vel,frc,box,time
  type(ncio)                           :: ncout
  logical                              :: spatial,cell_spatial,cell_angular
    call RoutineNameIs('EXPORT_AMBERNETCDF')
    ncout%terminates_at_abnormal = terminates_default
    if(CheckAbort(natm<1,IO_NATOMERR,path)) RETURN
    spatial = .FALSE. ; cell_spatial = .FALSE. ; cell_angular = .FALSE.
!
    call ncout%fetch(path) ; call ncout%generate()
!
    call ncout%add_dimension('frame,spatial=3,atom='//tostr(natm)//&
                            &',cell_spatial=3,label=5,cell_angular=3')
!
    if(present(time))then
      if(time)then
        call ncout%add_variable('float :: time[frame]')
        call ncout%put_attribute('time :: units=picosecond')
      endif
    endif
    if(present(xyz))then
      if(xyz)then
        spatial = spatial_set(spatial)
        call ncout%add_variable('float :: coordinates [spatial,atom,frame]')
        call ncout%put_attribute('coordinates :: units=angstrom')
      endif
    endif
    if(present(vel))then
      if(vel)then
        spatial = spatial_set(spatial)
        call ncout%add_variable('float :: velocities [spatial,atom,frame]')
      endif
    endif
    if(present(frc))then
      if(frc)then
        spatial = spatial_set(spatial)
        call ncout%add_variable('float :: forces [spatial,atom,frame]')
      endif
    endif
    if(present(box))then
      if(box)then
        cell_spatial = cell_spatial_set(cell_spatial)
        call ncout%add_variable( 'double:: cell_lengths[cell_spatial,frame]')
        call ncout%put_attribute('cell_lengths :: units=angstrom')
        cell_angular = cell_angular_set(cell_angular)
        call ncout%add_variable( 'double:: cell_angles [cell_angular,frame]')
        call ncout%put_attribute('cell_angles :: units=degree')
      endif
    endif
!
    call ncout%put_attribute('title=default_name')
    call ncout%put_attribute('application=AMBER')
    call ncout%put_attribute('program=moltypes')
    call ncout%put_attribute('programVersion=1.0')
    call ncout%put_attribute('Conventions=AMBER')
    call ncout%put_attribute('ConventionVersion=1.0')
!
    if(spatial)      call ncout%put('spatial',["x","y","z"],from=[1])
    if(cell_spatial) call ncout%put('cell_spatial',["a","b","c"],from=[1])
    if(cell_angular) call ncout%put('cell_angular',reshape(["a","l","p","h","a","b","e","t","a"," ","g","a","m","m","a"],[5,3]),from=[1,1])
    call ncout%quit()
  contains
    logical function spatial_set(spatial)
    logical,intent(in) :: spatial
      if(spatial) call ncout%add_variable('char  :: spatial[spatial]')
      spatial_set = .TRUE.
    end function spatial_set
!
    logical function cell_spatial_set(cell_spatial)
    logical,intent(in) :: cell_spatial
      if(cell_spatial) call ncout%add_variable( 'char  :: cell_spatial[cell_spatial]')
      cell_spatial_set = .TRUE.
    end function cell_spatial_set
!
    logical function cell_angular_set(cell_angular)
    logical,intent(in) :: cell_angular
      if(cell_angular) call ncout%add_variable( 'char  :: cell_angular[label,cell_angular]')
      cell_angular_set = .TRUE.
    end function cell_angular_set
  end subroutine GenerateAmberNetcdf

  subroutine ExportAmberNetcdf(path,natm,nframe,xyz,vel,frc,box,ang,time)
  use spur_ncio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm,nframe
  real,intent(in),optional             :: xyz(:,:,:),vel(:,:,:),frc(:,:,:)
  double precision,intent(in),optional :: box(spatial,nframe),ang(spatial,nframe)
  real,intent(in),optional             :: time(:)
  type(ncio)                           :: ncout
  integer                              :: frame
    call RoutineNameIs('EXPORT_AMBERNETCDF')
    ncout%terminates_at_abnormal = terminates_default
    if(CheckAbort(natm<1,IO_NATOMERR,path)) RETURN
!
    call ncout%fetch(path) ; if(CheckAbort(ncout%iserr(),IO_NCFMTERR,path)) RETURN
!
    if(ncout%isnotExist()) call GenerateAmberNetcdf(path,natm,present(xyz),present(vel),present(frc), &
                              &                     present(box).or.present(ang),present(time))
!
    call ncout%Loadheader()
    frame = ncout%dim_length('frame') + 1
!
    if(present(time))then
      if(all(shape(time)>=[nframe])) call ncout%put('time',time,from=[frame])
    endif
!
    if(present(xyz))then
      if(all(shape(xyz)>=[spatial,natm,nframe])) call ncout%put('coordinates',xyz(1:spatial,1:natm,1:nframe),from=[1,1,frame])
    endif
!
    if(present(vel))then
      if(all(shape(vel)>=[spatial,natm,nframe])) call ncout%put('velocities',vel(1:spatial,1:natm,1:nframe),from=[1,1,frame])
    endif
!
    if(present(frc))then
      if(all(shape(frc)>=[spatial,natm,nframe])) call ncout%put('forces',vel(1:spatial,1:natm,1:nframe),from=[1,1,frame])
    endif
!
    if(present(box))then
      if(all(shape(box)>=[spatial,nframe]))then
        call ncout%put('cell_lengths',box(1:cell_spatial,1:nframe),from=[1,frame])
        if(present(ang))then
          if(all(shape(ang)>=[spatial,nframe]))then
            call ncout%put('cell_angles',ang(1:cell_spatial,1:nframe),from=[1,frame])
          else
            call ncout%put('cell_angles',dummy_ang(cell_spatial,nframe),from=[1,frame])
          endif
        else
          call ncout%put('cell_angles',dummy_ang(cell_spatial,nframe),from=[1,frame])
        endif
      endif
    else
      if(present(ang))then
        call ncout%put('cell_lengths',dummy_box(cell_spatial,nframe),from=[1,frame])
        if(all(shape(ang)>=[spatial,nframe]))then
          !call ncout%put('cell_angles',ang(1:cell_spatial,1:nframe),from=[1,frame])
          call ncout%put('cell_angles',dummy_ang(cell_spatial,nframe),from=[1,frame])
        else
          call ncout%put('cell_angles',dummy_ang(cell_spatial,nframe),from=[1,frame])
        endif
      endif
    endif
    call ncout%quit()
    if(CheckAbort(ncout%iserr(),IO_NCFMTERR)) RETURN
  end subroutine ExportAmberNetcdf
!
  pure function dummy_box(spatial,nframe) result(res)
  integer,intent(in)          :: spatial,nframe
  double precision            :: res(spatial,nframe)
    res = 0.d0
  end function dummy_box
!
  pure function dummy_ang(spatial,nframe) result(res)
  integer,intent(in)          :: spatial,nframe
  double precision            :: res(spatial,nframe)
    res = 90.d0
  end function dummy_ang
end module moltypes_export
