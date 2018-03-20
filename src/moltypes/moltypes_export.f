module moltypes_export
  use moltypes_errorhandler
  use moltypes
  implicit none
  private
  public :: ExportAs,ExportXYZ,ExportMdcrd,ExportRST7,ExportAmberNetcdf
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
!
  interface ExportAs
    module procedure  MoltypeExport_Single,MoltypeExport_Array
  end interface ExportAs
!
contains
  subroutine MoltypeExport_Single(this,path,overwrite)
  use spur_pathname
  class(moltype),intent(inout)            :: this
  character(*),intent(in)                 :: path
  logical,intent(in),optional             :: overwrite
  type(pathname)                          :: fpath
    call fpath%fetch(path)
    if(CheckAbort(fpath%isnotWritable(),IO_NOTWRITABLE,path)) RETURN
    if(present(overwrite))then
      call MoltypeExport(this,fpath,overwrite)
    else
      call MoltypeExport(this,fpath)
    endif
  end subroutine MoltypeExport_Single
!
  subroutine MoltypeExport_Array(this,path,overwrite)
  use spur_pathname
  class(moltype),intent(inout)            :: this
  character(*),intent(in)                 :: path(:)
  logical,intent(in),optional             :: overwrite
  type(pathname)                          :: fpath
  integer                                 :: i
    do i = lbound(path,1),ubound(path,1)
      call fpath%fetch(path(i))
      if(CheckAbort(fpath%isnotWritable(),IO_NOTWRITABLE,path(i))) CYCLE
      if(present(overwrite))then
        call MoltypeExport(this,fpath,overwrite)
      else
        call MoltypeExport(this,fpath)
      endif
    enddo
  end subroutine MoltypeExport_Array
!
  subroutine MoltypeExport(this,fpath,overwrite)
  use spur_pathname
  class(moltype),intent(inout)     :: this
  type(pathname) ,intent(in)       :: fpath
  logical,intent(in),optional      :: overwrite
  logical                          :: lo
    if(this%natoms()<=0) RETURN
    call RoutineNameIs('MOLTYPES_EXPORT')
    lo = .TRUE. ; if(present(overwrite)) lo = overwrite
    select case(fpath%extension())
    case('netcdf','nc')
      call ExportAmberNetcdf(fpath%is(),this%natoms(),this%nframes(),    &
     &                       xyz=this%xyz(),box=dble(this%box()),        &
     &                       ang=dble(this%boxang()),overwrite=lo)
    case('mdcrd','crd')
      call ExportMdcrd(fpath%is(),this%natoms(),this%nframes(),          &
     &                 this%xyz(),this%box(),this%boxang(),overwrite=lo)
    case('rst7','restrt')
      call ExportRST7(fpath%is(),this%natoms(),                          &
     &                this%xyz(this%nframes()),this%box(this%nframes()), &
     &                this%boxang(this%nframes()))
    case default
      call ExportXYZ(fpath%is(),this%natoms(),this%nframes(),this%xyz(), &
     &               this%inq('name','XX  '),overwrite=lo)
    end select
  end subroutine MoltypeExport
!
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
    if(CheckAbort(natm<=0,IO_NATOMERR))RETURN
    call fout%fetch(path)
    if(present(overwrite))then
      if(overwrite) call fout%generate()
    endif
    call fout%append()
 
    allocate(character(0)::lffmt,ltitle)
    if(present(ffmt))then  ; lffmt  = '(A,3'//ffmt//')'
    else ; lffmt  = '(A,3F9.3)'  ; endif
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
  subroutine ExportRST7(path,natm,xyz,box,ang,vel,time,title)
  use spur_stdio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm
  real,intent(in)                      :: xyz(spatial,natm),box(spatial)
  real,intent(in),optional             :: vel(spatial,natm),ang(spatial),time
  character(*),intent(in),optional     :: title
  character(:),allocatable             :: ltitle
  real                                 :: ltime,lang(spatial)
  type(stdio)                          :: fout
    call RoutineNameIs('EXPORT_AMBERRST7')
    if(CheckAbort(natm<=0,IO_NATOMERR))RETURN
    call fout%fetch(path) ; call fout%generate()
!
    if(present(time))then ; ltime = time
    else ; ltime = 0.0 ; endif
    if(present(ang))then  ; lang  = ang
    else ; lang = 90.0 ; endif
    allocate(character(0)::ltitle)
    if(present(title))then ; ltitle = title
    else ; ltitle = DEFAULT_NAME ; endif
!
    call fout%puts(ltitle)
    write(fout%devn(),'(I5,5E15.7)',ERR=100) natm,ltime
    write(fout%devn(),'(6F12.7)',ERR=100)   xyz
    if(present(vel)) write(fout%devn(),'(6F12.7)',ERR=100) vel
    write(fout%devn(),'(6F12.7)',ERR=100) box,lang
    call fout%quit() ; RETURN
100 if(CheckAbort(.TRUE.,IO_WRITERR)) RETURN
  end subroutine ExportRST7
!
  subroutine ExportAmberNetcdf(path,natm,nframe,xyz,box,ang,time,overwrite)
  use spur_string, only : ToStr
  use spur_ncio
  character(*),intent(in)              :: path
  integer,intent(in)                   :: natm,nframe
  real,intent(in),optional             :: xyz(spatial,natm,nframe)
  double precision,intent(in),optional :: box(spatial,nframe),ang(spatial,nframe)
  real,intent(in),optional             :: time(nframe)
  logical,intent(in),optional          :: overwrite
  type(ncio)                           :: ncout
  logical                              :: create
  integer                              :: frame
    call RoutineNameIs('EXPORT_AMBERNETCDF')
    ncout%terminates_at_abnormal = terminates_default
    if(CheckAbort(natm<=0,IO_NATOMERR,path)) RETURN
!
    call ncout%fetch(path)
!
    create = ncout%isnotExist()
    if(present(overwrite)) create = overwrite.or.create
!
    if(create)then
      call ncout%generate()
      call ncout%add_dimension('frame,spatial=3,atom='//ToStr(natm)//&
                              &',cell_spatial=3,label=5,cell_angular=3')
!
      if(present(time))then
        call ncout%add_variable('float :: time[frame]')
        call ncout%put_attribute('time :: units=picosecond')
      endif
      if(present(xyz))then
        call ncout%add_variable('char  :: spatial[spatial]')
        call ncout%add_variable('float :: coordinates [spatial,atom,frame]')
        call ncout%put_attribute('coordinates :: units=angstrom')
      endif
      if(present(box))then
        call ncout%add_variable( 'char  :: cell_spatial[cell_spatial]')
        call ncout%add_variable( 'double:: cell_lengths[cell_spatial,frame]')
        call ncout%put_attribute('cell_lengths :: units=angstrom')
      endif
      if(present(ang))then
        call ncout%add_variable( 'char  :: cell_angular[label,cell_angular]')
        call ncout%add_variable( 'double:: cell_angles [cell_angular,frame]')
        call ncout%put_attribute('cell_angles :: units=degree')
      endif
!
      call ncout%put_attribute('title=default_name')
      call ncout%put_attribute('application=AMBER')
      call ncout%put_attribute('program=moltypes')
      call ncout%put_attribute('programVersion=1.0')
      call ncout%put_attribute('Conventions=AMBER')
      call ncout%put_attribute('ConventionVersion=1.0')
!
      if(present(xyz))then
        call ncout%put('spatial',["x","y","z"],from=[1])
      endif
      if(present(box))then
        call ncout%put('cell_spatial',["a","b","c"],from=[1])
      endif
      if(present(ang))then
        call ncout%put('cell_angular',reshape(["a","l","p","h","a","b","e","t","a"," ","g","a","m","m","a"],[5,3]),from=[1,1])
      endif
      call ncout%quit()
    endif
    if(CheckAbort(ncout%iserr(),IO_NCFMTERR,path)) RETURN
!
    call ncout%Loadheader()
    frame = ncout%dim_length('frame') + 1
!
    if(present(time))then
      call ncout%put('time',time,from=[frame])
    endif
    if(present(xyz))then
      call ncout%put('coordinates',xyz(1:spatial,1:natm,1:nframe),from=[1,1,frame])
    endif
    if(present(box))then
      call ncout%put('cell_lengths',box(1:cell_spatial,1:nframe),from=[1,frame])
    endif
    if(present(ang))then
      call ncout%put('cell_angles',ang(1:cell_spatial,1:nframe),from=[1,frame])
    endif
    call ncout%quit()
    if(CheckAbort(ncout%iserr(),IO_NCFMTERR)) RETURN
  end subroutine ExportAmberNetcdf
end module moltypes_export
