module mdcrdio
implicit none
contains
  integer function ReadNatmFromNetcdf(ncid) result(res)
  use netcdf
  integer,intent(in)                :: ncid
  integer                           :: DimID1,DimID2
  integer                           :: Spatial
  logical                           :: iserr
    iserr = nf90_inq_dimid(ncid,"atom",DimID1)              /= nf90_NoErr
    iserr = nf90_inquire_dimension(ncid,DimID1,len=res)     /= nf90_NoErr .or. iserr
    iserr = nf90_inq_dimid(ncid,"spatial",DimID2)           /= nf90_NoErr .or. iserr
    iserr = nf90_inquire_dimension(ncid,DimID2,len=Spatial) /= nf90_NoErr .or. iserr
    if(iserr .or. Spatial/=3) res = -1
  end function ReadNatmFromNetcdf
!
  logical function TrjLoadMDCRD(this,fdata,lus) result(res)
  use netcdf
  class(Trajectory),intent(inout)   :: this
  class(fileio),intent(inout)       :: fdata
  integer,intent(in)                :: lus(3)
  integer                           :: NATM
  integer                           :: is,ncid
  logical                           :: iserr,nerr
    if(fdata%IsBinary())then
      iserr = nf90_open(fdata%NameIs(),nf90_nowrite,ncid)   /= nf90_NoErr
!
      NATM = ReadNatmFromNetcdf(ncid)
      nerr = TrjCheckNatm(this,NATM)
!
      if(.not. iserr.or.nerr) iserr = ReadNetcdf(ncid,this%top%NATM,this%top%mask,lus)
      iserr = nf90_close(ncid) /= nf90_NoErr .or. iserr
    else
      nerr = this%top%NATM<=0
      if(.not.nerr)iserr = ReadFromMdcrd(this%top%NATM,this%top%mask,lus)
    endif
    call this%top%ERR%check(nerr, "NATM is not match   "//fdata%NameIs())
    call this%top%ERR%check(iserr,"File open failed. "//fdata%NameIs())
    res = iserr.or.nerr ; RETURN
  contains
    logical function ReadNetcdf(ncid,NATM,mask,lus) result(res)
    integer,intent(in)           :: ncid,NATM
    logical,intent(in)           :: mask(NATM)
    integer,intent(in)           :: lus(3)
    integer                      :: frame
    integer                      :: from,to,DimID,VarID
      res = nf90_inq_dimid(ncid,"frame",DimID)      /= nf90_NoErr .or. res
      res = nf90_inquire_dimension(ncid,DimID,len=frame)   /= nf90_NoErr .or. res
      if(res)RETURN
!
      frame = minval([frame,1+int((lus(2)-lus(1))/lus(3))],1)
!
      from = this%NTRJ + 1
      to = this%NTRJ + frame
      this%NTRJ = to
      call TrjExpand(this)
!
      res = ReadCoordinatesFromNetcdf(ncid,NATM,mask,lus(1),lus(3),from,to,frame) .or. res
      res = ReadBoxsizeFromNetcdf(ncid,lus(1),lus(3),from,to)                     .or. res
      RETURN
    end function ReadNetcdf
!
    logical function ReadCoordinatesFromNetcdf(ncid,NATM,mask,lb,st,from,to,frame) result(res)
    integer,intent(in)           :: ncid,natm,lb,st,from,to,frame
    logical,intent(in)           :: mask(natm)
    integer                      :: Head(1:3), Tail(1:3)
    integer                      :: VarID
    double precision             :: CRD(3,NATM,1)
    integer                      :: i,j,k
      res = nf90_inq_varid(ncid,"coordinates",VarID) /= nf90_NoErr
      if(res)RETURN
      Head=[1,1,lb] ; Tail=[3,natm,1]
!
      do k = from,to
        res = nf90_get_var(ncid,VarID,CRD,start=Head,count=Tail) /= nf90_NoErr .or. res
        if(res)EXIT
!
        j = 0
        do i=1,NATM
          if(mask(i))then
            j = j + 1 ; this%xyz(:,j,k) = CRD(:,i,1)
          endif
        enddo
        Head(3) = Head(3) + st
      enddo
    end function ReadCoordinatesFromNetcdf
!
    logical function ReadBoxsizeFromNetcdf(ncid,lb,st,from,to) result(res)
    integer,intent(in)           :: ncid,lb,st,from,to
    integer                      :: Head(1:2), Tail(1:2)
    integer                      :: i,VarID
    logical                      :: hasbox
      hasbox = nf90_inq_varid(ncid,"cell_lengths",VarID) == nf90_NoErr
      if(hasbox)then
        Head=[1,lb] ; Tail=[3,1]
        do i = from,to
          res = nf90_get_var(ncid,VarID,this%Box(:,i),start=Head,count=Tail) /= nf90_NoErr .or. res
          if(res)RETURN
          Head(2) = Head(2) + st
        enddo
      else
        this%box(:,from:to) = 0.d0
        res = .FALSE.
      endif
    end function ReadBoxsizeFromNetcdf
!
    logical function ReadFromMdcrd(NATM,mask,lus) result (res)
    integer,intent(in)           :: NATM,lus(3)
    logical,intent(in)           :: mask(NATM)
    double precision             :: C(3*NATM), CRD(3,NATM), BOX(3)
    integer                      :: is,dev,frame,co
    integer                      :: i,j,k
    type(vector_character)       :: spl
      call fdata%fetch()
      res = .not.fdata%isExist() ; if(res)RETURN
      dev = fdata%DevN()
      read(dev,*,iostat=is) ; res = is/=0 ; if(res)RETURN
!
      frame = 1+int((lus(2)-lus(1))/lus(3))
      co = this%NTRJ + 1
      this%NTRJ = this%NTRJ + frame
      call TrjExpand(this)
!
    do k=1,lus(1)-1
      read(dev,'(10F8.3)',iostat=is)CRD
      read(dev,'(3F8.3)',iostat=is)BOX
      res = is>0 ; if(res.or.is<0)RETURN
    enddo
!
    do k=1,lus(2)-lus(1)+1
      read(dev,'(10F8.3)',iostat=is)C
      read(dev,'(3F8.3)',iostat=is)BOX
      res = is>0
      if(res.or.is<0)then
        this%NTRJ = co ; EXIT
      endif
      if(mod(k,lus(3))/=0)CYCLE
      co = co + 1
      CRD = reshape(C,[3,NATM])
!
      j = 0
      do i=1,NATM
        if(mask(i))then
          j = j + 1 ; this%xyz(:,j,co) = CRD(:,i)
        endif
      enddo
      this%Box(:,this%NTRJ) = BOX
    enddo
    call fdata%quit()
    RETURN
  end function ReadFromMdcrd
end function TrjLoadMDCRD
!
logical function TrjExportMDCRD(this,fdata,fmode) result(res)
class(trajectory),intent(inout)   :: this
type(fileio),intent(inout)        :: fdata
integer,intent(in)                :: fmode
real                              :: Box(3)
integer                           :: i,is,Dev
logical                           :: append
  append = fdata%isExist()
  res = this%NTRJ<=0 ;  if(res)RETURN
!
  if(fmode==0)then
    call fdata%generate(fdata%nameis(),STAT=is)
  elseif(fmode==1)then
    call fdata%fetch(fdata%nameis(),APPEND=.TRUE.,STAT=is)
  endif
!
  Dev = fdata%DevN()
  if(fmode==0.or..not.append) write(Dev,'(A)',IOSTAT=is)"default_name"
  do i=1,this%NTRJ
    write(dev,'(10F8.3)',iostat=is) this%xyz(:,:,i)
    write(dev,'(3F8.3)',iostat=is)this%Box(:,i)
  enddo
  res = fdata%isAbnormal().or.is/=0
  RETURN
end function TrjExportMDCRD
!
logical function TrjExportNetcdf(this,fdata,fmode) result(res)
use netcdf
class(trajectory),intent(inout)   :: this
type(fileio),intent(inout)        :: fdata
integer,intent(in)                :: fmode
integer                           :: ncid,id
integer                           :: i,is,frame
logical                           :: iserr,nerr
  if(this%NTRJ<=0)RETURN
  if(fmode==0.or..not.fdata%isExist())iserr = createMdcrdNetcdf(fdata,this%NATM)
  iserr = nf90_open(fdata%NameIs(),nf90_write,ncid)                  /= nf90_NoErr.or.iserr
!
  nerr = ReadNatmFromNetcdf(ncid) /= this%NATM
  call this%top%ERR%check(iserr,"File open failed. "//fdata%NameIs())
  call this%top%ERR%check(nerr,"NATM is not match. "//fdata%NameIs())
  res = iserr.or.nerr ; if(res)RETURN
!
  res = nf90_inq_dimid(ncid,"frame",ID)                              /= nf90_NoErr.or.res
  res = nf90_inquire_dimension(ncid,ID,len=frame)                    /= nf90_NoErr.or.res
!
  res = nf90_inq_varid(ncid,"time",ID)                               /= nf90_NoErr.or.res
  res = nf90_put_var(ncid,ID,[(real(i),i=frame+1,frame+this%NTRJ)],start=[frame+1],&
                     &count=[this%NTRJ])                             /= nf90_NoErr.or.res
  res = nf90_inq_varid(ncid,"coordinates",ID)                        /= nf90_NoErr.or.res
  res = nf90_put_var(ncid,ID,this%xyz(1:3,1:this%NATM,1:this%NTRJ),start=[1,1,frame+1],&
                     &count=[3,this%NATM,this%NTRJ])                 /= nf90_NoErr.or.res
  res = nf90_inq_varid(ncid,"cell_lengths",ID)                       /= nf90_NoErr.or.res
  res = nf90_put_var(ncid,id,this%Box(1:3,1:this%NTRJ),start=[1,frame+1],&
                     &count=[3,this%NTRJ])                           /= nf90_NoErr.or.res
  res = nf90_inq_varid(ncid,"cell_angles",ID)                        /= nf90_NoErr.or.res
  res = nf90_put_var(ncid,id,reshape([(90.d0,i=1,3*this%NTRJ)],[3,this%NTRJ]),&
                    &start=[1,frame+1],count=[3,this%NTRJ])          /= nf90_NoErr.or.res
  res=nf90_close(ncid)                                               /= nf90_NoErr.or.res
  RETURN
contains
  logical function createMdcrdNetcdf(fdata,natm) result(res)
  type(fileio),intent(inout)  :: fdata
  integer,intent(in)          :: natm
  integer                     :: ncid,atmdid,csdid,label,cadid,sdid,fdid
  integer                     :: time,sptvid,crdvid,cellvid,cspat,cangu,clanvid
    res = nf90_create(fdata%NameIs(),nf90_clobber,ncid)                         /=nf90_NoErr
!
    res = nf90_def_dim(ncid,"frame",nf90_unlimited,fdid)                        /=nf90_NoErr.or.res
    res = nf90_def_dim(ncid,"spatial",3,sdid)                                   /=nf90_NoErr.or.res
    res = nf90_def_dim(ncid,"atom",natm,atmdid)                                 /=nf90_NoErr.or.res
    res = nf90_def_dim(ncid,"cell_spatial",3,csdid)                             /=nf90_NoErr.or.res
    res = nf90_def_dim(ncid,"label",5,label)                                    /=nf90_NoErr.or.res
    res = nf90_def_dim(ncid,"cell_angular",3,cadid)                             /=nf90_NoErr.or.res
!
    res = nf90_def_var(ncid,"time",nf90_float,[fdid],time)                      /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"spatial",nf90_char,[sdid],sptvid)                  /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"coordinates",nf90_float,[sdid,atmdid,fdid],crdvid) /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"cell_spatial",nf90_char,[csdid],cspat)             /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"cell_angular",nf90_char,[label,cadid],cangu)       /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"cell_lengths",nf90_double,[csdid,fdid],cellvid)    /=nf90_NoErr.or.res
    res = nf90_def_var(ncid,"cell_angles",nf90_double,[cadid,fdid],clanvid)     /=nf90_NoErr.or.res
!
    res = nf90_put_att(ncid,time,"units","picosecond")                          /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,crdvid,"units","angstrom")                          /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,cellvid,"units","angstrom")                         /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,clanvid,"units","degree")                           /=nf90_NoErr.or.res
!
    res = nf90_put_att(ncid,nf90_global,"title","default_name")                 /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,nf90_global,"application","AMBER")                  /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,nf90_global,"Conventions","AMBER")                  /=nf90_NoErr.or.res
    res = nf90_put_att(ncid,nf90_global,"ConventionVersion","1.0")             /=nf90_NoErr.or.res
!
    res = nf90_enddef(ncid)                                                     /=nf90_NoErr.or.res
!
    res = nf90_put_var(ncid,cspat,["a","b","c"],start=[1],count=[3])            /=nf90_NoErr.or.res
    res = nf90_put_var(ncid,cangu,reshape(["a","l","p","h","a","b","e","t","a"," ",&
                      &"g","a","m","m","a"],[5,3]),start=[1,1],count=[5,3])     /=nf90_NoErr.or.res
    res = nf90_put_var(ncid,sptvid,["x","y","z"],start=[1],count=[3])           /=nf90_NoErr.or.res
!
    res=nf90_close(ncid)                                                        /=nf90_NoErr.or.res
  end function createMdcrdNetcdf
end function TrjExportNetcdf
end module mdcrdio
