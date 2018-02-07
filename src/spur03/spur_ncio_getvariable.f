  integer function NcGetVid(this,var) result(res)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
    res = 0
    if(CheckAbort(this,NcReadOpen(this),ierr=IO_OPENERR,filename=this%is())) RETURN
    res = this%var%find(var)
  end function NcGetVid
!
  subroutine NcGetVariable_byte_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_1d
!
  subroutine NcGetVariable_byte_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_2d
!
  subroutine NcGetVariable_byte_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_3d
!
  subroutine NcGetVariable_byte_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_4d
!
  subroutine NcGetVariable_byte_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_5d
!
  subroutine NcGetVariable_byte_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(1),intent(out)           :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_byte_6d
!
  subroutine NcGetVariable_int2_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_1d
!
  subroutine NcGetVariable_int2_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_2d
!
  subroutine NcGetVariable_int2_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_3d
!
  subroutine NcGetVariable_int2_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_4d
!
  subroutine NcGetVariable_int2_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_5d
!
  subroutine NcGetVariable_int2_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer(2),intent(out)           :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int2_6d
!
  subroutine NcGetVariable_int4_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_1d
!
  subroutine NcGetVariable_int4_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_2d
!
  subroutine NcGetVariable_int4_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_3d
!
  subroutine NcGetVariable_int4_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_4d
!
  subroutine NcGetVariable_int4_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_5d
!
  subroutine NcGetVariable_int4_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  integer,intent(out)              :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_int4_6d
!
  subroutine NcGetVariable_char_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_1d
!
  subroutine NcGetVariable_char_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_2d
!
  subroutine NcGetVariable_char_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_3d
!
  subroutine NcGetVariable_char_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_4d
!
  subroutine NcGetVariable_char_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_5d
!
  subroutine NcGetVariable_char_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  character(*),intent(out)         :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_char_6d
!
  subroutine NcGetVariable_real_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_1d
!
  subroutine NcGetVariable_real_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_2d
!
  subroutine NcGetVariable_real_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_3d
!
  subroutine NcGetVariable_real_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_4d
!
  subroutine NcGetVariable_real_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_5d
!
  subroutine NcGetVariable_real_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  real,intent(out)                 :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_real_6d
!
  subroutine NcGetVariable_dble_1d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:)
  integer,intent(in)               :: from(1)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_1d
!
  subroutine NcGetVariable_dble_2d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:,:)
  integer,intent(in)               :: from(2)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_2d
!
  subroutine NcGetVariable_dble_3d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:,:,:)
  integer,intent(in)               :: from(3)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_3d
!
  subroutine NcGetVariable_dble_4d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:,:,:,:)
  integer,intent(in)               :: from(4)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_4d
!
  subroutine NcGetVariable_dble_5d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:,:,:,:,:)
  integer,intent(in)               :: from(5)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_5d
!
  subroutine NcGetVariable_dble_6d(this,var,val,from)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: var
  double precision,intent(out)     :: val(:,:,:,:,:,:)
  integer,intent(in)               :: from(6)
  integer                          :: vid
    vid = NcGetVid(this,var) ; if(vid==0)RETURN
    this%io = nf90_get_var(this%ncid,vid,val,start=from,count=shape(val))
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN ; call this%quit()
  end subroutine NcGetVariable_dble_6d
