module spur_string_tonum
  implicit none
  public tonum_byte,tonum_int2,tonum_int4,tonum_real,tonum_dble
contains
  pure elemental function tonum_byte(string,dumm) result(res)
  character(*), intent(in) :: string
  integer(1),intent(in)    :: dumm
  integer(1)               :: res
    res = tonum_read(string,int(dumm))
  end function tonum_byte
!
  pure elemental function tonum_int2(string,dumm) result(res)
  character(*), intent(in) :: string
  integer(2),intent(in)    :: dumm
  integer(2)               :: res
    res = tonum_read(string,int(dumm))
  end function tonum_int2
!
  pure elemental integer function tonum_int4(string,dumm) result(res)
  character(*), intent(in) :: string
  integer,intent(in)       :: dumm
    res = tonum_read(string,dumm)
  end function tonum_int4
!
  pure elemental function tonum_real(string,dumm) result(res)
  character(*), intent(in) :: string
  real,intent(in)          :: dumm
  real                     :: res
    res = real(tonum_float(string,dble(dumm)))
  end function tonum_real
!
  pure elemental function tonum_dble(string,dumm) result(res)
  character(*), intent(in)     :: string
  double precision,intent(in)  :: dumm
  double precision             :: res
    res = tonum_float(string,dumm)
  end function tonum_dble
!
  pure elemental function tonum_read(string,dumm) result(res)
  character(*), intent(in)    :: string
  integer,intent(in)          :: dumm
  integer                     :: tmp,res
    res = dumm ; read(string,*,err=100) tmp ; res = tmp
100 return
  end function tonum_read
!
  pure elemental function tonum_float(string,dumm) result(res)
  character(*), intent(in)    :: string
  double precision,intent(in) :: dumm
  double precision            :: tmp,res
    res = dumm ; read(string,'(f)',err=100) tmp ; res = tmp
100 return
  end function tonum_float
end module spur_string_tonum
