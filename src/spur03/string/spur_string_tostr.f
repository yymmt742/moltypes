module spur_string_tostr
  use spur_string_digit
  implicit none
  public tostr_byte,tostr_int2,tostr_int4
contains
  pure function tostr_byte(num) result(res)
  integer(1),intent(in)    :: num
  character(:),allocatable :: res
    allocate(character(digit_byte(num)) :: res) ; write(res,'(i0)') num
  end function tostr_byte
!
  pure function tostr_int2(num) result(res)
  integer(2),intent(in)   :: num
  character(:),allocatable :: res
    allocate(character(digit_int2(num)) :: res) ; write(res,'(i0)') num
  end function tostr_int2
!
  pure function tostr_int4(num) result(res)
  integer,intent(in)       :: num
  character(:),allocatable :: res
    allocate(character(digit_int4(num)) :: res) ; write(res,'(i0)') num
  end function tostr_int4
end module spur_string_tostr
