module spur_string_digit
  implicit none
  public digit_byte,digit_int2,digit_int4,digit_int8,digit_chr
contains
  pure elemental integer function digit_byte(num) result(res)
  integer(1),intent(in)       :: num
    if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function digit_byte
!
  pure elemental integer function digit_int2(num) result(res)
  integer(2),intent(in)        :: num
    if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function digit_int2
!
  pure elemental integer function digit_int4(num) result(res)
  integer(4),intent(in)        :: num
   if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function digit_int4
!
  pure elemental integer function digit_int8(num) result(res)
  integer(8),intent(in)        :: num
   if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function digit_int8
!
  pure elemental integer function digit_chr(String) result(res)
  character(*),intent(in) :: string
    res = len_trim(adjustl(string))
  end function digit_chr
end module spur_string_digit
