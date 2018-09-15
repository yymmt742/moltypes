module spur_string
  use spur_string_neaten
  use spur_string_digit
  use spur_string_tostr
  use spur_string_tonum
  use spur_string_padding
  use spur_string_join
  implicit none
  private
  public  :: neaten,large,small,digit,tostr,tonum,padding,join
!
  interface digit
    module procedure  digit_byte,digit_int2,digit_int4,digit_int8,digit_chr
  end interface digit
!
  interface tostr
    module procedure  tostr_byte, tostr_int2, tostr_int4
  end interface tostr
!
  interface tonum
    module procedure  tonum_byte, tonum_int2, tonum_int4, &
                   &  tonum_real, tonum_dble
  end interface tonum
!
  interface padding
    module procedure  padding_byte, padding_int2, padding_int4, padding_chr
  end interface padding
!
  interface join
    module procedure  join_byte, join_int2, join_int4, join_chr
  end interface join
end module spur_string
