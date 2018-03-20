module spur_string_padding
  use spur_string_digit
  implicit none
  public  padding_byte, padding_int2, padding_int4, padding_chr
!
  character(1),parameter :: PAD_SPACE = ' '
!
contains
  pure elemental function padding_byte(num,digit,fill) result(res)
  integer(1),intent(in)                        :: num
  integer,intent(in)                           :: digit
  character(*),intent(in),optional             :: fill
  character(maxval([digit_byte(num),digit],1)) :: res
  integer                                      :: b
    b = maxval([0,digit-digit_byte(num)],1)
    if(present(fill))then
      call fill_string(fill,res(:b))
    else
      call fill_string(PAD_SPACE,res(:b))
    endif
    write(res(b+1:),'(i0)') num
  end function padding_byte
!
  pure elemental function padding_int2(num,digit,fill) result(res)
  integer(2),intent(in)                        :: num
  integer,intent(in)                           :: digit
  character(*),intent(in),optional             :: fill
  character(maxval([digit_int2(num),digit],1)) :: res
  integer                                      :: b
    b = maxval([0,digit-digit_int2(num)],1)
    if(present(fill))then
      call fill_string(fill,res(:b))
    else
      call fill_string(PAD_SPACE,res(:b))
    endif
    write(res(b+1:),'(i0)') num
  end function padding_int2
!
  pure elemental function padding_int4(num,digit,fill) result(res)
  integer(4),intent(in)                        :: num
  integer,intent(in)                           :: digit
  character(*),intent(in),optional             :: fill
  character(maxval([digit_int4(num),digit],1)) :: res
  integer                                      :: b
    b = maxval([0,digit-digit_int4(num)],1)
    if(present(fill))then
      call fill_string(fill,res(:b))
    else
      call fill_string(PAD_SPACE,res(:b))
    endif
    write(res(b+1:),'(i0)') num
  end function padding_int4
!
  pure elemental function padding_chr(word,digit,fill) result(res)
  character(*),intent(in)                      :: word
  integer,intent(in)                           :: digit
  character(*),intent(in),optional             :: fill
  character(maxval([digit_chr(word),digit],1)) :: res
  integer                                      :: b
    b = maxval([0,digit-digit_chr(word)],1)
    if(present(fill))then
      call fill_string(fill,res(:b))
    else
      call fill_string(PAD_SPACE,res(:b))
    endif
    res(b+1:) = adjustl(trim(word))
  end function padding_chr
!
  pure elemental subroutine fill_string(fill,res)
  character(*),intent(in)       :: fill
  character(*),intent(inout)    :: res
  integer                       :: d,n,lb,ub
    d = len(fill) ; if(d<1) RETURN
    n = len(res)  ; if(n<1) RETURN
    lb = 0 ; ub = 0
    do
      lb = ub + 1 ; ub = minval([lb+d-1,n],1)
      res(lb:ub) = fill
      if(ub==n) RETURN
    enddo
  end subroutine fill_string
end module spur_string_padding
