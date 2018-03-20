module spur_string_join
  use spur_string_digit
  use spur_string_tostr
  use spur_string_padding
  implicit none
  public join_byte, join_int2, join_int4, join_chr
contains
!---------------------------------------------------------!
!       functions for Join.                               !
!---------------------------------------------------------!
  pure function join_byte(num,delimiter,digit) result(res)
  integer(1),intent(in)            :: num(:)
  character(*),intent(in),optional :: delimiter
  integer,intent(in),optional      :: digit
  character(:),allocatable         :: res
  integer                          :: i
    if(present(delimiter))then
      if(present(digit))then
        call join_fmted_delim(size(num),len(delimiter),digit,[(padding_byte(num(i),digit),i=1,size(num))],delimiter,res)
      else
        call join_delim(size(num),len(delimiter),digit_byte(num),[(tostr_byte(num(i)),i=1,size(num))],delimiter,res)
      endif
    else
      if(present(digit))then
        call join_fmted(size(num),len(delimiter),[(padding_byte(num(i),digit),i=1,size(num))],res)
      else
        call join_direct(size(num),digit_byte(num),[(tostr_byte(num(i)),i=1,size(num))],res)
      endif
    endif
  end function join_byte
!
  pure function join_int2(num,delimiter,digit) result(res)
  integer(2),intent(in)            :: num(:)
  character(*),intent(in),optional :: delimiter
  integer,intent(in),optional      :: digit
  character(:),allocatable         :: res
  integer                          :: i
    if(present(delimiter))then
      if(present(digit))then
        call join_fmted_delim(size(num),len(delimiter),digit,[(padding_int2(num(i),digit),i=1,size(num))],delimiter,res)
      else
        call join_delim(size(num),len(delimiter),digit_int2(num),[(tostr_int2(num(i)),i=1,size(num))],delimiter,res)
      endif
    else
      if(present(digit))then
        call join_fmted(size(num),len(delimiter),[(padding_int2(num(i),digit),i=1,size(num))],res)
      else
        call join_direct(size(num),digit_int2(num),[(tostr_int2(num(i)),i=1,size(num))],res)
      endif
    endif
  end function join_int2
!
  pure function join_int4(num,delimiter,digit) result(res)
  integer(4),intent(in)            :: num(:)
  character(*),intent(in),optional :: delimiter
  integer,intent(in),optional      :: digit
  character(:),allocatable         :: res
  integer                          :: i
    if(present(delimiter))then
      if(present(digit))then
        call join_fmted_delim(size(num),len(delimiter),digit,[(padding_int4(num(i),digit),i=1,size(num))],delimiter,res)
      else
        call join_delim(size(num),len(delimiter),digit_int4(num),[(tostr_int4(num(i)),i=1,size(num))],delimiter,res)
      endif
    else
      if(present(digit))then
        call join_fmted(size(num),len(delimiter),[(padding_int4(num(i),digit),i=1,size(num))],res)
      else
        call join_direct(size(num),digit_int4(num),[(tostr_int4(num(i)),i=1,size(num))],res)
      endif
    endif
  end function join_int4
!
  pure function join_chr(word,delimiter) result(res)
  character(*),intent(in)          :: word(:)
  character(*),intent(in),optional :: delimiter
  character(:),allocatable         :: res
    if(present(delimiter))then
      call join_delim(size(word),len(delimiter),digit_chr(word),word,delimiter,res)
    else
      call join_direct(size(word),digit_chr(word),word,res)
    endif
  end function join_chr
!
  pure subroutine join_direct(n,digit,word,res)
  integer,intent(in)                     :: n,digit(n)
  character(*),intent(in)                :: word(n)
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u
    allocate(character(sum(digit))::res)
    l = 0 ; u = 0
    do i=1,n
      l = u + 1 ; u = u + digit(i) ; res(l:u) = adjustl(trim(word(i)))
    enddo
  end subroutine join_direct
!
  pure subroutine join_delim(n,d,digit,word,delimiter,res)
  integer,intent(in)                     :: n,d,digit(n)
  character(*),intent(in)                :: word(n),delimiter
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u
    allocate(character(sum(digit)+(n-1)*d) :: res)
    l = 0 ; u = 0
    do i=1,n-1
      l = u + 1 ; u = u + digit(i) + d
      res(l:u) = adjustl(trim(word(i)))//delimiter
    enddo
    l = u + 1 ; u = u + digit(n)
    res(l:u) = adjustl(trim(word(n)))
  end subroutine join_delim
!
  pure subroutine join_fmted(n,t,word,res)
  integer,intent(in)                     :: n,t
  character(t),intent(in)                :: word(n)
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u
    allocate(character(n*t) :: res)
    l = 0 ; u = 0
    do i=1,n
      l = u + 1 ; u = u + t ; res(l:u) = word(i)
    enddo
  end subroutine join_fmted
!
  pure subroutine join_fmted_delim(n,d,t,word,delimiter,res)
  integer,intent(in)                     :: n,d,t
  character(t),intent(in)                :: word(n)
  character(d),intent(in)                :: delimiter
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u,c
    allocate(character(n*t+(n-1)*d) :: res)
    l = 0 ; u = 0 ; c = t + d
    do i=1,n-1
      l = u + 1 ; u = u + c ; res(l:u) = word(i)//delimiter
    enddo
    l = u + 1 ; u = u + t
    res(l:u) = word(n)
  end subroutine join_fmted_delim
end module spur_string_join
