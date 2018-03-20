module spur_string_neaten
  implicit none
  public large,upperword,small,lowerword
contains
  pure function large(string) result(res)
  character(*), intent(in) :: string
  character(len(string))   :: res
  integer                  :: i
    do i=1,len(string)
      res(i:i) = UpperWord(string(i:i))
    enddo
  end function large
!
  pure elemental function UpperWord(word) result(res)
  character(1),intent(in) :: word
  character(1)            :: res
    if(word>='a'.and.word<='z')then
      res = char(ichar(word)-32)
    else
      res = word
    endif
  end function UpperWord
!
  pure function Small(String) result(res)
  character(*), intent(in) :: String
  character(len(string))   :: res
  integer                  :: i
    do i=1,len(string)
      res(i:i) = LowerWord(string(i:i))
    enddo
  end function Small
!
  pure elemental function LowerWord(word) result(res)
  character(1),intent(in) :: word
  character(1)            :: res
    if(word>='A'.and.word<='Z')then
      res = char(ichar(word)+32)
    else
      res = word
    endif
  end function LowerWord
end module spur_string_neaten
