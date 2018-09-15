module spur_string_neaten
  implicit none
  public neaten,large,upperword,small,lowerword
contains
  pure function neaten(string,let) result(res)
  character(*),intent(in)          :: string
  character(*),intent(in),optional :: let
  character(:),allocatable         :: res
    allocate(character(len_trim(adjustl(string)))::res)
    res(:) = trim(adjustl(string))
    if(present(let))then
      select case(let(1:1))
      case('u','U') ;  res(:) = large(res)
      case('l','L') ;  res(:) = small(res)
      end select
    endif
  end function neaten
!
  pure function large(string) result(res)
  character(*), intent(in) :: string
  character(:),allocatable :: res
  integer                  :: i
    allocate(character(len(string))::res)
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
  pure function small(string) result(res)
  character(*), intent(in) :: string
  character(:),allocatable :: res
  integer                  :: i
    allocate(character(len(string))::res)
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
