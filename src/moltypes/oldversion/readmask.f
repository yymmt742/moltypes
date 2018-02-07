!-----------------------------------------------------------------------------
! Subroutine ReadMask(MaskString)
!    use atomdata
!    Usage :: char*  MaskString
!-----------------------------------------------------------------------------
pure function ReadMask(this,MaskString) result(res)
class(molinfo),intent(in)      :: this
character(*),intent(in)        :: MaskString
logical                        :: res(count(this%mask)), tmp(count(this%mask))
type(vector_character)         :: words, keyword
type(vector_integer)           :: head,tail
logical                        :: A,O,N,D
integer                        :: i,j,NMASK
character(7)                   :: keywords(12)
  res = .TRUE.
  NMASK = count(this%mask)
  call words%Split(MaskString,[" ",","],pickup=["-"])
  if(this%NATM<=0.or.words%empty())RETURN
!
  keywords = ["all    ","none   ","and    ","or     ",&
           & "not     ","nor    ","nand   ","name   ",&
           & "type    ","residue","resname","index  "&
           & ]
!
  do i=1,words%size()
    if(any(words%small(i)==keywords))then
      call keyword%push(words%small(i)) ; call head%push(i)
    endif
  enddo
!
  call tail%reserve(keyword%size())
  do i=2,keyword%size()
    call tail%push(head%at(i)-1)
  enddo
  call tail%push(words%size())
!
  A = .FALSE. ; O = .FALSE.
  N = .FALSE. ; D = .FALSE.
!
  Tmp = .FALSE.
!
  do i=1,keyword%size()
    select case(keyword%at(i))
   !Keywords that does not requir argument
    case("all","none","and","or","not","nand","nor")
      if(head%at(i)/=tail%at(i))then ; res = .FALSE. ; RETURN ; endif
      select case(keyword%at(i))
      case("all")     ; Tmp = .TRUE.
      case("none")
      case("not")     ; N = .TRUE. ; CYCLE
      case("and","or","nand","nor")
        if(i==1.or.i==keyword%size().or.A.or.O)then
          res = .TRUE. ; RETURN
        endif
        A = keyword%at(i) == "and".or.keyword%at(i) == "nand"
        O = keyword%at(i) == "or" .or.keyword%at(i) == "nor"
        if(keyword%at(i) == "nand".or.keyword%at(i) == "nor") N = not(N)
        CYCLE
      end select
   !Keywords that require arguments
    case("index")  ; tmp = mask_index(head%at(i),tail%at(i),this%NATM,this%NATM,NMASK,[(j,j=1,this%NATM)])
    case("residue"); tmp = mask_index(head%at(i),tail%at(i),this%NATM,this%NRES,NMASK,this%residue%lookup())
    case("name")   ; tmp = mask_chr(head%at(i),tail%at(i),this%NATM,this%NATM,NMASK,this%AtmName%lookup(),this%UniqAtm)
    case("resname"); tmp = mask_chr(head%at(i),tail%at(i),this%NATM,this%UniqRes%size(),NMASK,this%ResName%lookup(),this%UniqRes)
    case("type")   ; tmp = mask_chr(head%at(i),tail%at(i),this%NATM,this%NATM,NMASK,this%AtmType%lookup(),this%UniqTyp)
    end select
!
    if(N)then ; Tmp = .not.tmp ; N = .FALSE. ; endif
!
    if(A)then     ; res = IAND(res,Tmp) ; A = .FALSE. ; CYCLE
    elseif(O)then ; res = IOR(res,Tmp)  ; O = .FALSE. ; CYCLE 
    endif
    if(D)then ; res = .FALSE. ; RETURN ; endif
    res = Tmp ; D = .TRUE. ; CYCLE
  enddo
  RETURN
contains
  pure function mask_index(head,tail,NATM,NMAX,NMASK,Ilist) result(res)
  integer,intent(in)         :: head, tail
  integer,intent(in)         :: NATM,NMAX,NMASK,Ilist(NMASK)
  logical                    :: res(NMASK)
  type(vector_integer)       :: list
  character(:),allocatable   :: str
  integer                    :: i,j,Nold,N
  logical                    :: to
    res = .FALSE.
    to = .FALSE. ; Nold = 1 ; N = 1 ; allocate(character(0)::str)
    do i=head+1,tail
      if(any(words%small(i)==["to","- "]))then
        if(to) RETURN ; Nold = N
        to = .TRUE. ; if(i<tail)CYCLE ; N = NMAX
      else
        str = words%at(i) ; read(str,*,err=100)N
        if(N>NMAX)then   ; N = NMAX
        elseif(N==0)then ; N = 1
        elseif(N<0)then  ; N = modulo(N,maxval([NMAX,1],1)) + 1
        endif
      endif
!
      if(to.and.Nold>N) RETURN ; if(.not.to)Nold = N
      to=.FALSE. ; call list%push([(j,j=Nold,N)])
    enddo
!
    do i=1,list%size()
      res = IOR(res,list%at(i).eq.Ilist)
    enddo
100 continue
    RETURN
  end function mask_index
!
  pure function mask_chr(head,tail,NATM,NMAX,NMASK,Clist,Ulist) result(res)
  integer,intent(in)                :: head,tail,NATM,NMAX,NMASK
  character(*),intent(in)           :: Clist(NMASK)
  type(vector_character),intent(in) :: Ulist
  logical                           :: res(NMASK)
  type(vector_integer)              :: list
  integer                           :: i,j,N,Nold
  logical                           :: to, F
    res = .FALSE.
    to = .FALSE.
    do i=head+1,tail
      if(any(words%small(i)==["to","- "]))then
        if(to) RETURN ; Nold = N
        to = .TRUE. ; if(i<tail)CYCLE ; N = NMAX
      else
        N = Ulist%find(words%at(i)) ; if(N==0) RETURN
      endif
!
      if(to.and.Nold>N) RETURN ; if(.not.to)Nold = N
      to=.FALSE. ; call list%push([(j,j=Nold,N)])
    enddo
!
    do i=1,list%size()
      res = IOR(res,Ulist%at(list%at(i)).eq.Clist)
    enddo
    RETURN
  end function mask_chr
end function ReadMask
