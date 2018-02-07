module spur_string
  implicit none
  private
  public :: ToStr,ToNum,Digit,Str_pad,Large,Small,Split,Join,TextWrap
!
  character(1),parameter :: pad_zero = '0'
!
  interface ToStr
    module procedure  ToStr_BYTE, ToStr_INT2, ToStr_INT4
  end interface ToStr
!
  interface ToNum
    module procedure   ToInt1,ToInt2,ToInt4,ToReal,ToDble
  end interface ToNum
!
  interface Digit
    module procedure  dig_byte, dig_int2, dig_int4, dig_chr
  end interface Digit
!
  interface Str_pad
    module procedure  str_pad_int
  end interface Str_pad
!
  interface Join
    module procedure  IJoin,CJoin
  end interface Join
!
  interface Split
    module procedure  StrSplit, StrSplit_Arr
  end interface Split
!
contains
  pure function FirstWord(string) result(res)
  character(*),intent(in)  :: string
  character(:),allocatable :: res
  integer                  :: at(2)
    allocate(character(0)::res)
    at(1) = index(string,' ') ; if(at(1)==0) at(1) = len_trim(string)
    at(2) = index(string,',') ; if(at(2)==0) at(2) = len_trim(string)
    res = adjustl(string(:minval(at,1)))
  end function FirstWord
!
  pure function ToStr_BYTE(num) result(res)
  integer(1),intent(in)    :: num
  character(:),allocatable :: res
    allocate(character(dig_byte(num)) :: res) ; write(res,'(i0)') num
  end function ToStr_BYTE
!
  pure function ToStr_Int2(num) result(res)
  integer(2),intent(in)   :: num
  character(:),allocatable :: res
    allocate(character(dig_int2(num)) :: res) ; write(res,'(i0)') num
  end function ToStr_Int2
!
  pure function ToStr_Int4(num) result(res)
  integer,intent(in)       :: num
  character(:),allocatable :: res
    allocate(character(dig_int4(num)) :: res) ; write(res,'(i0)') num
  end function ToStr_Int4
!
  pure elemental function ToInt1(string,dumm) result(res)
  character(*), intent(in) :: string
  integer(1),intent(in)    :: dumm
  integer(1)               :: res
  integer                  :: tmp
    tmp = dumm ; res = ToInt_Read(FirstWord(string),tmp)
  end function ToInt1
!
  pure elemental function ToInt2(string,dumm) result(res)
  character(*), intent(in) :: string
  integer(2),intent(in)    :: dumm
  integer(2)               :: res
  integer                  :: tmp
    tmp = dumm ; res = ToInt_Read(FirstWord(string),tmp)
  end function ToInt2
!
  pure elemental integer function ToInt4(string,dumm) result(res)
  character(*), intent(in) :: string
  integer,intent(in)       :: dumm
    res = ToInt_Read(FirstWord(string),dumm)
  end function ToInt4
!
  pure elemental function ToInt_Read(string,dumm) result(res)
  character(*), intent(in) :: string
  integer,intent(in)       :: dumm
  integer                  :: tmp,res
    res = dumm
    read(string,'(I)',err=100) tmp ; res = tmp
100 RETURN
  end function ToInt_Read
!
  pure elemental function ToReal(string,dumm) result(res)
  character(*), intent(in) :: string
  real,intent(in)          :: dumm
  real                     :: res
  double precision         :: tmp
    tmp = dumm ; res = ToReal_Read(FirstWord(string),tmp)
  end function ToReal
!
  pure elemental function ToDble(string,dumm) result(res)
  character(*), intent(in)    :: string
  double precision,intent(in) :: dumm
  double precision            :: res
    res = ToReal_Read(FirstWord(string),dumm)
  end function ToDble
!
  pure elemental function ToReal_Read(string,dumm) result(res)
  character(*), intent(in)    :: string
  double precision,intent(in) :: dumm
  double precision            :: tmp,res
    res = dumm ; read(string,'(F)',err=100) tmp ; res = tmp
100 RETURN
  end function ToReal_Read
!
  pure elemental function str_pad_int(num,dig,fill) result(res)
  integer,intent(in)                       :: num,dig
  character(*),intent(in),optional         :: fill
  character(dig)                           :: res
  integer                                  :: i,j,lb,ub
    if(num<0)then
      res(1:1) = '-' ; lb = 2
    else
      lb = 1
    endif
    ub = lb + len(res) - dig_int4(num) - 1
    if(present(fill))then
      i = lb
      do
        do j=1,len(fill)
          res(i:i) = fill(j:j)
          if(i==ub)EXIT
          i = i + 1
        enddo
        if(i==ub)EXIT
      enddo
    else
      do i = lb,ub
        res(i:i) = PAD_ZERO
      enddo
    endif
    lb = ub + 1 ; ub = len(res)
    write(res(lb:ub),'(i0)') abs(num)
  end function str_pad_int
!
  pure elemental integer function dig_byte(num) result(res)
  integer(1),intent(in)       :: num
    if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function dig_byte
!
  pure elemental integer function dig_int2(num) result(res)
  integer(2),intent(in)        :: num
    if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function dig_int2
!
  pure elemental integer function dig_int4(num) result(res)
  integer,intent(in)          :: num
   if(num<0)then
     res = int(log10(real(abs(num)))) + 2
   elseif(num>0)then
     res = int(log10(real(abs(num)))) + 1
   else
     res = 1
   endif
  end function dig_int4
!
  pure elemental integer function dig_chr(String) result(res)
  character(*),intent(in) :: string
    res = len_trim(adjustl(string))
  end function dig_chr
!
!---------------------------------------------------------!
!       functions for Large,Small                         !
!---------------------------------------------------------!
!
  pure function Large(string) result(res)
  character(*), intent(in) :: string
  character(:),allocatable :: res
  integer                  :: i
    allocate(character(len_trim(string))::res)
    do i=1,len_trim(string)
      res(i:i) = UpperWord(string(i:i))
    enddo
  end function Large
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
  character(:),allocatable :: res
  integer                  :: i
    allocate(character(len_trim(string))::res)
    do i=1,len_trim(string)
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
!
!---------------------------------------------------------!
!       functions for Join.                               !
!---------------------------------------------------------!
  pure function Ijoin(num,delim,dig) result(res)
  integer,intent(in)               :: num(:)
  character(*),intent(in),optional :: delim
  integer,intent(in),optional      :: dig
  character(:),allocatable         :: res,del
  integer                          :: i,ldig
    allocate(character(0)::del) ; if(present(delim)) del = delim
    if(present(dig))then
      call join_cd(size(num),len(del),dig,[(str_pad(num(i),dig),i=1,size(num))],del,res)
    else
      call join_c(size(num),len(del),digit(num),[(ToStr(num(i)),i=1,size(num))],del,res)
    endif
    deallocate(del)
  end function IJoin
!
  pure function CJoin(word,delim,dig) result(res)
  character(*),intent(in)          :: word(:)
  character(*),intent(in),optional :: delim
  integer,intent(in),optional      :: dig
  character(:),allocatable         :: res,del
    allocate(character(0)::del) ; if(present(delim)) del = delim
    if(present(dig))then
      call join_cd(size(word),len(del),dig,word,del,res)
    else
      call join_c(size(word),len(del),digit(word),word,del,res)
    endif
    deallocate(del)
  end function CJoin
!
  pure subroutine join_cd(n,ldel,dig,word,delim,res)
  integer,intent(in)                     :: n,ldel,dig
  character(*),intent(in)                :: word(n)
  character(*),intent(in)                :: delim
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u
    allocate(character(n*dig+(n-1)*ldel)::res)
    l = 0 ; u = 0
    do i=1,n
      l = u + 1 ; u = u + dig   ; res(l:u) = word(i)
      if(i==n) RETURN ; if(ldel<=0) CYCLE
      l = u + 1 ; u = u + ldel  ; res(l:u) = delim
    enddo
  end subroutine join_cd
!
  pure subroutine join_c(n,ldel,dig,word,delim,res)
  integer,intent(in)                     :: n,ldel,dig(n)
  character(*),intent(in)                :: word(n)
  character(*),intent(in)                :: delim
  character(:),allocatable,intent(inout) :: res
  integer                                :: i,l,u
    allocate(character(sum(dig)+(n-1)*ldel)::res)
    l = 0 ; u = 0
    do i=1,n
      l = u + 1 ; u = u + dig(i) ; res(l:u) = word(i)
      if(i==n) RETURN ; if(ldel<=0) CYCLE
      l = u + 1 ; u = u + ldel   ; res(l:u) = delim
    enddo
  end subroutine join_c
!---------------------------------------------------------!
! strsplit(string,word,delim,quot,mark,bracket,nword)     !
!---------------------------------------------------------!
  pure subroutine StrSplit(String,Word,NWord,Delim,Mark,Quot,Comout,Next,Bracket)
  character(*),intent(in) :: String
  character(*),allocatable,intent(out) :: Word(:)
  integer,intent(out),optional         :: NWord
  character(1),intent(in),optional     :: Delim(:)
  character(1),intent(in),optional     :: Quot(:), Mark(:)
  character(1),intent(in),optional     :: Comout(:)
  character(1),intent(in),optional     :: Next(:)
  character(2),intent(in),optional     :: Bracket(:)
!
  character(1),allocatable :: DumDlm(:)
  character(1),allocatable :: DumM(:), DumQ(:)
  character(2),allocatable :: DumBK(:)
  character(1),allocatable :: DumCM(:)
  character(1),allocatable :: DumNX(:)
  integer(4) :: Mask(len(String))
  integer(4) :: LenStr, LenWrd, DumNWord
  integer(4) :: i, j, k, s, BraPnt
  logical :: Ld, Lm, Lq, Lbk, Lcm, Lnx
  logical :: QQ, BRA, NX
    LenStr=len(String)
!
    if(allocated(Word))deallocate(Word)
    LenWrd=len(Word)
!
    if(len_trim(String)==0)then
        if(present(NWord))NWord=1
        allocate(Word(1)) ; Word="" ; RETURN
    endif
!
    if(present(Delim))then
        allocate(DumDlm(size(Delim))) ; DumDlm=Delim
    else
        allocate(DumDlm(1))  ; DumDlm=(/" "/)
    endif
!
    if(present(Mark))then
        Lm=.TRUE.
        allocate(DumM(size(Mark))) ; DumM=Mark
        do i=1,size(Mark)
            if(ANY(Mark(i)==DumDlm))then
                Lm=.FALSE. ; EXIT
            endif
        enddo
    else
        allocate(DumM(1)) ; DumM=" " ; Lm=.FALSE.
    endif
!
    if(present(Quot))then
        Lq=.TRUE.
        allocate(DumQ(size(Quot))) ; DumQ=Quot
        do i=1,size(Quot)
            if(ANY(Quot(i)==DumDlm))then
                Lq=.FALSE. ; EXIT
            endif
        enddo
        if(Lq.and.Lm)then
            do i=1,size(Quot)
                if(ANY(Quot(i)==Mark))then
                    Lq=.FALSE. ; EXIT
                endif
            enddo
        endif
    else
        allocate(DumQ(1)) ; DumQ=" " ; Lq=.FALSE.
    endif
!
    if(present(Comout))then
        Lcm=.TRUE.
        allocate(DumCM(size(Comout))) ; DumCM=Comout
        do i=1,size(Comout)
            if(ANY(Comout(i) == DumDlm))then
                Lcm=.FALSE. ; EXIT
            endif
        enddo
        if(Lcm.and.Lm)then
            do i=1,size(Comout)
                if(ANY(Comout(i)==Mark))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lcm.and.Lq)then
            do i=1,size(Comout)
                if(ANY(Comout(i)==Quot))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumCM(1)) ; DumCM=" " ; Lcm=.FALSE.
    endif
!
    if(present(Next))then
        Lnx=.TRUE.
        allocate(DumNX(size(Next))) ; DumNX=Next
        do i=1,size(Next)
            if(ANY(Next(i)==DumDlm))then
                Lnx=.FALSE. ; EXIT
            endif
        enddo
        if(Lnx.and.Lm)then
            do i=1,size(Next)
                if(ANY(Next(i)==Mark))then
                    Lnx=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lnx.and.Lq)then
            do i=1,size(Next)
                if(ANY(Next(i)==Quot))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lnx.and.Lcm)then
            do i=1,size(Next)
                if(ANY(Next(i)==Comout))then
                    Lnx=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumNX(1)) ; DumNX=" " ; Lnx=.FALSE.
    endif
!
    if(present(Bracket))then
        Lbk=.TRUE.
        allocate(DumBK(size(Bracket))) ; DumBK=Bracket
        do i=1,size(Bracket)
            if(ANY(Bracket(i)(1:1)==DumDlm).or.&
              &ANY(Bracket(i)(2:2)==DumDlm))then
                Lbk=.FALSE. ; EXIT
            endif
        enddo
        if(Lbk.and.Lm)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Mark).or.&
                  &ANY(Bracket(i)(2:2)==Mark))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lq)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Quot).or.&
                  &ANY(Bracket(i)(2:2)==Quot))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lcm)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Comout).or.&
                  &ANY(Bracket(i)(2:2)==Comout))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lnx)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Next).or.&
                  &ANY(Bracket(i)(2:2)==Next))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumBK(1)) ; DumBK="  " ; Lbk=.FALSE.
    endif
!
    DumNWord=0 ; j=0 ; k=0
    Mask=0
    QQ=.FALSE. ; BRA=.FALSE. ; NX=.FALSE.
!
    do i=1,LenStr
        k=k+1
!
        if(NX.and.String(i:i)==" ")CYCLE
        if(NX.and.ANY(String(i:i)==DumNX))then
            NX=.FALSE. ; CYCLE
        endif
        if(NX)NX=.FALSE.
!
        if(ANY(String(i:i)==DumDlm))then
            if(QQ.or.BRA)then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            j=0
        elseif(Lm.and.ANY(String(i:i)==DumM))then
            if(QQ.or.BRA)then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            DumNWord=DumNWord+1 ; j=0 ; Mask(k)=DumNWord
        elseif(Lq.and.ANY(String(i:i)==DumQ))then
            if(BRA)then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            j=0 ; QQ = .not. QQ
        elseif(Lcm.and.ANY(String(i:i)==DumCM))then
            if(QQ .or. BRA)then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            if(j==0)then
                k=s*LenStr ; EXIT
            else
                j=j+1 ; if(j==1)DumNWord=DumNWord+1
                Mask(k)=DumNWord
            endif
        elseif(Lnx.and.ANY(String(i:i)==DumNX))then
            k=s*LenStr ; NX=.TRUE. ; EXIT
        elseif(Lbk.and.ANY(String(i:i)==DumBK(:)(1:1)))then
            if(QQ .or. BRA)then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            do BraPnt=1,size(DumBK)
               if(String(i:i)==DumBK(BraPnt)(1:1))EXIT
            enddo
            DumNWord=DumNWord+1 ; j=1
            Mask(k)=DumNWord ; BRA=.TRUE.
        elseif(Lbk.and.ANY(String(i:i)==DumBK(:)(2:2)))then
            if(QQ.or.String(i:i)/=DumBK(BraPnt)(2:2))then
                Mask(k)=DumNWord ; j=j+1 ; CYCLE
            endif
            if(BRA)then
                Mask(k)=DumNWord ; j=0 ; BRA=.FALSE. ; CYCLE
            endif
            j=j+1 ; if(j==1)DumNWord=DumNWord+1
            Mask(k)=DumNWord
        else
            j=j+1 ; if(j==1)DumNWord=DumNWord+1
            Mask(k)=DumNWord
        endif
    enddo
    if(.not. NX)then
        QQ=.FALSE. ; BRA=.FALSE. ; j=0
    endif
!
    if(allocated(DumDlm))deallocate(DumDlm)
    if(allocated(DumM))deallocate(DumM)
    if(allocated(DumQ))deallocate(DumQ)
    if(allocated(DumCM))deallocate(DumCM)
    if(allocated(DumNX))deallocate(DumNX)
    if(allocated(DumBK))deallocate(DumBK)
!
    if(DumNWord==0)then
        if(present(NWord))NWord=1
        allocate(Word(1)) ; Word="" ; RETURN
    endif
    allocate(Word(DumNWord)) ; Word=""
!
    k=0 ; j=0
    do i=1,lenStr
        if(Mask(i)==0)CYCLE
        if(Mask(i)/=k) j=0
        j=j+1 ; k=Mask(i)
        if(j<=LenWrd)Word(Mask(i))(j:j)=String(i:i)
    enddo
    if(present(NWord))NWord=DumNWord
    RETURN
  end subroutine StrSplit
!
  pure subroutine StrSplit_Arr(String,Word,NWord,Delim,Mark,Quot,Comout,Next,Bracket)
  character(*),intent(in) :: String(:)
  character(*),allocatable,intent(out) :: Word(:)
  integer,intent(out),optional :: NWord
  character(1),intent(in),optional :: Delim(:)
  character(1),intent(in),optional :: Quot(:), Mark(:)
  character(1),intent(in),optional :: Comout(:)
  character(1),intent(in),optional :: Next(:)
  character(2),intent(in),optional :: Bracket(:)
!
  character(len(String)*size(String)) :: DumStr
  character(1),allocatable :: DumDlm(:)
  character(1),allocatable :: DumM(:), DumQ(:)
  character(2),allocatable :: DumBK(:)
  character(1),allocatable :: DumCM(:)
  character(1),allocatable :: DumNX(:)
  integer(4) :: Mask(len(String)*size(String))
  integer(4) :: LenStr, LenWrd, DumNWord
  integer(4) :: i, j, k, s, BraPnt
  logical :: Ld, Lm, Lq, Lbk, Lcm, Lnx
  logical :: QQ, BRA, NX
    LenStr=len(String)
    DumStr=""
    j=1
    do i=1,size(String)
        DumStr(j:j+LenStr-1)=String(i)
        j=j+LenStr
    enddo
!
    if(allocated(Word))deallocate(Word)
    LenWrd=len(Word)
!
    if(len_trim(DumStr)==0)then
        if(present(NWord))NWord=1
        allocate(Word(1)) ; Word="" ; RETURN
    endif
!
    if(present(Delim))then
        allocate(DumDlm(size(Delim))) ; DumDlm=Delim
    else
        allocate(DumDlm(1))  ; DumDlm=(/" "/)
    endif
!
    if(present(Mark))then
        Lm=.TRUE.
        allocate(DumM(size(Mark))) ; DumM=Mark
        do i=1,size(Mark)
            if(ANY(Mark(i)==DumDlm))then
                Lm=.FALSE. ; EXIT
            endif
        enddo
    else
        allocate(DumM(1)) ; DumM=" " ; Lm=.FALSE.
    endif
!
    if(present(Quot))then
        Lq=.TRUE.
        allocate(DumQ(size(Quot))) ; DumQ=Quot
        do i=1,size(Quot)
            if(ANY(Quot(i)==DumDlm))then
                Lq=.FALSE. ; EXIT
            endif
        enddo
        if(Lq.and.Lm)then
            do i=1,size(Quot)
                if(ANY(Quot(i)==Mark))then
                    Lq=.FALSE. ; EXIT
                endif
            enddo
        endif
    else
        allocate(DumQ(1)) ; DumQ=" " ; Lq=.FALSE.
    endif
!
    if(present(Comout))then
        Lcm=.TRUE.
        allocate(DumCM(size(Comout))) ; DumCM=Comout
        do i=1,size(Comout)
            if(ANY(Comout(i) == DumDlm))then
                Lcm=.FALSE. ; EXIT
            endif
        enddo
        if(Lcm.and.Lm)then
            do i=1,size(Comout)
                if(ANY(Comout(i)==Mark))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lcm.and.Lq)then
            do i=1,size(Comout)
                if(ANY(Comout(i)==Quot))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumCM(1)) ; DumCM=" " ; Lcm=.FALSE.
    endif
!
    if(present(Next))then
        Lnx=.TRUE.
        allocate(DumNX(size(Next))) ; DumNX=Next
        do i=1,size(Next)
            if(ANY(Next(i)==DumDlm))then
                Lnx=.FALSE. ; EXIT
            endif
        enddo
        if(Lnx.and.Lm)then
            do i=1,size(Next)
                if(ANY(Next(i)==Mark))then
                    Lnx=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lnx.and.Lq)then
            do i=1,size(Next)
                if(ANY(Next(i)==Quot))then
                    Lcm=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lnx.and.Lcm)then
            do i=1,size(Next)
                if(ANY(Next(i)==Comout))then
                    Lnx=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumNX(1)) ; DumNX=" " ; Lnx=.FALSE.
    endif
!
    if(present(Bracket))then
        Lbk=.TRUE.
        allocate(DumBK(size(Bracket))) ; DumBK=Bracket
        do i=1,size(Bracket)
            if(ANY(Bracket(i)(1:1)==DumDlm).or.&
              &ANY(Bracket(i)(2:2)==DumDlm))then
                Lbk=.FALSE. ; EXIT
            endif
        enddo
        if(Lbk.and.Lm)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Mark).or.&
                  &ANY(Bracket(i)(2:2)==Mark))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lq)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Quot).or.&
                  &ANY(Bracket(i)(2:2)==Quot))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lcm)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Comout).or.&
                  &ANY(Bracket(i)(2:2)==Comout))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
        if(Lbk.and.Lnx)then
            do i=1,size(Bracket)
                if(ANY(Bracket(i)(1:1)==Next).or.&
                  &ANY(Bracket(i)(2:2)==Next))then
                    Lbk=.FALSE. ;  EXIT
                endif
            enddo
        endif
    else
        allocate(DumBK(1)) ; DumBK="  " ; Lbk=.FALSE.
    endif
!
    DumNWord=0 ; j=0 ; k=0
    Mask=0
    QQ=.FALSE. ; BRA=.FALSE. ; NX=.FALSE.
!
    do s=1,size(String)
        do i=1,LenStr
            k=k+1
!
            if(NX.and.String(s)(i:i)==" ")CYCLE
            if(NX.and.ANY(String(s)(i:i)==DumNX))then
                NX=.FALSE. ; CYCLE
            endif
            if(NX)NX=.FALSE.
!
            if(ANY(String(s)(i:i)==DumDlm))then
                if(QQ.or.BRA)then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                j=0
            elseif(Lm.and.ANY(String(s)(i:i)==DumM))then
                if(QQ.or.BRA)then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                DumNWord=DumNWord+1 ; j=0 ; Mask(k)=DumNWord
            elseif(Lq.and.ANY(String(s)(i:i)==DumQ))then
                if(BRA)then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                j=0 ; QQ = .not. QQ
            elseif(Lcm.and.ANY(String(s)(i:i)==DumCM))then
                if(QQ .or. BRA)then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                if(j==0)then
                    k=s*LenStr ; EXIT
                else
                    j=j+1 ; if(j==1)DumNWord=DumNWord+1
                    Mask(k)=DumNWord
                endif
            elseif(Lnx.and.ANY(String(s)(i:i)==DumNX))then
                k=s*LenStr ; NX=.TRUE. ; EXIT
            elseif(Lbk.and.ANY(String(s)(i:i)==DumBK(:)(1:1)))then
                if(QQ .or. BRA)then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                do BraPnt=1,size(DumBK)
                   if(String(s)(i:i)==DumBK(BraPnt)(1:1))EXIT
                enddo
                DumNWord=DumNWord+1 ; j=1
                Mask(k)=DumNWord ; BRA=.TRUE.
            elseif(Lbk.and.ANY(String(s)(i:i)==DumBK(:)(2:2)))then
                if(QQ.or.String(s)(i:i)/=DumBK(BraPnt)(2:2))then
                    Mask(k)=DumNWord ; j=j+1 ; CYCLE
                endif
                if(BRA)then
                    Mask(k)=DumNWord ; j=0 ; BRA=.FALSE. ; CYCLE
                endif
                j=j+1 ; if(j==1)DumNWord=DumNWord+1
                Mask(k)=DumNWord
            else
                j=j+1 ; if(j==1)DumNWord=DumNWord+1
                Mask(k)=DumNWord
            endif
        enddo
        if(.not. NX)then
            QQ=.FALSE. ; BRA=.FALSE. ; j=0
        endif
    enddo
!
    if(allocated(DumDlm))deallocate(DumDlm)
    if(allocated(DumM))deallocate(DumM)
    if(allocated(DumQ))deallocate(DumQ)
    if(allocated(DumCM))deallocate(DumCM)
    if(allocated(DumNX))deallocate(DumNX)
    if(allocated(DumBK))deallocate(DumBK)
!
    if(DumNWord==0)then
        if(present(NWord))NWord=1
        allocate(Word(1)) ; Word="" ; RETURN
    endif
    allocate(Word(DumNWord)) ; Word=""
!
    k=0 ; j=0
    do i=1,lenStr*size(String)
        if(Mask(i)==0)CYCLE
        if(Mask(i)/=k) j=0
        j=j+1 ; k=Mask(i)
        if(j<=LenWrd)Word(Mask(i))(j:j)=DumStr(i:i)
    enddo
    if(present(NWord))NWord=DumNWord
    RETURN
  end subroutine StrSplit_Arr
!---------------------------------------------------------!
! TextWrap(Text,Line,NLine)                               !
!   string formatting                                     !
!---------------------------------------------------------!
  subroutine TextWrap(Text, Wrap)
  character(*),intent(in) :: Text
  character(*), allocatable, intent(inout) :: Wrap(:)
  character(len(Text)), allocatable :: Word(:)
  integer(4)  :: Width, NWord, Offset, NLine
  integer(4)  :: r, edge
  integer(4), allocatable :: offsets(:), minima(:), breaks(:)
  integer(4), allocatable :: row(:), col(:)
  logical :: NextLine(len_trim(Text))
  integer(4) :: i, j, k, x, y, N
    Width = len(Wrap)
    call StrSplit(Text,Word,NWord=NWord)
!
    allocate(offsets(0:NWord),minima(0:NWord),breaks(0:NWord))
    offsets(0) = 0
    do i = 1,NWord
      offsets(i) = offsets(i-1) + len_trim(word(i))
    enddo
    minima(0) = 0 ; minima(1:NWord) = 10**9
    breaks = 0
    N = NWord + 1
    i = 0
    Offset = 0
    allocate(row(N+1),col(N+1))
    row = (/(i,i=0,N)/)
    col = (/(i,i=0,N)/)
    do
      r = minval([N,2**(i+1)])
      edge = 2**i + Offset
      call smawk(row(Offset+1:edge),col(edge+1:r+Offset))
      x = minima(r-1+offset)
      do j = 2**i,r-2
        y = cost(j + offset,r-1+offset)
        if(y<=x)then
          n = n - j
          i = -1
          offset = offset + j
          EXIT
        endif
      enddo
      if(r==n)EXIT
      i = i + 1
    enddo
!
    j = nword ; nline = 0
    do
      i = breaks(j)
      j = i
      NLine = NLine + 1
      if(j <= 0)EXIT
    enddo
!
    if(allocated(wrap))deallocate(wrap)
    allocate(wrap(nline))
!
    j = NWord
    do
      i = breaks(j)
      wrap(nline) = join(word(i+1:j)," ")
      j = i
      NLine = NLine - 1
      if(j <= 0)EXIT
    enddo
    deallocate(minima,breaks,Word,row,col)
    contains
      integer(4) function cost(i,j) result(res)
      integer(4), intent(in) :: i,j
      integer(4) :: w
        w = offsets(j) - offsets(i) + j - i - 1
        if(w>width)then
          res = 10**7 * (w - width)
        else
          res = minima(i) + (width - w)**2
        endif
        RETURN
      end function cost
!
      recursive subroutine smawk(row,col)
      integer(4), intent(in) :: row(:), col(:)
      integer(4) :: stack(size(row))
      integer(4) ::  i,j,c,ls,fin
         stack = 0
         ls = 0
         i = 1
         do 
           if(ls==0)then
             ls = 1
             stack(ls) = row(i)
             i = i + 1
           else
             c = col(ls)
             if(cost(stack(ls),c)<cost(row(i),c))then
               if(ls<size(col))then
                 ls = ls + 1
                 stack(ls) = row(i)
               endif
               i = i + 1
             else
               ls = ls - 1
             endif
           endif
           if(i>size(row))EXIT
         enddo
!
        if(size(col)>1)call smawk(stack(:ls),col(2::2))
!
        i = 1 ; j = 1
        do
          if(j<size(col))then
            fin = breaks(col(j+1))
          else
            fin = stack(ls)
          endif
          c = cost(stack(i),col(j))
          if(c<minima(col(j)))then
            minima(col(j)) = c
            breaks(col(j)) = stack(i)
          endif
          if(stack(i)<fin)then
            i = i + 1
          else
            j = j + 2
          endif
          if(j>=size(col))EXIT
        enddo
      end subroutine smawk
  end subroutine TextWrap
end module spur_String
