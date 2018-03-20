module spur_string_split
  implicit none
  public  split_length, split_delimiter
contains
!---------------------------------------------------------!
! strsplit(string,word,delim,quot,mark,bracket,nword)     !
!---------------------------------------------------------!
  function split_length(string,length) result(res)
  character(*),intent(in)        :: string
  integer,intent(in)             :: length
  character(lres(string,length)) :: res(nres(string,length))
  integer                        :: i,l,s,lb,ub
    s = len(string) ; l = minval([s,maxval([length,1],1)],1)
    lb = 0 ; ub = 0
print*,'[',size(res),']'
    do i=1,size(res)
      lb = ub + 1 ; ub = minval([lb+l-1,s],1)
      res(i) = string(lb:ub)//repeat(' ',(l-lb+ub-1))
    enddo
  contains
  end function split_length
!
  pure integer function lres(string,length)
  character(*),intent(in) :: string
  integer,intent(in)      :: length
    lres = minval([len(string),maxval([length,1],1)],1)
  end function lres
!
  pure integer function nres(string,length)
  character(*),intent(in) :: string
  integer,intent(in)      :: length
    nres = (len(string)-1)/minval([len(string),maxval([length,1],1)],1)+1
  end function nres
!
  function split_delimiter(string,delimiter) result(res)
  character(*),intent(in)          :: string
  character(*),intent(in),optional :: delimiter
  character(:),allocatable         :: res(:)
  integer                          :: i,j,l(2)
    if(present(delimiter))then
      call GetSplitSize(string,delimiter,l)
    else
      call GetSplitSize(string,' ',l)
    endif
!
 print*,l
    allocate(character(l(1))::res(l(2)))
    do i=1,l(2)
      
    enddo
    res = ''
  end function split_delimiter
!
  pure subroutine GetSplitSize(string,dl,res)
  character(*),intent(in) :: string
  character(*),intent(in) :: dl
  integer,intent(out)     :: res(2)
  integer                 :: i,j,k,l
    l = len(string)
    j = 1 ; k = l ; res = 0
    do i=1,l
      if(index(dl,string(i:i))>0)then
        j = i + 1
        res(1) = maxval([res(1),i-k],1)
        k = l
      endif
      if(i==j)then
        res(2) = res(2) + 1 ; k = i
      endif
    enddo
  end subroutine GetSplitSize
!
! pure subroutine StrSplit(String,Word,NWord,Delim,Mark,Quot,Comout,Next,Bracket)
! character(*),intent(in) :: String
! character(*),allocatable,intent(out) :: Word(:)
! integer,intent(out),optional         :: NWord
! character(1),intent(in),optional     :: Delim(:)
! character(1),intent(in),optional     :: Quot(:), Mark(:)
! character(1),intent(in),optional     :: Comout(:)
! character(1),intent(in),optional     :: Next(:)
! character(2),intent(in),optional     :: Bracket(:)
!
! character(1),allocatable :: DumDlm(:)
! character(1),allocatable :: DumM(:), DumQ(:)
! character(2),allocatable :: DumBK(:)
! character(1),allocatable :: DumCM(:)
! character(1),allocatable :: DumNX(:)
! integer(4) :: Mask(len(String))
! integer(4) :: LenStr, LenWrd, DumNWord
! integer(4) :: i, j, k, s, BraPnt
! logical :: Ld, Lm, Lq, Lbk, Lcm, Lnx
! logical :: QQ, BRA, NX
!   LenStr=len(String)
!
!   if(allocated(Word))deallocate(Word)
!   LenWrd=len(Word)
!
!   if(len_trim(String)==0)then
!       if(present(NWord))NWord=1
!       allocate(Word(1)) ; Word="" ; RETURN
!   endif
!
!   if(present(Delim))then
!       allocate(DumDlm(size(Delim))) ; DumDlm=Delim
!   else
!       allocate(DumDlm(1))  ; DumDlm=(/" "/)
!   endif
!
!   if(present(Mark))then
!       Lm=.TRUE.
!       allocate(DumM(size(Mark))) ; DumM=Mark
!       do i=1,size(Mark)
!           if(ANY(Mark(i)==DumDlm))then
!               Lm=.FALSE. ; EXIT
!           endif
!       enddo
!   else
!       allocate(DumM(1)) ; DumM=" " ; Lm=.FALSE.
!   endif
!
!   if(present(Quot))then
!       Lq=.TRUE.
!       allocate(DumQ(size(Quot))) ; DumQ=Quot
!       do i=1,size(Quot)
!           if(ANY(Quot(i)==DumDlm))then
!               Lq=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lq.and.Lm)then
!           do i=1,size(Quot)
!               if(ANY(Quot(i)==Mark))then
!                   Lq=.FALSE. ; EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumQ(1)) ; DumQ=" " ; Lq=.FALSE.
!   endif
!
!   if(present(Comout))then
!       Lcm=.TRUE.
!       allocate(DumCM(size(Comout))) ; DumCM=Comout
!       do i=1,size(Comout)
!           if(ANY(Comout(i) == DumDlm))then
!               Lcm=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lcm.and.Lm)then
!           do i=1,size(Comout)
!               if(ANY(Comout(i)==Mark))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lcm.and.Lq)then
!           do i=1,size(Comout)
!               if(ANY(Comout(i)==Quot))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumCM(1)) ; DumCM=" " ; Lcm=.FALSE.
!   endif
!
!   if(present(Next))then
!       Lnx=.TRUE.
!       allocate(DumNX(size(Next))) ; DumNX=Next
!       do i=1,size(Next)
!           if(ANY(Next(i)==DumDlm))then
!               Lnx=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lnx.and.Lm)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Mark))then
!                   Lnx=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lnx.and.Lq)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Quot))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lnx.and.Lcm)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Comout))then
!                   Lnx=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumNX(1)) ; DumNX=" " ; Lnx=.FALSE.
!   endif
!
!   if(present(Bracket))then
!       Lbk=.TRUE.
!       allocate(DumBK(size(Bracket))) ; DumBK=Bracket
!       do i=1,size(Bracket)
!           if(ANY(Bracket(i)(1:1)==DumDlm).or.&
!             &ANY(Bracket(i)(2:2)==DumDlm))then
!               Lbk=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lbk.and.Lm)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Mark).or.&
!                 &ANY(Bracket(i)(2:2)==Mark))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lq)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Quot).or.&
!                 &ANY(Bracket(i)(2:2)==Quot))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lcm)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Comout).or.&
!                 &ANY(Bracket(i)(2:2)==Comout))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lnx)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Next).or.&
!                 &ANY(Bracket(i)(2:2)==Next))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumBK(1)) ; DumBK="  " ; Lbk=.FALSE.
!   endif
!
!   DumNWord=0 ; j=0 ; k=0
!   Mask=0
!   QQ=.FALSE. ; BRA=.FALSE. ; NX=.FALSE.
!
!   do i=1,LenStr
!       k=k+1
!
!       if(NX.and.String(i:i)==" ")CYCLE
!       if(NX.and.ANY(String(i:i)==DumNX))then
!           NX=.FALSE. ; CYCLE
!       endif
!       if(NX)NX=.FALSE.
!
!       if(ANY(String(i:i)==DumDlm))then
!           if(QQ.or.BRA)then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           j=0
!       elseif(Lm.and.ANY(String(i:i)==DumM))then
!           if(QQ.or.BRA)then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           DumNWord=DumNWord+1 ; j=0 ; Mask(k)=DumNWord
!       elseif(Lq.and.ANY(String(i:i)==DumQ))then
!           if(BRA)then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           j=0 ; QQ = .not. QQ
!       elseif(Lcm.and.ANY(String(i:i)==DumCM))then
!           if(QQ .or. BRA)then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           if(j==0)then
!               k=s*LenStr ; EXIT
!           else
!               j=j+1 ; if(j==1)DumNWord=DumNWord+1
!               Mask(k)=DumNWord
!           endif
!       elseif(Lnx.and.ANY(String(i:i)==DumNX))then
!           k=s*LenStr ; NX=.TRUE. ; EXIT
!       elseif(Lbk.and.ANY(String(i:i)==DumBK(:)(1:1)))then
!           if(QQ .or. BRA)then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           do BraPnt=1,size(DumBK)
!              if(String(i:i)==DumBK(BraPnt)(1:1))EXIT
!           enddo
!           DumNWord=DumNWord+1 ; j=1
!           Mask(k)=DumNWord ; BRA=.TRUE.
!       elseif(Lbk.and.ANY(String(i:i)==DumBK(:)(2:2)))then
!           if(QQ.or.String(i:i)/=DumBK(BraPnt)(2:2))then
!               Mask(k)=DumNWord ; j=j+1 ; CYCLE
!           endif
!           if(BRA)then
!               Mask(k)=DumNWord ; j=0 ; BRA=.FALSE. ; CYCLE
!           endif
!           j=j+1 ; if(j==1)DumNWord=DumNWord+1
!           Mask(k)=DumNWord
!       else
!           j=j+1 ; if(j==1)DumNWord=DumNWord+1
!           Mask(k)=DumNWord
!       endif
!   enddo
!   if(.not. NX)then
!       QQ=.FALSE. ; BRA=.FALSE. ; j=0
!   endif
!
!   if(allocated(DumDlm))deallocate(DumDlm)
!   if(allocated(DumM))deallocate(DumM)
!   if(allocated(DumQ))deallocate(DumQ)
!   if(allocated(DumCM))deallocate(DumCM)
!   if(allocated(DumNX))deallocate(DumNX)
!   if(allocated(DumBK))deallocate(DumBK)
!
!   if(DumNWord==0)then
!       if(present(NWord))NWord=1
!       allocate(Word(1)) ; Word="" ; RETURN
!   endif
!   allocate(Word(DumNWord)) ; Word=""
!
!   k=0 ; j=0
!   do i=1,lenStr
!       if(Mask(i)==0)CYCLE
!       if(Mask(i)/=k) j=0
!       j=j+1 ; k=Mask(i)
!       if(j<=LenWrd)Word(Mask(i))(j:j)=String(i:i)
!   enddo
!   if(present(NWord))NWord=DumNWord
!   RETURN
! end subroutine StrSplit
! pure subroutine StrSplit_Arr(String,Word,NWord,Delim,Mark,Quot,Comout,Next,Bracket)
! character(*),intent(in) :: String(:)
! character(*),allocatable,intent(out) :: Word(:)
! integer,intent(out),optional :: NWord
! character(1),intent(in),optional :: Delim(:)
! character(1),intent(in),optional :: Quot(:), Mark(:)
! character(1),intent(in),optional :: Comout(:)
! character(1),intent(in),optional :: Next(:)
! character(2),intent(in),optional :: Bracket(:)
!
! character(len(String)*size(String)) :: DumStr
! character(1),allocatable :: DumDlm(:)
! character(1),allocatable :: DumM(:), DumQ(:)
! character(2),allocatable :: DumBK(:)
! character(1),allocatable :: DumCM(:)
! character(1),allocatable :: DumNX(:)
! integer(4) :: Mask(len(String)*size(String))
! integer(4) :: LenStr, LenWrd, DumNWord
! integer(4) :: i, j, k, s, BraPnt
! logical :: Ld, Lm, Lq, Lbk, Lcm, Lnx
! logical :: QQ, BRA, NX
!   LenStr=len(String)
!   DumStr=""
!   j=1
!   do i=1,size(String)
!       DumStr(j:j+LenStr-1)=String(i)
!       j=j+LenStr
!   enddo
!
!   if(allocated(Word))deallocate(Word)
!   LenWrd=len(Word)
!
!   if(len_trim(DumStr)==0)then
!       if(present(NWord))NWord=1
!       allocate(Word(1)) ; Word="" ; RETURN
!   endif
!
!   if(present(Delim))then
!       allocate(DumDlm(size(Delim))) ; DumDlm=Delim
!   else
!       allocate(DumDlm(1))  ; DumDlm=(/" "/)
!   endif
!
!   if(present(Mark))then
!       Lm=.TRUE.
!       allocate(DumM(size(Mark))) ; DumM=Mark
!       do i=1,size(Mark)
!           if(ANY(Mark(i)==DumDlm))then
!               Lm=.FALSE. ; EXIT
!           endif
!       enddo
!   else
!       allocate(DumM(1)) ; DumM=" " ; Lm=.FALSE.
!   endif
!
!   if(present(Quot))then
!       Lq=.TRUE.
!       allocate(DumQ(size(Quot))) ; DumQ=Quot
!       do i=1,size(Quot)
!           if(ANY(Quot(i)==DumDlm))then
!               Lq=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lq.and.Lm)then
!           do i=1,size(Quot)
!               if(ANY(Quot(i)==Mark))then
!                   Lq=.FALSE. ; EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumQ(1)) ; DumQ=" " ; Lq=.FALSE.
!   endif
!
!   if(present(Comout))then
!       Lcm=.TRUE.
!       allocate(DumCM(size(Comout))) ; DumCM=Comout
!       do i=1,size(Comout)
!           if(ANY(Comout(i) == DumDlm))then
!               Lcm=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lcm.and.Lm)then
!           do i=1,size(Comout)
!               if(ANY(Comout(i)==Mark))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lcm.and.Lq)then
!           do i=1,size(Comout)
!               if(ANY(Comout(i)==Quot))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumCM(1)) ; DumCM=" " ; Lcm=.FALSE.
!   endif
!
!   if(present(Next))then
!       Lnx=.TRUE.
!       allocate(DumNX(size(Next))) ; DumNX=Next
!       do i=1,size(Next)
!           if(ANY(Next(i)==DumDlm))then
!               Lnx=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lnx.and.Lm)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Mark))then
!                   Lnx=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lnx.and.Lq)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Quot))then
!                   Lcm=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lnx.and.Lcm)then
!           do i=1,size(Next)
!               if(ANY(Next(i)==Comout))then
!                   Lnx=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumNX(1)) ; DumNX=" " ; Lnx=.FALSE.
!   endif
!
!   if(present(Bracket))then
!       Lbk=.TRUE.
!       allocate(DumBK(size(Bracket))) ; DumBK=Bracket
!       do i=1,size(Bracket)
!           if(ANY(Bracket(i)(1:1)==DumDlm).or.&
!             &ANY(Bracket(i)(2:2)==DumDlm))then
!               Lbk=.FALSE. ; EXIT
!           endif
!       enddo
!       if(Lbk.and.Lm)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Mark).or.&
!                 &ANY(Bracket(i)(2:2)==Mark))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lq)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Quot).or.&
!                 &ANY(Bracket(i)(2:2)==Quot))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lcm)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Comout).or.&
!                 &ANY(Bracket(i)(2:2)==Comout))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!       if(Lbk.and.Lnx)then
!           do i=1,size(Bracket)
!               if(ANY(Bracket(i)(1:1)==Next).or.&
!                 &ANY(Bracket(i)(2:2)==Next))then
!                   Lbk=.FALSE. ;  EXIT
!               endif
!           enddo
!       endif
!   else
!       allocate(DumBK(1)) ; DumBK="  " ; Lbk=.FALSE.
!   endif
!
!   DumNWord=0 ; j=0 ; k=0
!   Mask=0
!   QQ=.FALSE. ; BRA=.FALSE. ; NX=.FALSE.
!
!   do s=1,size(String)
!       do i=1,LenStr
!           k=k+1
!
!           if(NX.and.String(s)(i:i)==" ")CYCLE
!           if(NX.and.ANY(String(s)(i:i)==DumNX))then
!               NX=.FALSE. ; CYCLE
!           endif
!           if(NX)NX=.FALSE.
!
!           if(ANY(String(s)(i:i)==DumDlm))then
!               if(QQ.or.BRA)then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               j=0
!           elseif(Lm.and.ANY(String(s)(i:i)==DumM))then
!               if(QQ.or.BRA)then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               DumNWord=DumNWord+1 ; j=0 ; Mask(k)=DumNWord
!           elseif(Lq.and.ANY(String(s)(i:i)==DumQ))then
!               if(BRA)then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               j=0 ; QQ = .not. QQ
!           elseif(Lcm.and.ANY(String(s)(i:i)==DumCM))then
!               if(QQ .or. BRA)then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               if(j==0)then
!                   k=s*LenStr ; EXIT
!               else
!                   j=j+1 ; if(j==1)DumNWord=DumNWord+1
!                   Mask(k)=DumNWord
!               endif
!           elseif(Lnx.and.ANY(String(s)(i:i)==DumNX))then
!               k=s*LenStr ; NX=.TRUE. ; EXIT
!           elseif(Lbk.and.ANY(String(s)(i:i)==DumBK(:)(1:1)))then
!               if(QQ .or. BRA)then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               do BraPnt=1,size(DumBK)
!                  if(String(s)(i:i)==DumBK(BraPnt)(1:1))EXIT
!               enddo
!               DumNWord=DumNWord+1 ; j=1
!               Mask(k)=DumNWord ; BRA=.TRUE.
!           elseif(Lbk.and.ANY(String(s)(i:i)==DumBK(:)(2:2)))then
!               if(QQ.or.String(s)(i:i)/=DumBK(BraPnt)(2:2))then
!                   Mask(k)=DumNWord ; j=j+1 ; CYCLE
!               endif
!               if(BRA)then
!                   Mask(k)=DumNWord ; j=0 ; BRA=.FALSE. ; CYCLE
!               endif
!               j=j+1 ; if(j==1)DumNWord=DumNWord+1
!               Mask(k)=DumNWord
!           else
!               j=j+1 ; if(j==1)DumNWord=DumNWord+1
!               Mask(k)=DumNWord
!           endif
!       enddo
!       if(.not. NX)then
!           QQ=.FALSE. ; BRA=.FALSE. ; j=0
!       endif
!   enddo
!
!   if(allocated(DumDlm))deallocate(DumDlm)
!   if(allocated(DumM))deallocate(DumM)
!   if(allocated(DumQ))deallocate(DumQ)
!   if(allocated(DumCM))deallocate(DumCM)
!   if(allocated(DumNX))deallocate(DumNX)
!   if(allocated(DumBK))deallocate(DumBK)
!
!   if(DumNWord==0)then
!       if(present(NWord))NWord=1
!       allocate(Word(1)) ; Word="" ; RETURN
!   endif
!   allocate(Word(DumNWord)) ; Word=""
!
!   k=0 ; j=0
!   do i=1,lenStr*size(String)
!       if(Mask(i)==0)CYCLE
!       if(Mask(i)/=k) j=0
!       j=j+1 ; k=Mask(i)
!       if(j<=LenWrd)Word(Mask(i))(j:j)=DumStr(i:i)
!   enddo
!   if(present(NWord))NWord=DumNWord
!   RETURN
! end subroutine StrSplit_Arr
end module spur_string_split
