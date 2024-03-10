module rctn   
    ! use c4_io, only : strLength
    implicit none

    public retReactionMT, retReactionName, strLength

    integer*4, parameter :: nrmax = 54       ! max # of reactions tested

    character*9, parameter :: LIST(nrmax) = (/ &
     'Total    ', 'Elastic  ', '(z,n)    ', '(z,2nd)  ', &    ! 4
     '(z,2n)   ', '(z,3n)   ', 'Fission  ', '(z,f)    ', &    ! 8
     '(z,nf)   ', '(z,2nf)  ', '(z,na)   ', '(z,n3a)  ', &    ! 12
     '(z,2na)  ', '(z,3na)  ', '(z,np)   ', '(z,n2a)  ', &    ! 16
     '(z,2n2a) ', '(z,nd)   ', '(z,nt)   ', '(z,nHe3) ', &    ! 20
     '(z,nd2a) ', '(z,nt2a) ', '(z,4n)   ', '(z,3nf)  ', &    ! 24
     '(z,2np)  ', '(z,3np)  ', '(z,n2p)  ', '(z,npa)  ', &    ! 28
     '(z,gamma)', '(z,p)    ', '(z,d)    ', '(z,t)    ', &    ! 32
     '(z,h)    ', '(z,a)    ', '(z,2a)   ', '(z,3a)   ', &    ! 36
     '(z,2p)   ', '(z,pa)   ', '(z,t2a)  ', '(z,d2a)  ', &    ! 40
     '(z,pd)   ', '(z,pt)   ', '(z,da)   ', 'Nonel-Cel', &    ! 44
     '(z,2npa) ', 'Mu-bar   ', 'Nu-bar   ', '     MT=5', &    ! 48
     'Elastic* ', 'Nonelast*', '(z,4np)  ', '(z,xa)   ', &    ! 52
     '    (z,X)', '(n,a_dis)' /)

    integer*4, parameter :: listMT(nrmax) = (/ &
       1,   2,   4,  11,  16,  17,  18,  19,  20,  21, &  ! 10
      22,  23,  24,  25,  28,  29,  30,  32,  33,  34, &  ! 20
      35,  36,  37,  38,  41,  42,  44,  45, 102, 103, &  ! 30
     104, 105, 106, 107, 108, 109, 111, 112, 113, 114, &  ! 40
     115, 116, 117,   3, 999, 251, 456,   5, 902, 903, &  ! 50
     156, 207,   5, 851/)

    integer*4, private :: ln1,ln2,ll1,ll2,i
    ! public retReactionMT, retReactionName

    contains 

    integer*4 function retReactionMT(name12)
      !! Look for the supplied 12-character reaction name and
      !! return the corresponding MT value for this reaction.
      !! If reaction string is not found, return -1.
      !! The name is * instead of 9-characters since 
      !! the format in *.xsc file is 12. It is converted to 
      !! 9 characters in the first executable line.
    
      character(len=*), intent(in) :: name12   ! reaction name to find (* characters)
      character*9 :: name                      ! reaction name to find (9 characters)

      name = name12(1:9)

      i = 1
      call strLength(NAME,LN1,LN2)
      do
         call strLength(LIST(I),LL1,LL2)
         if(name(ln1:ln2) == list(i)(ll1:ll2)) then
             retReactionMT = listMT(i)
             return
         endif
         i = i + 1
         if(i > nrmax) then
            !  WRITE(0,*) 'MT NOT FOUND FOR ',name(ln1:ln2)
             retReactionMT = -1
             return
         endif
      end do

    end function retReactionMT


    subroutine retReactionName(name,mtAsked)
        !! Return the  9-character reaction name 
        !! corresponding to the supplied MT value.
        !! If MT is not found, return string "wrong MT".
        !! Inverse functionality to retReactionMT function which returns MT.
        
        character*9, intent(out) :: name  ! reaction name to return
        integer*4, intent(in) :: mtAsked  ! MT value asking for reaction name

        i = 1
        do
            if(mtAsked == listMT(i)) then
                name = LIST(i)
                return
            endif
            i = i + 1
            if(i > nrmax) then
                WRITE(0,*) 'MT NOT FOUND FOR ',name(ln1:ln2)
                name = "wrong MT "
                return
            endif
        end do

    end subroutine retReactionName

    subroutine strLength(st, ls1, ls2)

        implicit none

        !! search the input string (st) and return indicies of the first (ls1)
        !! and last (ls2) non-blank characters. If a string contains all blanks,
        !! print a warning message on unit 0 and return ls1=ls2=0.

        character*(*), intent(in) :: st         ! string to search
        integer*4, intent(out)    :: ls1        ! first non-blank character in st
        integer*4, intent(out)    :: ls2        ! last  non-blank character in st

        integer*4 i

        i = len(st)

        do while(i > 0)
            if(st(i:i) .ne. ' ') exit
            i = i - 1
        end do
        ls2 = i

        if(i == 0) then
            write(0,*) 'EMPTY STRING'
            ls1 = 0
            return
        endif

        i = 1
        do while(i < ls2)
            if(st(i:i) .ne. ' ') exit
            i = i + 1
        end do
        ls1 = i

        return
    end subroutine strLength

end module rctn
