module endf_queue

    use endf_mf1_io
    use endf_mf2_io
    use endf_mf3_io
    use endf_mf4_io
    use endf_mf5_io
    use endf_mf6_io
    use endf_mf7_io
    use endf_mf8_io
    use endf_mf9_io
    use endf_mf10_io
    use endf_mf12_io
    use endf_mf13_io
    use endf_mf14_io
    use endf_mf15_io
    use endf_mf23_io
    use endf_mf26_io
    use endf_mf27_io
    use endf_mf28_io
    use endf_mf31_io
    use endf_mf32_io
    use endf_mf33_io
    use endf_mf34_io
    use endf_mf35_io
    use endf_mf40_io

    implicit none

    !! This module provides generic routines to locate, remove and insert
    !! an MF/MT section to/from a linked list of sections.

    !! To remove or "pop" a section, use the 'pop' function specifying the
    !! head of the list along with the MT number to find & remove. If it's
    !! found in the list then pop removes the section and returns a
    !! pointer to that section.

    !! To insert, use the 'put' function, specifying the head of the list
    !! and the section to insert. If a section is found in the list with
    !! the same MT as the one to be inserted, put does not insert the
    !! section and returns false. If there is not already a section with
    !! the same MT as the one to be inserted, then put inserts the section
    !! into the list and put returns true.

    !! To locate a section with a given MT in a list, use 'find'. Here the
    !! head of the list is provided along with the MT number to locate. If
    !! a section with the specified MT is found, find returns a pointer
    !! to that section; if not, it returns a pointer to null.

    !! this is yet another example of where an abstract section class
    !! would have come in quite handy. In Fortran95 we have to repeat
    !! these basic functions over & over for every MF type....

    private

    interface pop
        module procedure pop1
        module procedure pop2
        module procedure pop3
        module procedure pop4
        module procedure pop5
        module procedure pop6
        module procedure pop7
        module procedure pop8
        module procedure pop9
        module procedure pop10
        module procedure pop12
        module procedure pop13
        module procedure pop14
        module procedure pop15
        module procedure pop23
        module procedure pop26
        module procedure pop27
        module procedure pop28
        module procedure pop31
        module procedure pop32
        module procedure pop33
        module procedure pop34
        module procedure pop35
        module procedure pop40
    end interface

    interface put
        module procedure put1
        module procedure put2
        module procedure put3
        module procedure put4
        module procedure put5
        module procedure put6
        module procedure put7
        module procedure put8
        module procedure put9
        module procedure put10
        module procedure put12
        module procedure put13
        module procedure put14
        module procedure put15
        module procedure put23
        module procedure put26
        module procedure put27
        module procedure put28
        module procedure put31
        module procedure put32
        module procedure put33
        module procedure put34
        module procedure put35
        module procedure put40
    end interface

    interface find
        module procedure find1
        module procedure find2
        module procedure find3
        module procedure find4
        module procedure find5
        module procedure find6
        module procedure find7
        module procedure find8
        module procedure find9
        module procedure find10
        module procedure find12
        module procedure find13
        module procedure find14
        module procedure find15
        module procedure find23
        module procedure find26
        module procedure find27
        module procedure find28
        module procedure find31
        module procedure find32
        module procedure find33
        module procedure find34
        module procedure find35
        module procedure find40
    end interface

    interface del
        module procedure del1
        module procedure del2
        module procedure del3
        module procedure del4
        module procedure del5
        module procedure del6
        module procedure del7
        module procedure del8
        module procedure del9
        module procedure del10
        module procedure del12
        module procedure del13
        module procedure del14
        module procedure del15
        module procedure del23
        module procedure del26
        module procedure del27
        module procedure del28
        module procedure del31
        module procedure del32
        module procedure del33
        module procedure del34
        module procedure del35
        module procedure del40
    end interface

    interface cnt
        module procedure cnt1
        module procedure cnt2
        module procedure cnt3
        module procedure cnt4
        module procedure cnt5
        module procedure cnt6
        module procedure cnt7
        module procedure cnt8
        module procedure cnt9
        module procedure cnt10
        module procedure cnt12
        module procedure cnt13
        module procedure cnt14
        module procedure cnt15
        module procedure cnt23
        module procedure cnt26
        module procedure cnt27
        module procedure cnt28
        module procedure cnt31
        module procedure cnt32
        module procedure cnt33
        module procedure cnt34
        module procedure cnt35
        module procedure cnt40
    end interface

    public :: pop, put, find, del, cnt

contains

    !=========================================================================

    function find1(hed, mt)

        implicit none

        type(mf_1), pointer :: find1
        type(mf_1), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_1), pointer :: mf

        nullify (find1)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find1 => mf
            end if
            exit
        end do

        return
    end function find1

    !-------------------------------------------------------------------------

    function find2(hed, mt)

        implicit none

        type(mf_2), pointer :: find2
        type(mf_2), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        nullify (find2)
        if (mt /= 151) return
        find2 => hed

        return
    end function find2

    !-------------------------------------------------------------------------

    function find3(hed, mt)

        implicit none

        type(mf_3), pointer :: find3
        type(mf_3), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_3), pointer :: mf

        nullify (find3)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find3 => mf
            end if
            exit
        end do

        return
    end function find3

    !-------------------------------------------------------------------------

    function find4(hed, mt)

        implicit none

        type(mf_4), pointer :: find4
        type(mf_4), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_4), pointer :: mf

        nullify (find4)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find4 => mf
            end if
            exit
        end do

        return
    end function find4

    !-------------------------------------------------------------------------

    function find5(hed, mt)

        implicit none

        type(mf_5), pointer :: find5
        type(mf_5), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_5), pointer :: mf

        nullify (find5)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find5 => mf
            end if
            exit
        end do

        return
    end function find5

    !-------------------------------------------------------------------------

    function find6(hed, mt)

        implicit none

        type(mf_6), pointer :: find6
        type(mf_6), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_6), pointer :: mf

        nullify (find6)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find6 => mf
            end if
            exit
        end do

        return
    end function find6

    !-------------------------------------------------------------------------

    function find7(hed, mt)

        implicit none

        type(mf_7), pointer :: find7
        type(mf_7), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_7), pointer :: mf

        nullify (find7)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find7 => mf
            end if
            exit
        end do

        return
    end function find7

    !-------------------------------------------------------------------------

    function find8(hed, mt)

        implicit none

        type(mf_8), pointer :: find8
        type(mf_8), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_8), pointer :: mf

        nullify (find8)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find8 => mf
            end if
            exit
        end do

        return
    end function find8

    !-------------------------------------------------------------------------

    function find9(hed, mt)

        implicit none

        type(mf_9), pointer :: find9
        type(mf_9), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_9), pointer :: mf

        nullify (find9)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find9 => mf
            end if
            exit
        end do

        return
    end function find9

    !-------------------------------------------------------------------------

    function find10(hed, mt)

        implicit none

        type(mf_10), pointer :: find10
        type(mf_10), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_10), pointer :: mf

        nullify (find10)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find10 => mf
            end if
            exit
        end do

        return
    end function find10

    !-------------------------------------------------------------------------

    function find12(hed, mt)

        implicit none

        type(mf_12), pointer :: find12
        type(mf_12), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_12), pointer :: mf

        nullify (find12)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find12 => mf
            end if
            exit
        end do

        return
    end function find12

    !-------------------------------------------------------------------------

    function find13(hed, mt)

        implicit none

        type(mf_13), pointer :: find13
        type(mf_13), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_13), pointer :: mf

        nullify (find13)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find13 => mf
            end if
            exit
        end do

        return
    end function find13

    !-------------------------------------------------------------------------

    function find14(hed, mt)

        implicit none

        type(mf_14), pointer :: find14
        type(mf_14), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_14), pointer :: mf

        nullify (find14)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find14 => mf
            end if
            exit
        end do

        return
    end function find14

    !-------------------------------------------------------------------------

    function find15(hed, mt)

        implicit none

        type(mf_15), pointer :: find15
        type(mf_15), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_15), pointer :: mf

        nullify (find15)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find15 => mf
            end if
            exit
        end do

        return
    end function find15

    !-------------------------------------------------------------------------

    function find23(hed, mt)

        implicit none

        type(mf_23), pointer :: find23
        type(mf_23), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_23), pointer :: mf

        nullify (find23)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find23 => mf
            end if
            exit
        end do

        return
    end function find23

    !-------------------------------------------------------------------------

    function find26(hed, mt)

        implicit none

        type(mf_26), pointer :: find26
        type(mf_26), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_26), pointer :: mf

        nullify (find26)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find26 => mf
            end if
            exit
        end do

        return
    end function find26

    !-------------------------------------------------------------------------

    function find27(hed, mt)

        implicit none

        type(mf_27), pointer :: find27
        type(mf_27), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_27), pointer :: mf

        nullify (find27)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find27 => mf
            end if
            exit
        end do

        return
    end function find27

    !-------------------------------------------------------------------------

    function find28(hed, mt)

        implicit none

        type(mf_28), pointer :: find28
        type(mf_28), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_28), pointer :: mf

        nullify (find28)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find28 => mf
            end if
            exit
        end do

        return
    end function find28

    !-------------------------------------------------------------------------

    function find31(hed, mt)

        implicit none

        type(mf_31), pointer :: find31
        type(mf_31), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_31), pointer :: mf

        nullify (find31)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find31 => mf
            end if
            exit
        end do

        return
    end function find31

    !-------------------------------------------------------------------------

    function find32(hed, mt)

        implicit none

        type(mf_32), pointer :: find32
        type(mf_32), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        nullify (find32)

        nullify (find32)
        if (mt /= 151) return
        find32 => hed

        return
    end function find32

    !-------------------------------------------------------------------------

    function find33(hed, mt)

        implicit none

        type(mf_33), pointer :: find33
        type(mf_33), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_33), pointer :: mf

        nullify (find33)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find33 => mf
            end if
            exit
        end do

        return
    end function find33

    !-------------------------------------------------------------------------

    function find34(hed, mt)

        implicit none

        type(mf_34), pointer :: find34
        type(mf_34), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_34), pointer :: mf

        nullify (find34)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find34 => mf
            end if
            exit
        end do

        return
    end function find34

    !-------------------------------------------------------------------------

    function find35(hed, mt)

        implicit none

        type(mf_35), pointer :: find35
        type(mf_35), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_35), pointer :: mf

        nullify (find35)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find35 => mf
            end if
            exit
        end do

        return
    end function find35

    !-------------------------------------------------------------------------

    function find40(hed, mt)

        implicit none

        type(mf_40), pointer :: find40
        type(mf_40), intent(in), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_40), pointer :: mf

        nullify (find40)

        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                mf => mf%next
                cycle
            else if (mf%mt == mt) then
                find40 => mf
            end if
            exit
        end do

        return
    end function find40

    !=========================================================================

    function pop1(hed, mt)

        implicit none

        type(mf_1), pointer :: pop1
        type(mf_1), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_1), pointer :: lm, mf

        nullify (pop1)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop1 => mf

        return

    end function pop1

    !-------------------------------------------------------------------------

    function pop2(hed, mt)

        implicit none

        type(mf_2), pointer :: pop2
        type(mf_2), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        ! MF2 is special : only 1 MT = 151

        nullify (pop2)
        if (mt /= 151) return
        if (.not. associated(hed)) return
        pop2 => hed
        nullify (hed)

        return

    end function pop2

    !-------------------------------------------------------------------------

    function pop3(hed, mt)

        implicit none

        type(mf_3), pointer :: pop3
        type(mf_3), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_3), pointer :: lm, mf

        nullify (pop3)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop3 => mf

        return

    end function pop3

    !-------------------------------------------------------------------------

    function pop4(hed, mt)

        implicit none

        type(mf_4), pointer :: pop4
        type(mf_4), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_4), pointer :: lm, mf

        nullify (pop4)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop4 => mf

        return

    end function pop4

    !-------------------------------------------------------------------------

    function pop5(hed, mt)

        implicit none

        type(mf_5), pointer :: pop5
        type(mf_5), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_5), pointer :: lm, mf

        nullify (pop5)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop5 => mf

        return

    end function pop5

    !-------------------------------------------------------------------------

    function pop6(hed, mt)

        implicit none

        type(mf_6), pointer :: pop6
        type(mf_6), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_6), pointer :: lm, mf

        nullify (pop6)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop6 => mf

        return

    end function pop6

    !-------------------------------------------------------------------------

    function pop7(hed, mt)

        implicit none

        type(mf_7), pointer :: pop7
        type(mf_7), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_7), pointer :: lm, mf

        nullify (pop7)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop7 => mf

        return

    end function pop7

    !-------------------------------------------------------------------------

    function pop8(hed, mt)

        implicit none

        type(mf_8), pointer :: pop8
        type(mf_8), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_8), pointer :: lm, mf

        nullify (pop8)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop8 => mf

        return

    end function pop8

    !-------------------------------------------------------------------------

    function pop9(hed, mt)

        implicit none

        type(mf_9), pointer :: pop9
        type(mf_9), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_9), pointer :: lm, mf

        nullify (pop9)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop9 => mf

        return

    end function pop9

    !-------------------------------------------------------------------------

    function pop10(hed, mt)

        implicit none

        type(mf_10), pointer :: pop10
        type(mf_10), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_10), pointer :: lm, mf

        nullify (pop10)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop10 => mf

        return

    end function pop10

    !-------------------------------------------------------------------------

    function pop12(hed, mt)

        implicit none

        type(mf_12), pointer :: pop12
        type(mf_12), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_12), pointer :: lm, mf

        nullify (pop12)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop12 => mf

        return

    end function pop12

    !-------------------------------------------------------------------------

    function pop13(hed, mt)

        implicit none

        type(mf_13), pointer :: pop13
        type(mf_13), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_13), pointer :: lm, mf

        nullify (pop13)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop13 => mf

        return

    end function pop13

    !-------------------------------------------------------------------------

    function pop14(hed, mt)

        implicit none

        type(mf_14), pointer :: pop14
        type(mf_14), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_14), pointer :: lm, mf

        nullify (pop14)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop14 => mf

        return

    end function pop14

    !-------------------------------------------------------------------------

    function pop15(hed, mt)

        implicit none

        type(mf_15), pointer :: pop15
        type(mf_15), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_15), pointer :: lm, mf

        nullify (pop15)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop15 => mf

        return

    end function pop15

    !-------------------------------------------------------------------------

    function pop23(hed, mt)

        implicit none

        type(mf_23), pointer :: pop23
        type(mf_23), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_23), pointer :: lm, mf

        nullify (pop23)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop23 => mf

        return

    end function pop23

    !-------------------------------------------------------------------------

    function pop26(hed, mt)

        implicit none

        type(mf_26), pointer :: pop26
        type(mf_26), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_26), pointer :: lm, mf

        nullify (pop26)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop26 => mf

        return

    end function pop26

    !-------------------------------------------------------------------------

    function pop27(hed, mt)

        implicit none

        type(mf_27), pointer :: pop27
        type(mf_27), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_27), pointer :: lm, mf

        nullify (pop27)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop27 => mf

        return

    end function pop27

    !-------------------------------------------------------------------------

    function pop28(hed, mt)

        implicit none

        type(mf_28), pointer :: pop28
        type(mf_28), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_28), pointer :: lm, mf

        nullify (pop28)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop28 => mf

        return

    end function pop28

    !-------------------------------------------------------------------------

    function pop31(hed, mt)

        implicit none

        type(mf_31), pointer :: pop31
        type(mf_31), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_31), pointer :: lm, mf

        nullify (pop31)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop31 => mf

        return

    end function pop31

    !-------------------------------------------------------------------------

    function pop32(hed, mt)

        implicit none

        type(mf_32), pointer :: pop32
        type(mf_32), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        ! MF32 is special : only 1 MT = 151

        nullify (pop32)
        if (mt /= 151) return
        if (.not. associated(hed)) return
        pop32 => hed
        nullify (hed)

        return

    end function pop32

    !-------------------------------------------------------------------------

    function pop33(hed, mt)

        implicit none

        type(mf_33), pointer :: pop33
        type(mf_33), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_33), pointer :: lm, mf

        nullify (pop33)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop33 => mf

        return

    end function pop33

    !-------------------------------------------------------------------------

    function pop34(hed, mt)

        implicit none

        type(mf_34), pointer :: pop34
        type(mf_34), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_34), pointer :: lm, mf

        nullify (pop34)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop34 => mf

        return

    end function pop34

    !-------------------------------------------------------------------------

    function pop35(hed, mt)

        implicit none

        type(mf_35), pointer :: pop35
        type(mf_35), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_35), pointer :: lm, mf

        nullify (pop35)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop35 => mf

        return

    end function pop35

    !-------------------------------------------------------------------------

    function pop40(hed, mt)

        implicit none

        type(mf_40), pointer :: pop40
        type(mf_40), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt

        type(mf_40), pointer :: lm, mf

        nullify (pop40)
        if (.not. associated(hed)) return

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mt) then
                exit
            else
                return
            end if
        end do

        if (associated(lm)) then
            lm%next => mf%next
        else
            hed => mf%next
        end if

        nullify (mf%next)
        pop40 => mf

        return

    end function pop40

    !=========================================================================

    subroutine del1(hed, mt, qstat)

        implicit none

        type(mf_1), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_1), pointer :: mf

        mf = pop1(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf1(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del1

    !-------------------------------------------------------------------------

    subroutine del2(hed, mt, qstat)

        implicit none

        type(mf_2), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_2), pointer :: mf

        mf = pop2(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf2(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del2

    !-------------------------------------------------------------------------

    subroutine del3(hed, mt, qstat)

        implicit none

        type(mf_3), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_3), pointer :: mf

        mf = pop3(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf3(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del3

    !-------------------------------------------------------------------------

    subroutine del4(hed, mt, qstat)

        implicit none

        type(mf_4), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_4), pointer :: mf

        mf = pop4(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf4(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del4

    !-------------------------------------------------------------------------

    subroutine del5(hed, mt, qstat)

        implicit none

        type(mf_5), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_5), pointer :: mf

        mf = pop5(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf5(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del5

    !-------------------------------------------------------------------------

    subroutine del6(hed, mt, qstat)

        implicit none

        type(mf_6), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_6), pointer :: mf

        mf = pop6(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf6(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del6

    !-------------------------------------------------------------------------

    subroutine del7(hed, mt, qstat)

        implicit none

        type(mf_7), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_7), pointer :: mf

        mf = pop7(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf7(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del7

    !-------------------------------------------------------------------------

    subroutine del8(hed, mt, qstat)

        implicit none

        type(mf_8), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_8), pointer :: mf

        mf = pop8(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf8(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del8

    !-------------------------------------------------------------------------

    subroutine del9(hed, mt, qstat)

        implicit none

        type(mf_9), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_9), pointer :: mf

        mf = pop9(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf9(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del9

    !-------------------------------------------------------------------------

    subroutine del10(hed, mt, qstat)

        implicit none

        type(mf_10), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_10), pointer :: mf

        mf = pop10(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf10(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del10

    !-------------------------------------------------------------------------

    subroutine del12(hed, mt, qstat)

        implicit none

        type(mf_12), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_12), pointer :: mf

        mf = pop12(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf12(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del12

    !-------------------------------------------------------------------------

    subroutine del13(hed, mt, qstat)

        implicit none

        type(mf_13), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_13), pointer :: mf

        mf = pop13(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf13(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del13

    !-------------------------------------------------------------------------

    subroutine del14(hed, mt, qstat)

        implicit none

        type(mf_14), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_14), pointer :: mf

        mf = pop14(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf14(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del14

    !-------------------------------------------------------------------------

    subroutine del15(hed, mt, qstat)

        implicit none

        type(mf_15), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_15), pointer :: mf

        mf = pop15(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf15(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del15

    !-------------------------------------------------------------------------

    subroutine del23(hed, mt, qstat)

        implicit none

        type(mf_23), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_23), pointer :: mf

        mf = pop23(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf23(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del23

    !-------------------------------------------------------------------------

    subroutine del26(hed, mt, qstat)

        implicit none

        type(mf_26), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_26), pointer :: mf

        mf = pop26(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf26(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del26

    !-------------------------------------------------------------------------

    subroutine del27(hed, mt, qstat)

        implicit none

        type(mf_27), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_27), pointer :: mf

        mf = pop27(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf27(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del27

    !-------------------------------------------------------------------------

    subroutine del28(hed, mt, qstat)

        implicit none

        type(mf_28), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_28), pointer :: mf

        mf = pop28(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf28(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del28

    !-------------------------------------------------------------------------

    subroutine del31(hed, mt, qstat)

        implicit none

        type(mf_31), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_31), pointer :: mf

        mf = pop31(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf31(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del31

    !-------------------------------------------------------------------------

    subroutine del32(hed, mt, qstat)

        implicit none

        type(mf_32), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_32), pointer :: mf

        mf = pop32(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf32(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del32

    !-------------------------------------------------------------------------

    subroutine del33(hed, mt, qstat)

        implicit none

        type(mf_33), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_33), pointer :: mf

        mf = pop33(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf33(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del33

    !-------------------------------------------------------------------------

    subroutine del34(hed, mt, qstat)

        implicit none

        type(mf_34), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_34), pointer :: mf

        mf = pop34(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf34(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del34

    !-------------------------------------------------------------------------

    subroutine del35(hed, mt, qstat)

        implicit none

        type(mf_35), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_35), pointer :: mf

        mf = pop35(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf35(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del35

    !-------------------------------------------------------------------------

    subroutine del40(hed, mt, qstat)

        implicit none

        type(mf_40), intent(inout), pointer :: hed
        integer*4, intent(in) :: mt
        logical*4, intent(out), optional :: qstat

        logical*4 qs
        type(mf_40), pointer :: mf

        mf = pop40(hed, mt)
        qs = associated(mf)
        if (qs) call del_mf40(mf)
        if (present(qstat)) qstat = qs

        return
    end subroutine del40

    !=========================================================================

    logical*4 function put1(hed, mx)

        implicit none

        type(mf_1), intent(inout), pointer :: hed
        type(mf_1), intent(inout), target :: mx

        type(mf_1), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put1 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put1 = .true.

        return
    end function put1

    !-------------------------------------------------------------------------

    logical*4 function put2(hed, mx)

        implicit none

        type(mf_2), intent(inout), pointer :: hed
        type(mf_2), intent(inout), target :: mx

        ! MF2 is special : only 1 MT = 151

        put2 = .false.
        if (mx%mt /= 151) return
        if (associated(hed)) return
        hed => mx
        put2 = .true.

        return
    end function put2

    !-------------------------------------------------------------------------

    logical*4 function put3(hed, mx)

        implicit none

        type(mf_3), intent(inout), pointer :: hed
        type(mf_3), intent(inout), target :: mx

        type(mf_3), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put3 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put3 = .true.

        return
    end function put3

    !-------------------------------------------------------------------------

    logical*4 function put4(hed, mx)

        implicit none

        type(mf_4), intent(inout), pointer :: hed
        type(mf_4), intent(inout), target :: mx

        type(mf_4), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put4 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put4 = .true.

        return
    end function put4

    !-------------------------------------------------------------------------

    logical*4 function put5(hed, mx)

        implicit none

        type(mf_5), intent(inout), pointer :: hed
        type(mf_5), intent(inout), target :: mx

        type(mf_5), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put5 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put5 = .true.

        return
    end function put5

    !-------------------------------------------------------------------------

    logical*4 function put6(hed, mx)

        implicit none

        type(mf_6), intent(inout), pointer :: hed
        type(mf_6), intent(inout), target :: mx

        type(mf_6), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put6 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put6 = .true.

        return
    end function put6

    !-------------------------------------------------------------------------

    logical*4 function put7(hed, mx)

        implicit none

        type(mf_7), intent(inout), pointer :: hed
        type(mf_7), intent(inout), target :: mx

        type(mf_7), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put7 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put7 = .true.

        return
    end function put7

    !-------------------------------------------------------------------------

    logical*4 function put8(hed, mx)

        implicit none

        type(mf_8), intent(inout), pointer :: hed
        type(mf_8), intent(inout), target :: mx

        type(mf_8), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put8 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put8 = .true.

        return
    end function put8

    !-------------------------------------------------------------------------

    logical*4 function put9(hed, mx)

        implicit none

        type(mf_9), intent(inout), pointer :: hed
        type(mf_9), intent(inout), target :: mx

        type(mf_9), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put9 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put9 = .true.

        return
    end function put9

    !-------------------------------------------------------------------------

    logical*4 function put10(hed, mx)

        implicit none

        type(mf_10), intent(inout), pointer :: hed
        type(mf_10), intent(inout), target :: mx

        type(mf_10), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put10 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put10 = .true.

        return
    end function put10

    !-------------------------------------------------------------------------

    logical*4 function put12(hed, mx)

        implicit none

        type(mf_12), intent(inout), pointer :: hed
        type(mf_12), intent(inout), target :: mx

        type(mf_12), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put12 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put12 = .true.

        return
    end function put12

    !-------------------------------------------------------------------------

    logical*4 function put13(hed, mx)

        implicit none

        type(mf_13), intent(inout), pointer :: hed
        type(mf_13), intent(inout), target :: mx

        type(mf_13), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put13 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put13 = .true.

        return
    end function put13

    !-------------------------------------------------------------------------

    logical*4 function put14(hed, mx)

        implicit none

        type(mf_14), intent(inout), pointer :: hed
        type(mf_14), intent(inout), target :: mx

        type(mf_14), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put14 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put14 = .true.

        return
    end function put14

    !-------------------------------------------------------------------------

    logical*4 function put15(hed, mx)

        implicit none

        type(mf_15), intent(inout), pointer :: hed
        type(mf_15), intent(inout), target :: mx

        type(mf_15), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put15 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put15 = .true.

        return
    end function put15

    !-------------------------------------------------------------------------

    logical*4 function put23(hed, mx)

        implicit none

        type(mf_23), intent(inout), pointer :: hed
        type(mf_23), intent(inout), target :: mx

        type(mf_23), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put23 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put23 = .true.

        return
    end function put23

    !-------------------------------------------------------------------------

    logical*4 function put26(hed, mx)

        implicit none

        type(mf_26), intent(inout), pointer :: hed
        type(mf_26), intent(inout), target :: mx

        type(mf_26), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put26 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put26 = .true.

        return
    end function put26

    !-------------------------------------------------------------------------

    logical*4 function put27(hed, mx)

        implicit none

        type(mf_27), intent(inout), pointer :: hed
        type(mf_27), intent(inout), target :: mx

        type(mf_27), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put27 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put27 = .true.

        return
    end function put27

    !-------------------------------------------------------------------------

    logical*4 function put28(hed, mx)

        implicit none

        type(mf_28), intent(inout), pointer :: hed
        type(mf_28), intent(inout), target :: mx

        type(mf_28), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put28 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put28 = .true.

        return
    end function put28

    !-------------------------------------------------------------------------

    logical*4 function put31(hed, mx)

        implicit none

        type(mf_31), intent(inout), pointer :: hed
        type(mf_31), intent(inout), target :: mx

        type(mf_31), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put31 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put31 = .true.

        return
    end function put31

    !-------------------------------------------------------------------------

    logical*4 function put32(hed, mx)

        implicit none

        type(mf_32), intent(inout), pointer :: hed
        type(mf_32), intent(inout), target :: mx

        ! MF32 is special : only 1 MT = 151

        put32 = .false.
        if (mx%mt /= 151) return
        if (associated(hed)) return
        hed => mx
        put32 = .true.

        return
    end function put32

    !-------------------------------------------------------------------------

    logical*4 function put33(hed, mx)

        implicit none

        type(mf_33), intent(inout), pointer :: hed
        type(mf_33), intent(inout), target :: mx

        type(mf_33), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put33 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put33 = .true.

        return
    end function put33

    !-------------------------------------------------------------------------

    logical*4 function put34(hed, mx)

        implicit none

        type(mf_34), intent(inout), pointer :: hed
        type(mf_34), intent(inout), target :: mx

        type(mf_34), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put34 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put34 = .true.

        return
    end function put34

    !-------------------------------------------------------------------------

    logical*4 function put35(hed, mx)

        implicit none

        type(mf_35), intent(inout), pointer :: hed
        type(mf_35), intent(inout), target :: mx

        type(mf_35), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put35 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put35 = .true.

        return
    end function put35

    !-------------------------------------------------------------------------

    logical*4 function put40(hed, mx)

        implicit none

        type(mf_40), intent(inout), pointer :: hed
        type(mf_40), intent(inout), target :: mx

        type(mf_40), pointer :: lm, mf

        nullify (lm)
        mf => hed
        do while (associated(mf))
            if (mf%mt < mx%mt) then
                lm => mf
                mf => mf%next
            else if (mf%mt == mx%mt) then
                put40 = .false.
                return
            else
                exit
            end if
        end do

        mx%next => mf
        if (associated(lm)) then
            lm%next => mx
        else
            hed => mx
        end if

        put40 = .true.

        return
    end function put40

!=========================================================================

    integer*4 function cnt1(hed)

        implicit none

        type(mf_1), intent(inout), pointer :: hed

        type(mf_1), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt1 = n

        return
    end function cnt1

    !-------------------------------------------------------------------------

    integer*4 function cnt2(hed)

        implicit none

        type(mf_2), intent(inout), pointer :: hed

        integer*4 n

        n = 0

        if (associated(hed)) then
            n = 1
        else
            n = 0
        end if

        cnt2 = n

        return
    end function cnt2

    !-------------------------------------------------------------------------

    integer*4 function cnt3(hed)

        implicit none

        type(mf_3), intent(inout), pointer :: hed

        type(mf_3), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt3 = n

        return
    end function cnt3

    !-------------------------------------------------------------------------

    integer*4 function cnt4(hed)

        implicit none

        type(mf_4), intent(inout), pointer :: hed

        type(mf_4), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt4 = n

        return
    end function cnt4

    !-------------------------------------------------------------------------

    integer*4 function cnt5(hed)

        implicit none

        type(mf_5), intent(inout), pointer :: hed

        type(mf_5), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt5 = n

        return
    end function cnt5

    !-------------------------------------------------------------------------

    integer*4 function cnt6(hed)

        implicit none

        type(mf_6), intent(inout), pointer :: hed

        type(mf_6), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt6 = n

        return
    end function cnt6

    !-------------------------------------------------------------------------

    integer*4 function cnt7(hed)

        implicit none

        type(mf_7), intent(inout), pointer :: hed

        type(mf_7), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt7 = n

        return
    end function cnt7

    !-------------------------------------------------------------------------

    integer*4 function cnt8(hed)

        implicit none

        type(mf_8), intent(inout), pointer :: hed

        type(mf_8), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt8 = n

        return
    end function cnt8

    !-------------------------------------------------------------------------

    integer*4 function cnt9(hed)

        implicit none

        type(mf_9), intent(inout), pointer :: hed

        type(mf_9), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt9 = n

        return
    end function cnt9

    !-------------------------------------------------------------------------

    integer*4 function cnt10(hed)

        implicit none

        type(mf_10), intent(inout), pointer :: hed

        type(mf_10), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt10 = n

        return
    end function cnt10

    !-------------------------------------------------------------------------

    integer*4 function cnt12(hed)

        implicit none

        type(mf_12), intent(inout), pointer :: hed

        type(mf_12), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt12 = n

        return
    end function cnt12

    !-------------------------------------------------------------------------

    integer*4 function cnt13(hed)

        implicit none

        type(mf_13), intent(inout), pointer :: hed

        type(mf_13), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt13 = n

        return
    end function cnt13

    !-------------------------------------------------------------------------

    integer*4 function cnt14(hed)

        implicit none

        type(mf_14), intent(inout), pointer :: hed

        type(mf_14), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt14 = n

        return
    end function cnt14

    !-------------------------------------------------------------------------

    integer*4 function cnt15(hed)

        implicit none

        type(mf_15), intent(inout), pointer :: hed

        type(mf_15), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt15 = n

        return
    end function cnt15

    !-------------------------------------------------------------------------

    integer*4 function cnt23(hed)

        implicit none

        type(mf_23), intent(inout), pointer :: hed

        type(mf_23), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt23 = n

        return
    end function cnt23

    !-------------------------------------------------------------------------

    integer*4 function cnt26(hed)

        implicit none

        type(mf_26), intent(inout), pointer :: hed

        type(mf_26), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt26 = n

        return
    end function cnt26

    !-------------------------------------------------------------------------

    integer*4 function cnt27(hed)

        implicit none

        type(mf_27), intent(inout), pointer :: hed

        type(mf_27), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt27 = n

        return
    end function cnt27

    !-------------------------------------------------------------------------

    integer*4 function cnt28(hed)

        implicit none

        type(mf_28), intent(inout), pointer :: hed

        type(mf_28), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt28 = n

        return
    end function cnt28

    !-------------------------------------------------------------------------

    integer*4 function cnt31(hed)

        implicit none

        type(mf_31), intent(inout), pointer :: hed

        type(mf_31), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt31 = n

        return
    end function cnt31

    !-------------------------------------------------------------------------

    integer*4 function cnt32(hed)

        implicit none

        type(mf_32), intent(inout), pointer :: hed

        integer*4 n

        n = 0

        if (associated(hed)) then
            n = 1
        else
            n = 0
        end if

        cnt32 = n

        return
    end function cnt32

    !-------------------------------------------------------------------------

    integer*4 function cnt33(hed)

        implicit none

        type(mf_33), intent(inout), pointer :: hed

        type(mf_33), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt33 = n

        return
    end function cnt33

    !-------------------------------------------------------------------------

    integer*4 function cnt34(hed)

        implicit none

        type(mf_34), intent(inout), pointer :: hed

        type(mf_34), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt34 = n

        return
    end function cnt34

    !-------------------------------------------------------------------------

    integer*4 function cnt35(hed)

        implicit none

        type(mf_35), intent(inout), pointer :: hed

        type(mf_35), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt35 = n

        return
    end function cnt35

    !-------------------------------------------------------------------------

    integer*4 function cnt40(hed)

        implicit none

        type(mf_40), intent(inout), pointer :: hed

        type(mf_40), pointer :: mf
        integer*4 n

        n = 0

        mf => hed
        do while (associated(mf))
            n = n + 1
            mf => mf%next
        end do

        cnt40 = n

        return
    end function cnt40

end module endf_queue
