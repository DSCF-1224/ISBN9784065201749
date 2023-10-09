! [reference]
! https://stdlib.fortran-lang.org/sourcefile/stdlib_math_arange.fypp.html

module arange_lib

    use, intrinsic :: iso_fortran_env, only: int32

    implicit none



    contains



    pure function arange(start_, end_, step_)

        !> 本 FUNCTION の仮引数
        integer(int32), intent(in) :: start_

        !> 本 FUNCTION の仮引数
        integer(int32), optional, intent(in) :: end_

        !> 本 FUNCTION の仮引数
        integer(int32), optional, intent(in) :: step_



        !> 本 FUNCTION の戻り値
        integer(int32), allocatable, dimension(:) :: arange



        !> 本 FUNCTION 用の変数
        integer(int32) :: finalized_end

        !> 本 FUNCTION 用の変数
        integer(int32) :: finalized_start

        !> 本 FUNCTION 用の変数
        integer(int32) :: tmp_step

        !> 本 FUNCTION 用の変数
        integer(int32) :: finalized_step



        if ( present(end_) ) then
            finalized_start = start_
            finalized_end   = end_
        else
            finalized_start = 1_int32
            finalized_end   = start_
        end if



        tmp_step = merge( step_, 1_int32, present(step_) )

        finalized_step = &!
            sign( &!
                a = merge( tmp_step, 1_int32, (tmp_step .ne. 0_int32) ), &!
                b = (finalized_end - finalized_start) &!
            )



        allocate( arange( (finalized_end - finalized_start) / finalized_step + 1_int32 ) )



        block

            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter

            arange(:) = [(iter, iter=finalized_start, finalized_end, finalized_step)]

        end block

    end function arange



    subroutine allocatable_with_arange(array, start_, end_, step_)

        !> 本 SUBROUTINE の仮引数
        integer(int32), allocatable, dimension(:), intent(inout) :: array

        !> 本 SUBROUTINE の仮引数
        integer(int32), intent(in) :: start_

        !> 本 SUBROUTINE の仮引数
        integer(int32), optional, intent(in) :: end_

        !> 本 SUBROUTINE の仮引数
        integer(int32), optional, intent(in) :: step_



        allocate( &!
            array , &!
            source = arange(start_, end_, step_) &!
        )

    end subroutine allocatable_with_arange

end module arange_lib
