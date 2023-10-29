module jackknife_statistics_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: math_constants_lib
    use, non_intrinsic :: statistics_lib

    implicit none

    private
    public  :: eval_block_average
    public  :: jackknife_average
    public  :: jackknife_error



    contains



    pure function eval_block_average(array, block_size, num_blocks) result(block_average)

        !> 本 SUBROUTINE の仮引数
        !> 各ブロックでの平均値を求めるデータ
        real(real64), dimension(:), intent(in) :: array

        !> 本 SUBROUTINE の仮引数
        !> 各ブロックのサイズ
        integer(int32), intent(in) :: block_size

        !> 本 SUBROUTINE の仮引数
        !> ブロック数
        integer(int32), intent(in) :: num_blocks



        !> 本 FUNCTION の戻り値
        real(real64), dimension(num_blocks) :: block_average



        !> 本 BLOCK 用の補助変数
        integer(int32) :: iter_block



        do concurrent (iter_block = 1_int32 : num_blocks)

            associate( minloc_block => (iter_block - 1_int32) * block_size + 1_int32 )
            associate( maxloc_block =>  iter_block            * block_size           )

                block_average(iter_block) = average( array( minloc_block : maxloc_block ) )

            end associate
            end associate

        end do

    end function eval_block_average



    pure function jackknife_average(array, block_size)

        !> 本 FUNCTION の仮引数
        !> jack knife error を計算するデータ
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        !> 各データブロックのサイズ
        integer(int32), intent(in) :: block_size



        !> 本 FUNCTION の戻り値
        real(real64) :: jackknife_average



        !> 本 FUNCTION 用の変数
        !> `array` を同じ要素数の block に分割する際、
        !> 利用可能なデータ数
        integer(int32) :: array_size_available

        !> 本 FUNCTION 用の変数
        !> 総データ数
        integer(int32) :: array_size_whole

        !> 本 FUNCTION 用の変数
        !> データブロックのの個数
        integer(int32) :: num_blocks



        if ( block_size .lt. 1_int32 ) then
            jackknife_average = QUIET_NAN_REAL64
            return
        end if



        array_size_whole     = size( array(:) ) 
        array_size_available = array_size_whole - mod( a=array_size_whole, p=block_size )



        if ( array_size_available .lt. 1_int32 ) then
            jackknife_average = QUIET_NAN_REAL64
            return
        end if



        ! データブロックの個数の算出
        num_blocks = array_size_available / block_size



        block

            !> 本 BLOCK 用の変数
            !> 各 block でのデータに基づく平均値
            real(real64), dimension(num_blocks) :: block_average

            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter_block



            ! 各 block での平均値の算出
            do concurrent (iter_block = 1_int32 : num_blocks)

                associate( minloc_block => (iter_block - 1_int32) * block_size + 1_int32 )
                associate( maxloc_block =>  iter_block            * block_size           )

                    block_average(iter_block) = average( array( minloc_block : maxloc_block ) )

                end associate
                end associate

            end do



            jackknife_average = average( block_average(:) )

        end block

    end function jackknife_average



    pure function jackknife_error(array, block_size)

        !> 本 FUNCTION の仮引数
        !> jack knife error を計算するデータ
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        !> 各データブロックのサイズ
        integer(int32), intent(in) :: block_size



        !> 本 FUNCTION の戻り値
        real(real64) :: jackknife_error



        !> 本 FUNCTION 用の変数
        !> `array` を同じ要素数の block に分割する際、
        !> 利用可能なデータ数
        integer(int32) :: array_size_available

        !> 本 FUNCTION 用の変数
        !> 総データ数
        integer(int32) :: array_size_whole

        !> 本 FUNCTION 用の変数
        !> データブロックのの個数
        integer(int32) :: num_blocks

        !> 本 FUNCTION 用の変数
        !> 全データに基づく平均値
        real(real64) :: whole_average



        if ( block_size .lt. 1_int32 ) then
            jackknife_error = QUIET_NAN_REAL64
            return
        end if



        array_size_whole     = size( array(:) ) 
        array_size_available = array_size_whole - mod( a=array_size_whole, p=block_size )



        if ( array_size_available .lt. 1_int32 ) then
            jackknife_error = QUIET_NAN_REAL64
            return
        end if



        ! データブロックの個数の算出
        num_blocks = array_size_available / block_size



        block

            !> 本 BLOCK 用の変数
            !> 各 block でのデータに基づく平均値
            real(real64), dimension(num_blocks) :: block_average



            ! 全データに基づく平均値の算出
            whole_average = average( array(:array_size_available) )



            ! 各 block での平均値の算出
            block_average(:) = &!
            &   eval_block_average( &!
            &       array      = array(:array_size_available) , &!
            &       block_size = block_size                   , &!
            &       num_blocks = num_blocks                     &!
            &   )



            jackknife_error = sqrt( average( ( block_average(:) - whole_average ) ** 2 ) / (num_blocks - 1_int32) )

        end block

    end function jackknife_error

end module jackknife_statistics_lib
