! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/pi_MC.c
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/pi_MC_integral.c

module pi_MC_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: intrinsic_prng_initializer_lib

    implicit none

    private
    public  :: pi_MC
    public  :: pi_MC_integral



    contains



    pure elemental function is_in(x, y)

        !> 本 FUNCTION の仮引数
        !> サンプル点の x 座標
        real(real64), intent(in) :: x

        !> 本 FUNCTION の仮引数
        !> サンプル点の y 座標
        real(real64), intent(in) :: y

        !> 本 FUNCTION の戻り値
        !> 原点を中心とする半径 1 の円の内点であれば `.true.` を返す
        logical :: is_in

        is_in = ( (x * x + y * y) .lt. 1.0_real64 )

    end function is_in



    subroutine pi_MC(num_samples_total, seed, pi_quarter)

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法のサンプル点数
        integer(int32), intent(in) :: num_samples_total

        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: seed

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法による \pi/4 の近似値
        real(real64), intent(out) :: pi_quarter



        !> 本 SUBROUTINE 用の変数
        !> 扇形の内点となったサンプル点数
        integer(int32) :: num_samples_in

        !> 本 SUBROUTINE 用の変数
        !> `RANDOM_SEED` 文用の変数
        type(intrinsic_prng_initializer_type) :: intrinsic_prng_initializer



        call intrinsic_prng_initializer%put(seed)

        num_samples_in = 0_int32

        block

            !> 本 BLOCK 用の変数
            !> サンプル点の x 座標
            real(real64) :: x

            !> 本 BLOCK 用の変数
            !> サンプル点の y 座標
            real(real64) :: y

            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter

            do iter = 1_int32, num_samples_total

                call random_number(x)
                call random_number(y)

                if ( is_in(x, y) ) then
                    num_samples_in = &!
                    num_samples_in + 1_int32
                end if

            end do

        end block

        pi_quarter = real(num_samples_in, real64) / num_samples_total

    end subroutine pi_MC



    subroutine pi_MC_integral(num_samples, seed, pi_quarter)

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法のサンプル点数
        integer(int32), intent(in) :: num_samples

        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: seed

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法による \pi/4 の近似値
        real(real64), intent(out) :: pi_quarter

        !> 本 SUBROUTINE 用の変数
        !> `RANDOM_SEED` 文用の変数
        type(intrinsic_prng_initializer_type) :: intrinsic_prng_initializer



        call intrinsic_prng_initializer%put(seed)

        block

            !> 本 BLOCK 用の変数
            !> サンプル点の x 座標
            real(real64) :: x

            !> 本 BLOCK 用の変数
            !> サンプル点の y 座標
            real(real64) :: y

            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter

            do iter = 1_int32, num_samples

                call random_number(x)

                y          = sqrt(1.0_real64 - x * x)
                pi_quarter = pi_quarter + y

            end do

        end block

        pi_quarter = &!
        pi_quarter / num_samples

    end subroutine pi_MC_integral

end module pi_MC_lib
