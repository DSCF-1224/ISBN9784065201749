module MC_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: intrinsic_prng_initializer_lib

    use, non_intrinsic :: math_constants_lib, only: MATH_1_SQRT2PI

    implicit none



    contains



    pure elemental function gaussian(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: gaussian

        gaussian = MATH_1_SQRT2PI * exp( - 0.5_real64 * x * x )

    end function gaussian




    subroutine MC_integral(num_samples, half_value_width, seed, integral)

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法のサンプル点数
        integer(int32), intent(in) :: num_samples

        !> 本 SUBROUTINE の仮引数
        !> 積分区間の半値幅
        real(real64), intent(in) :: half_value_width

        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: seed

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法で得られた積分値
        real(real64), intent(out) :: integral



        !> 本 SUBROUTINE 用の変数
        !> サンプル点での Gauss 関数の戻り値の総和
        real(real64) :: sum_gaussian

        !> 本 SUBROUTINE 用の変数
        !> `RANDOM_SEED` 文用の変数
        type(intrinsic_prng_initializer_type) :: intrinsic_prng_initializer



        ! 擬似乱数生成器の初期化
        call intrinsic_prng_initializer%put(seed)

        ! 変数の初期化
        sum_gaussian = 0.0_real64

        block

            !> 本 BLOCK 用の変数
            !> サンプル点の x 座標
            real(real64) :: x

            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter

            do iter = 1_int32, num_samples

                call random_number(x)

                x = (2.0_real64 * x - 1.0_real64) * half_value_width

                sum_gaussian = &!
                sum_gaussian + gaussian(x)

            end do

        end block

        integral = sum_gaussian / num_samples * (2.0_real64 * half_value_width)

    end subroutine MC_integral

end module MC_lib
