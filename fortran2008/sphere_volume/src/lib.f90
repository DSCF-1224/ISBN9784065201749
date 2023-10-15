! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/three_sphere.c

module sphere_volume_MC_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: intrinsic_prng_initializer_lib
    use, non_intrinsic :: math_constants_lib

    implicit none

    private
    public  :: sphere_volume_MC



    contains



    pure elemental function sphere_surface_area_analytical(dimension_sphere) result(sphere_surface_area)

        !> 本 SUBROUTINE の仮引数
        !> 球の次元
        integer(int32), intent(in) :: dimension_sphere

        !> 本関数の戻り値
        !> 半径 1 の球の体積
        real(real64) :: sphere_surface_area

        sphere_surface_area &!
        &   = 2.0_real64 &!
        &   * (MATH_SQRTPI ** dimension_sphere) &!
        &   / gamma( 0.5_real64 * dimension_sphere )

    end function sphere_surface_area_analytical



    subroutine sphere_volume_MC(dimension_sphere, num_samples_total, seed, num_samples_in, volume)

        !> 本 SUBROUTINE の仮引数
        !> 球の次元
        integer(int32), intent(in) :: dimension_sphere

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法のサンプル点数
        integer(int32), intent(in) :: num_samples_total

        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: seed

        !> 本 SUBROUTINE の仮引数
        !> 扇形の内点となったサンプル点数
        integer(int32), intent(out) :: num_samples_in

        !> 本 SUBROUTINE の仮引数
        !> モンテカルロ法による \pi/4 の近似値
        real(real64), intent(out) :: volume



        !> 本 SUBROUTINE 用の変数
        real(real64) :: sum_z

        !> 本 SUBROUTINE 用の変数
        !> `RANDOM_SEED` 文用の変数
        type(intrinsic_prng_initializer_type) :: intrinsic_prng_initializer



        ! 擬似乱数生成器の初期化

        call intrinsic_prng_initializer%put(seed)



        ! 変数の初期化

        num_samples_in = 0_int32
        sum_z          = 0.0_real64



        block

            !> 本 BLOCK 用の変数
            !> サンプル点の座標
            real(real64), dimension(dimension_sphere - 1_int32) :: x

            !> 本 BLOCK 用の変数
            !> サンプル点の各座標の二乗和
            real(real64) :: dot_product_x



            !> 本 BLOCK 用の補助変数
            integer(int32) :: iter



            do iter = 1_int32, num_samples_total

                call random_number( x(:) )

                dot_product_x = dot_product( x(:), x(:) )

                if ( dot_product_x .lt. 1.0_real64 ) then

                    num_samples_in = &!
                    num_samples_in + 1_int32

                    sum_z = &!
                    sum_z + sqrt( 1.0_real64 - dot_product_x )

                end if

            end do

        end block



        volume = 2.0_real64 / (dimension_sphere - 1_int32) &!
        &      * sphere_surface_area_analytical(dimension_sphere - 1_int32) &!
        &      * (sum_z / num_samples_in)

    end subroutine sphere_volume_MC

end module sphere_volume_MC_lib
