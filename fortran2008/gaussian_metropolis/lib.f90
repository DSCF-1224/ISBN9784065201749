! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

module gaussian_metropolis_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: intrinsic_prng_initializer_lib



    implicit none



    contains



    pure elemental function action(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        action = 0.5_real64 * x * x

    end function action



    subroutine exe_gaussian_metropolis(seed, num_samples_required, step_size, step_center, num_samples_total, generated_samples)

        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: seed

        !> 本 SUBROUTINE の仮引数
        !> 必要な（受理された）サンプル数
        integer(int32), intent(in) :: num_samples_required

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップ幅
        real(real64), intent(in) :: step_size

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップの中点
        real(real64), intent(in) :: step_center

        !> 本 SUBROUTINE の仮引数
        !> 受理されなかったものも含めたサンプル数
        integer(int32), intent(out) :: num_samples_total

        !> 本 SUBROUTINE の仮引数
        !> 受理された（生成された）サンプル
        real(real64), dimension(num_samples_required), intent(inout) :: generated_samples



        !> 本 SUBROUTINE 用の変数
        !> 作用の出力値の保持用
        real(real64) :: action_ini

        !> 本 SUBROUTINE 用の変数
        !> 作用の出力値の保持用
        real(real64) :: action_fin

        !> 本 SUBROUTINE 用の変数
        !> Metropolis Test 用
        real(real64) :: metropolis

        !> 本 SUBROUTINE 用の変数
        !> サンプルの変化量の保持用
        real(real64) :: sample_delta

        !> 本 SUBROUTINE 用の変数
        !> `RANDOM_SEED` 文用の変数
        type(intrinsic_prng_initializer_type) :: intrinsic_prng_initializer



        !> 本 SUBROUTINE 用の補助変数
        integer(int32) :: num_samples_accepted



        ! 擬似乱数生成器の初期化
        call intrinsic_prng_initializer%put(seed)



        ! 本 SUBROUTINE の出力値の初期化

        num_samples_accepted = 1_int32
        num_samples_total    = 0_int32

        generated_samples(num_samples_accepted) = 0.0_real64



        loop_accepted_sample: &!
        do

            associate( sample_backup => generated_samples( num_samples_accepted     ) )
            associate( sample_new    => generated_samples( num_samples_accepted + 1 ) )

                ! 受理済みのサンプルに対する作用を計算

                action_ini = action(sample_backup)



                loop_metropolis_test: &!
                do

                    ! 受理済みのサンプルからの変化量候補を用意

                    call random_number(sample_delta)

                    sample_delta = (sample_delta - 0.5_real64) * 2.0_real64 * step_size
                    sample_delta = sample_delta + step_center



                    ! 新しいのサンプル候補の作成

                    sample_new        = sample_backup     + sample_delta
                    num_samples_total = num_samples_total + 1_int32



                    ! 新しいのサンプル候補に対する作用を計算

                    action_fin = action(sample_new)



                    ! Metropolis Test

                    call random_number(metropolis)

                    if ( exp(action_ini - action_fin) .gt. metropolis ) then

                        ! 採択された場合

                        num_samples_accepted = &!
                        num_samples_accepted + 1_int32

                        exit loop_metropolis_test

                    else

                        ! 棄却された場合

                        cycle loop_metropolis_test

                    end if

                end do &!
                loop_metropolis_test

            end associate
            end associate



            if ( num_samples_accepted .eq. num_samples_required ) then
                exit loop_accepted_sample
            end if

        end do &!
        loop_accepted_sample

    end subroutine exe_gaussian_metropolis

end module gaussian_metropolis_lib
