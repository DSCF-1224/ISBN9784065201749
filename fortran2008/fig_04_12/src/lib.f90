! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

module modified_metropolis_sampling_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: intrinsic_prng_initializer_lib



    implicit none



    private
    public  :: exe_modified_metropolis_sampling



    contains



    subroutine exe_modified_metropolis_sampling(action, prng_seed, initial_sample, num_samples_required, step_size_small, step_size_large, step_center, acceptance_rate, generated_samples)

        !> 本 SUBROUTINE の仮引数
        interface

            pure function action(x)

                use, intrinsic :: iso_fortran_env, only: real64

                !> 本 FUNCTION の仮引数
                real(real64), intent(in) :: x

                !> 本 FUNCTION の戻り値
                real(real64) :: action

            end function action

        end interface



        !> 本 SUBROUTINE の仮引数
        !> 擬似乱数生成器のシード値
        integer(int32), intent(in) :: prng_seed

        !> 本 SUBROUTINE の仮引数
        !> 必要なサンプル数
        integer(int32), intent(in) :: num_samples_required

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法で生成するサンプルの初期値
        real(real64), intent(in) :: initial_sample

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップ幅（小）
        real(real64), intent(in) :: step_size_small

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップ幅（大）
        real(real64), intent(in) :: step_size_large

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップの中点
        real(real64), intent(in) :: step_center

        !> 本 SUBROUTINE の仮引数
        !> 受理されなかったものも含めたサンプル数
        real(real64), dimension(num_samples_required), intent(inout) :: acceptance_rate

        !> 本 SUBROUTINE の仮引数
        !> 受理された（生成された）サンプル
        real(real64), dimension(num_samples_required), intent(inout) :: generated_samples



        !> 本 SUBROUTINE 用の変数
        !> 採択されたサンプル数
        integer(int32) :: num_samples_accepted

        !> 本 SUBROUTINE 用の変数
        !> 作用の出力値の保持用
        real(real64) :: action_now

        !> 本 SUBROUTINE 用の変数
        !> 作用の出力値の保持用
        real(real64) :: action_new

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
        integer(int32) :: iter_sample



        ! 擬似乱数生成器の初期化
        call intrinsic_prng_initializer%put(prng_seed)



        ! Metropolis 法の初期値の指定
        generated_samples    (1) = initial_sample
        acceptance_rate      (1) = 1.0_real64
        num_samples_accepted     = 1_int32



        ! Metropolis 法によるサンプルの生成
        do iter_sample = 2_int32, num_samples_required

            associate( sample_now => generated_samples( iter_sample - 1_int32 ) )
            associate( sample_new => generated_samples( iter_sample           ) )

                ! 受理済みのサンプルに対する作用を計算
    
                action_now = action(sample_now)




                ! 受理済みのサンプルからの
                ! 変化量候補を生成
    
                call random_number(sample_delta)

                associate( flag      => ( mod(a=iter_sample, p=2_int32) .ne. 0_int32 )  )
                associate( step_size => merge( step_size_small, step_size_large, flag ) )
    
                    sample_delta = ( sample_delta - 0.5_real64  ) * 2.0_real64 * step_size
                    sample_delta =   sample_delta + step_center

                end associate
                end associate



                ! 新しいのサンプル候補の生成

                sample_new = sample_now + sample_delta



                ! 新しいのサンプル候補に対する作用を計算

                action_new = action(sample_new)



                ! Metropolis test

                call random_number(metropolis)

                if ( exp(action_now - action_new) .gt. metropolis ) then

                    ! 採択された場合

                    num_samples_accepted = &!
                    num_samples_accepted + 1_int32

                else

                    ! 棄却された場合

                    sample_new = sample_now

                end if



                ! 採択率の計算

                acceptance_rate(iter_sample) = real(num_samples_accepted, real64) / iter_sample

            end associate
            end associate

        end do

    end subroutine exe_modified_metropolis_sampling

end module modified_metropolis_sampling_lib
