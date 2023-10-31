! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

module gaussian_metropolis_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: metropolis_sampling_lib



    implicit none



    private
    public  :: exe_gaussian_metropolis



    contains



    pure function action(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        action = 0.5_real64 * x * x

    end function action



    subroutine exe_gaussian_metropolis(prng_seed, initial_sample, num_samples_required, step_size, step_center, acceptance_rate, generated_samples)

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
        !> Metropolis 法のステップ幅
        real(real64), intent(in) :: step_size

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップの中点
        real(real64), intent(in) :: step_center

        !> 本 SUBROUTINE の仮引数
        !> 受理されなかったものも含めたサンプル数
        real(real64), dimension(num_samples_required), intent(inout) :: acceptance_rate

        !> 本 SUBROUTINE の仮引数
        !> 受理された（生成された）サンプル
        real(real64), dimension(num_samples_required), intent(inout) :: generated_samples



        call exe_metropolis_sampling( &!
            action               = action               , &!
            prng_seed            = prng_seed            , &!
            initial_sample       = initial_sample       , &!
            num_samples_required = num_samples_required , &!
            step_size            = step_size            , &!
            step_center          = step_center          , &!
            acceptance_rate      = acceptance_rate      , &!
            generated_samples    = generated_samples      &!
        )

    end subroutine exe_gaussian_metropolis

end module gaussian_metropolis_lib
