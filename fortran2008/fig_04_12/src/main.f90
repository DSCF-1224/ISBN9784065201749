! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: gauss_function_lib               , only: standard_gauss_function
    use, non_intrinsic :: modified_metropolis_sampling_lib
    use, non_intrinsic :: statistics_lib



    implicit none



    !> 本 PROGRAM 用の PARAMETER
    !> Metropolis 法のサンプル点数
    integer(int32), parameter :: num_samples_required_log10 = 7_int32

    !> 本 PROGRAM 用の PARAMETER
    !> Metropolis 法のサンプル点数
    integer(int32), parameter :: num_samples_required = 10_int32 ** num_samples_required_log10



    !> 本 PROGRAM 用の変数
    !> Metropolis 法での採択率の推移
    real(real64), allocatable, dimension(:) :: acceptance_rate

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの平均値の格納用配列
    real(real64), allocatable, dimension(:) :: expectation

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの格納用配列
    real(real64), allocatable, dimension(:) :: sample



    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_sample

    !> 本 PROGRAM 用の補助変数
    integer :: write_unit



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( expectation     (num_samples_required) )
    allocate( sample          (num_samples_required) )



    ! Metropolis 法による
    ! 2つの Gauss 分布を重ね合わせた分布
    ! に従う乱数の生成

    call exe_modified_metropolis_sampling( &!
        action               = action               , &!
        prng_seed            = 1_int32              , &!
        initial_sample       = 0.0_real64           , &!
        num_samples_required = num_samples_required , &!
        step_size_large      = 1.0e2_real64         , &!
        step_size_small      = 1.0e0_real64         , &!
        step_center          = 0.0_real64           , &!
        acceptance_rate      = acceptance_rate(:)   , &!
        generated_samples    = sample(:)              &!
    )



    ! Metropolis 法で生成した乱数の期待値を計算する

    call welford_online_average( sample(:), expectation(:) )




    ! Metropolis 法で生成した乱数の出力

    open( &!
        newunit = write_unit      , &!
        file    = "../sample.dat" , &!
        access  = 'stream'        , &!
        action  = 'write'         , &!
        form    = 'unformatted'   , &!
        status  = 'replace'         &!
    )

    write(unit=write_unit) sample(:)

    close(write_unit)



    open( &!
        newunit = write_unit               , &!
        file    = "../acceptance_rate.dat" , &!
        access  = 'stream'                 , &!
        action  = 'write'                  , &!
        form    = 'unformatted'            , &!
        status  = 'replace'                  &!
    )

    do iter_sample = 0_int32, (10_int32 * num_samples_required_log10)
        associate( target_loc => floor( 10_int32 ** ( real(iter_sample, real64) / 10_int32 ) ) )
            write(write_unit) &!
            &                    target_loc   , &!
            &   acceptance_rate( target_loc )
        end associate
    end do

    close(write_unit)



    open( &!
        newunit = write_unit           , &!
        file    = "../expectation.dat" , &!
        access  = 'stream'             , &!
        action  = 'write'              , &!
        form    = 'unformatted'        , &!
        status  = 'replace'              &!
    )

    do iter_sample = 0_int32, (10_int32 * num_samples_required_log10)
        associate( target_loc => floor( 10_int32 ** ( real(iter_sample, real64) / 10_int32 ) ) )
            write(write_unit) &!
            &                target_loc   , &!
            &   expectation( target_loc )
        end associate
    end do

    close(write_unit)



    contains



    pure function action(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        action = - log( standard_gauss_function(x) + standard_gauss_function(x - 1.0e2_real64) )

    end function action

end program main
