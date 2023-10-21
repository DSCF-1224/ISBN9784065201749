! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: gaussian_metropolis_lib



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
    !> Metropolis 法で生成したサンプルの期待値の格納用配列
    real(real64), allocatable, dimension(:) :: expectation

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの平方の期待値の格納用配列
    real(real64), allocatable, dimension(:) :: expectation2

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの格納用配列
    real(real64), allocatable, dimension(:) :: sample



    !> 本 PROGRAM 用の補助変数
    integer :: iter_sample

    !> 本 PROGRAM 用の補助変数
    integer :: write_unit



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( expectation     (num_samples_required) )
    allocate( expectation2    (num_samples_required) )
    allocate( sample          (num_samples_required) )



    ! Metropolis 法による正規分布に従う乱数の生成

    call exe_gaussian_metropolis( &!
        prng_seed            = 1_int32              , &!
        initial_sample       = 1.0e2_real64         , &!
        num_samples_required = num_samples_required , &!
        step_size            = 0.5_real64           , &!
        step_center          = 0.0_real64           , &!
        acceptance_rate      = acceptance_rate(:)   , &!
        generated_samples    = sample(:)              &!
    )



    ! Metropolis 法で生成した乱数の出力

    open( &!
        newunit = write_unit       , &!
        file    = '../samples.dat' , &!
        access  = 'stream'         , &!
        action  = 'write'          , &!
        form    = 'unformatted'    , &!
        status  = 'replace'          &!
    )

    write(write_unit) sample(:)

    close(write_unit)




    ! Metropolis 法で生成した乱数の統計値の出力

    open( &!
        newunit = write_unit     , &!
        file    = '../stats.dat' , &!
        access  = 'stream'       , &!
        action  = 'write'        , &!
        form    = 'unformatted'  , &!
        status  = 'replace'        &!
    )



    expectation  (1) = sample(1)
    expectation2 (1) = sample(1) * sample(1)



    do iter_sample = 2_int32, num_samples_required

        associate( new_expectation => expectation ( iter_sample           ) )
        associate( ref_expectation => expectation ( iter_sample - 1_int32 ) )
        associate( new_sample      => sample      ( iter_sample           ) )

            new_expectation = &!
            ref_expectation + (new_sample - ref_expectation) / iter_sample

        end associate
        end associate
        end associate

        associate( new_expectation2 => expectation2 ( iter_sample           ) )
        associate( ref_expectation2 => expectation2 ( iter_sample - 1_int32 ) )
        associate( new_sample       => sample       ( iter_sample           ) )

            new_expectation2 = &!
            ref_expectation2 + (new_sample * new_sample - ref_expectation2) / iter_sample

        end associate
        end associate
        end associate

    end do



    do iter_sample = 0_int32, (10_int32 * num_samples_required_log10)

        associate( target_loc => floor( 10_int32 ** ( real(iter_sample, real64) / 10_int32 ) ) )

            write(write_unit) &!
            &                     target_loc   , &!
            &   acceptance_rate ( target_loc ) , &!
            &   expectation     ( target_loc ) , &!
            &   expectation2    ( target_loc )

        end associate

    end do



    close(write_unit)

end program main
