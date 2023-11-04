! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: gauss_function_lib      , only: standard_gauss_function_action
    use, non_intrinsic :: metropolis_sampling_lib
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



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( sample          (num_samples_required) )
    allocate( expectation     (num_samples_required) )



    ! Metropolis 法を実行

    call exe_simulation( 0.5_real64 )
    call exe_simulation( 2.0_real64 )
    call exe_simulation( 4.0_real64 )



    contains



    pure function sampler_action(x) result(action)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        action = standard_gauss_function_action(x)

    end function sampler_action



    pure function objective_action(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: objective_action

        if   ( abs(x) .lt. 1.0_real64 ) then ; objective_action = - 0.5_real64 * log( 1.0_real64 - x * x )
        else                                 ; objective_action =   huge(x)
        end if

    end function objective_action



    pure elemental function integrand(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: integrand

        integrand = exp( sampler_action(x) - objective_action(x) )

    end function integrand



    subroutine exe_simulation(step_size)

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップ幅
        real(real64), intent(in) :: step_size



        !> 本 SUBROUTINE 用の変数
        !> MCMC で得られた期待値を保存するファイル名
        character(len=20) :: file_name_expectation



        !> 本 SUBROUTINE 用の補助変数
        integer(int32) :: iter_sample

        !> 本 SUBROUTINE 用の補助変数
        integer :: write_unit



        !> 計算結果を保存するファイル名の生成

        write( unit=file_name_expectation (1:), fmt="('../expectation' , I2.2, '.dat')" ) int(step_size * 10_int32)



        ! 計算結果を保存するファイルの作成

        open( &!
            newunit = write_unit               , &!
            file    = file_name_expectation(:) , &!
            access  = 'stream'                 , &!
            action  = 'write'                  , &!
            form    = 'unformatted'            , &!
            status  = 'replace'                  &!
        )



        ! Metropolis 法による乱数の生成

        call exe_metropolis_sampling( &!
            action               = sampler_action       , &!
            prng_seed            = 1_int32              , &!
            initial_sample       = 0.0_real64           , &!
            num_samples_required = num_samples_required , &!
            step_size            = step_size            , &!
            step_center          = 0.0_real64           , &!
            acceptance_rate      = acceptance_rate(:)   , &!
            generated_samples    = sample(:)              &!
        )



        ! Metropolis 法で生成した乱数から、
        ! 目的の積分値を計算する

        call welford_online_average( integrand( sample(:) ), expectation(:) )




        ! Metropolis 法で計算した積分値を保存する

        do iter_sample = 0_int32, (10_int32 * num_samples_required_log10)

            associate( target_loc => floor( 10_int32 ** ( real(iter_sample, real64) / 10_int32 ) ) )

                write(unit=write_unit) &!
                &                target_loc   , &!
                &   expectation( target_loc )

            end associate

        end do

        close(write_unit)

    end subroutine exe_simulation

end program main
