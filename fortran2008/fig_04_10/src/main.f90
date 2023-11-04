! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: gauss_function_lib      , only: standard_gauss_function_action
    use, non_intrinsic :: math_constants_lib
    use, non_intrinsic :: metropolis_sampling_lib



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
    !> Metropolis 法で生成したサンプルの格納用配列
    real(real64), allocatable, dimension(:) :: sample



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( sample          (num_samples_required) )



    ! Metropolis 法を実行

    call exe_simulation( 0.5_real64 )



    contains



    pure function action(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        if      ( x .ge.  0.0_real64 ) then ; action =   standard_gauss_function_action(x) + MATH_LOG_SQRT_2PI
        else if ( x .lt. -1.0_real64 ) then ; action =   huge(x)
        else                                ; action = - log( MATH_2_PI * sqrt( 1.0_real64 - x * x ) )
        end if

    end function action



    subroutine exe_simulation(step_size)

        !> 本 SUBROUTINE の仮引数
        !> Metropolis 法のステップ幅
        real(real64), intent(in) :: step_size



        !> 本 SUBROUTINE 用の変数
        !> 生成した乱数 / 採択率を保存するファイル名
        character(len=15) :: file_name_result



        !> 本 SUBROUTINE 用の補助変数
        integer(int32) :: iter_sample

        !> 本 SUBROUTINE 用の補助変数
        integer :: write_unit



        !> 生成した乱数 / 採択率を保存するファイル名の生成

        write( unit=file_name_result(1:), fmt="('../sample',I2.2,'.dat')" ) int(step_size * 10_int32)



        ! Metropolis 法で生成した乱数の出力用ファイルの作成

        open( &!
            newunit = write_unit          , &!
            file    = file_name_result(:) , &!
            access  = 'stream'            , &!
            action  = 'write'             , &!
            form    = 'unformatted'       , &!
            status  = 'replace'             &!
        )



        ! Metropolis 法による
        ! 2つの Gauss 分布を重ね合わせた分布
        ! に従う乱数の生成

        call exe_metropolis_sampling( &!
            action               = action               , &!
            prng_seed            = 1_int32              , &!
            initial_sample       = 0.0_real64           , &!
            num_samples_required = num_samples_required , &!
            step_size            = step_size            , &!
            step_center          = 0.0_real64           , &!
            acceptance_rate      = acceptance_rate(:)   , &!
            generated_samples    = sample(:)              &!
        )




        ! Metropolis 法で生成した乱数の出力

        do iter_sample = 1_int32, num_samples_required

            write(unit=write_unit) &!
            &   acceptance_rate (iter_sample) , &!
            &   sample          (iter_sample)

        end do

        close(write_unit)

    end subroutine exe_simulation

end program main
