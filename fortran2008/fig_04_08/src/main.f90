! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/Gaussian_Metropolis.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: gaussian_metropolis_lib
    use, non_intrinsic :: statistics_lib



    implicit none



    !> 本 PROGRAM 用の PARAMETER
    !> Metropolis 法のサンプル点数
    integer(int32), parameter :: num_samples_required_log10 = 5_int32

    !> 本 PROGRAM 用の PARAMETER
    !> Metropolis 法のサンプル点数
    integer(int32), parameter :: num_samples_required = 10_int32 ** num_samples_required_log10

    !> 本 PROGRAM 用の PARAMETER
    !> Metropolis 法のステップ幅
    real(real64), parameter, dimension(7) :: list_step_size = [0.5_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 6.0_real64, 8.0_real64]



    !> 本 PROGRAM 用の変数
    !> Metropolis 法での採択率の推移
    real(real64), allocatable, dimension(:) :: acceptance_rate

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの平方の期待値の格納用配列
    real(real64), allocatable, dimension(:) :: expectation2

    !> 本 PROGRAM 用の変数
    !> Metropolis 法で生成したサンプルの格納用配列
    real(real64), allocatable, dimension(:) :: sample



    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_step_size

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_sample

    !> 本 PROGRAM 用の補助変数
    integer :: write_unit



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( expectation2    (num_samples_required) )
    allocate( sample          (num_samples_required) )



    do iter_step_size = 1_int32, size( list_step_size(:) )
    block

        !> 本 BLOCK 用の変数
        !> 生成した乱数 / 採択率を保存するファイル名
        character(len=15) :: file_name_result



        associate( target_step_size => list_step_size(iter_step_size) )

            !> 生成した乱数 / 採択率を保存するファイル名の生成

            write( unit=file_name_result(1:), fmt="('../sample',I2.2,'.dat')" ) int(target_step_size * 10_int32)



            ! Metropolis 法で生成した乱数の出力用ファイルの作成

            open( &!
                newunit = write_unit          , &!
                file    = file_name_result(:) , &!
                access  = 'stream'            , &!
                action  = 'write'             , &!
                form    = 'unformatted'       , &!
                status  = 'replace'             &!
            )



            ! Metropolis 法による正規分布に従う乱数の生成
        
            call exe_gaussian_metropolis( &!
                prng_seed            = 1_int32              , &!
                initial_sample       = 0.0_real64           , &!
                num_samples_required = num_samples_required , &!
                step_size            = target_step_size     , &!
                step_center          = 0.0_real64           , &!
                acceptance_rate      = acceptance_rate(:)   , &!
                generated_samples    = sample(:)              &!
            )



            ! Metropolis 法で生成した乱数の期待値

            call welford_online_average( sample(:) * sample(:), expectation2(:) )




            ! Metropolis 法で生成した乱数の統計値の出力

            do iter_sample = 1_int32, num_samples_required

                write(unit=write_unit) &!
                &   acceptance_rate (iter_sample) , &!
                &   sample          (iter_sample) , &!
                &   expectation2    (iter_sample)

            end do

            close(write_unit)

        end associate

    end block
    end do

end program main
