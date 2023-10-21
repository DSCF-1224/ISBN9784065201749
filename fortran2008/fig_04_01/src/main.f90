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
    !> Metropolis 法で生成したサンプルの格納用配列
    real(real64), allocatable, dimension(:) :: sample



    !> 本 PROGRAM 用の補助変数
    integer :: write_unit



    ! Metropolis 法の結果を格納する配列の確保

    allocate( acceptance_rate (num_samples_required) )
    allocate( sample          (num_samples_required) )



    ! Metropolis 法による正規分布に従う乱数の生成

    call exe_gaussian_metropolis( &!
        prng_seed            = 1_int32              , &!
        initial_sample       = 0.0_real64           , &!
        num_samples_required = num_samples_required , &!
        step_size            = 0.5_real64           , &!
        step_center          = 0.0_real64           , &!
        acceptance_rate      = acceptance_rate(:)   , &!
        generated_samples    = sample(:)              &!
    )



    ! Metropolis 法で生成した正規分布に従う乱数の出力

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



    ! Metropolis 法での採択率の出力

    open( &!
        newunit = write_unit               , &!
        file    = '../acceptance_rate.dat' , &!
        access  = 'stream'                 , &!
        action  = 'write'                  , &!
        form    = 'unformatted'            , &!
        status  = 'replace'                  &!

    )

    block

        !> 本 BLOCK 用の補助変数
        integer(int32) :: iter



        do iter = 0_int32, (10_int32 * num_samples_required_log10)
            associate( target_loc => floor( 10_int32 ** ( real(iter, real64) / 10_int32 ) ) )
                write(write_unit) target_loc, acceptance_rate( target_loc )
            end associate
        end do

    end block

    close(write_unit)

end program main
