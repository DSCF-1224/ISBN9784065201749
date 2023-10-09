program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: arange_lib

    use, non_intrinsic :: pi_MC_lib, only: pi_MC_unit => pi_MC_integral

    use, non_intrinsic :: statistics_lib, only: calc_average       => average
    use, non_intrinsic :: statistics_lib, only: calc_std_corrected => std_corrected



    implicit none



    !> 本 PROGRAM 用の PARAMETER
    integer(int32), parameter :: num_samples_minval = 100_int32

    !> 本 PROGRAM 用の PARAMETER
    integer(int32), parameter :: num_samples_maxval = 10000_int32

    !> 本 PROGRAM 用の PARAMETER
    integer(int32), parameter :: num_seeds = 100_int32



    !> 本 PROGRAM 用の変数
    !> モンテカルロ法のサンプル点数
    integer(int32), allocatable, dimension(:) :: num_samples_total

    !> 本 PROGRAM 用の変数
    !> 擬似乱数生成器のシード値
    integer(int32), allocatable, dimension(:) :: seed

    !> 本 PROGRAM 用の変数
    !> モンテカルロ法による \pi/4 の近似値
    real(real64), allocatable, dimension(:,:) :: pi_quarter



    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_num_samples

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_seed

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: write_unit



    open( &!
        file    = '../approx.dat' , &!
        newunit = write_unit      , &!
        action  = 'write'         , &!
        form    = 'formatted'     , &!
        status  = 'replace'         &!
    )



    call allocatable_with_arange( &!
        array  = num_samples_total                       , &!
        start_ =                      num_samples_minval , &!
        end_   = num_samples_maxval                      , &!
        step_  = num_samples_maxval / num_samples_minval   &!
    )



    call allocatable_with_arange( &!
        array  = seed      , &!
        start_ = 1_int32   , &!
        end_   = num_seeds   &!
    )



    allocate( pi_quarter( size( num_samples_total(:) ), size( seed(:) ) ) )



    do iter_seed = 1_int32, size( seed(:) )

        do iter_num_samples = 1_int32, size( num_samples_total(:) )

            call pi_MC_unit( &!
                num_samples = num_samples_total(iter_num_samples)     , &!
                seed        = seed(iter_seed)                         , &!
                pi_quarter  = pi_quarter(iter_num_samples, iter_seed)   &!
            )

            write( unit=write_unit, fmt=* ) &!
            &   num_samples_total(iter_num_samples) , &!
            &   pi_quarter(iter_num_samples, iter_seed)

        end do

        write( unit=write_unit, fmt=* ) ! BLANK_LINE

    end do



    close(unit=write_unit)



    open( &!
        file    = '../stats.dat' , &!
        newunit = write_unit     , &!
        action  = 'write'        , &!
        form    = 'formatted'    , &!
        status  = 'replace'        &!
    )



    do iter_num_samples = 1_int32, size( num_samples_total(:) )

        block

            !> 本 BLOCK 用の変数
            real(real64) :: average

            average = calc_average( pi_quarter(iter_num_samples,:) )

            write( unit=write_unit, fmt=* ) &!
            &   num_samples_total(iter_num_samples) , &!
            &   maxval( pi_quarter(iter_num_samples,:) ) , &!
            &   average , &!
            &   minval( pi_quarter(iter_num_samples,:) ) , &!
            &   calc_std_corrected( pi_quarter(iter_num_samples,:), average )

        end block

    end do



    close(unit=write_unit)

end program main
