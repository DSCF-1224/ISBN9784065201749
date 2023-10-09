program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: arange_lib
    use, non_intrinsic :: MC_lib

    use, non_intrinsic :: statistics_lib, only: calc_average       => average
    use, non_intrinsic :: statistics_lib, only: calc_std_corrected => std_corrected



    implicit none



    !> 本 PROGRAM 用の PARAMETER
    integer(int32), parameter :: num_seeds = 100_int32



    !> 本 PROGRAM 用の PARAMETER
    integer(int32), dimension(5), parameter :: half_value_width = [2, 10, 100, 1000, 10000]



    !> 本 PROGRAM 用の変数
    !> モンテカルロ法のサンプル点数
    integer(int32), parameter, dimension(29) :: num_samples = &!
        (/ &!
        &     100,    200,   300,   400,   500,   600,   700,   800,   900, &!
        &    1000,   2000,  3000,  4000,  5000,  6000,  7000,  8000,  9000, &!
        &   10000,  20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, &!
        &  100000, 200000  &!
        /)

    !> 本 PROGRAM 用の変数
    !> 擬似乱数生成器のシード値
    integer(int32), allocatable, dimension(:) :: seed



    !> 本 PROGRAM 用の変数
    !> モンテカルロ法による Gauss 関数の積分値
    real(real64), allocatable, dimension(:,:) :: integral



    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_half_value_width

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_num_samples

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_seed

    !> 本 PROGRAM 用の補助変数
    integer(int32) :: write_unit



    ! 計算結果を保存するファイルを開く

    open( &!
        file    = '../approx.dat' , &!
        newunit = write_unit      , &!
        action  = 'write'         , &!
        form    = 'formatted'     , &!
        status  = 'replace'         &!
    )



    ! 擬似乱数生成器のシード値を配列として作成する

    call allocatable_with_arange( &!
        array  = seed      , &!
        start_ = 1_int32   , &!
        end_   = num_seeds   &!
    )



    ! モンテカルロ法で計算した積分値を保存する配列を確保する

    allocate( integral( size( num_samples(:) ), size( seed(:) ) ) )



    do iter_half_value_width = 1_int32, size( half_value_width(:) )

        do iter_seed = 1_int32, size( seed(:) )

            do iter_num_samples = 1_int32, size( num_samples(:) )

                call MC_integral( &!
                    num_samples      = num_samples(iter_num_samples)                         , &!
                    half_value_width = real(half_value_width(iter_half_value_width), real64) , &!
                    seed             = seed(iter_seed)                                       , &!
                    integral         = integral(iter_num_samples, iter_seed)                   &!
                )

            end do

        end do



        do iter_num_samples = 1_int32, size( num_samples(:) )

            block

                !> 本 BLOCK 用の変数
                real(real64) :: average



                average = calc_average( integral(iter_num_samples,:) )

                write( unit=write_unit, fmt=* ) &!
                &   num_samples(iter_num_samples) , &!
                &   average , &!
                &   calc_std_corrected(integral(iter_num_samples,:), average)

            end block

        end do

        write( unit=write_unit, fmt=* ) ! BLANK_LINE

    end do

end program main
