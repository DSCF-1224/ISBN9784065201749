! [original]
! https://github.com/masanorihanada/MCMC-Sample-Codes/blob/master/three_sphere.c

program main

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: arange_lib
    use, non_intrinsic :: statistics_lib
    use, non_intrinsic :: sphere_volume_MC_lib



    implicit none



    !> 本 PROGRAM 用の PARAMETER
    integer(int32), parameter :: num_seeds = 100_int32

    !> 本 PROGRAM 用の PARAMETER
    !> モンテカルロ法のサンプル点数
    integer(int32), parameter, dimension(37) :: num_samples_total = &!
        (/ &!
        &       10,     20,     30,     40,     50,     60,     70,     80,     90, &!
        &      100,    200,    300,    400,    500,    600,    700,    800,    900, &!
        &     1000,   2000,   3000,   4000,   5000,   6000,   7000,   8000,   9000, &!
        &    10000,  20000,  30000,  40000,  50000,  60000,  70000,  80000,  90000, &!
        &   100000  &!
        /)



    !> 本 PROGRAM 用の変数
    !> 球の次元
    integer(int32), allocatable, dimension(:) :: dimension_sphere

    !> 本 PROGRAM 用の変数
    !> 扇形の内点となったサンプル点数
    integer(int32), allocatable, dimension(:,:) :: num_samples_in

    !> 本 PROGRAM 用の変数
    !> 擬似乱数生成器のシード値
    integer(int32), allocatable, dimension(:) :: seed



    !> 本 PROGRAM 用の変数
    !> モンテカルロ法による球の体積の積分値
    real(real64), allocatable, dimension(:,:) :: volume_sphere



    !> 本 PROGRAM 用の補助変数
    integer(int32) :: iter_dimension_sphere

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



    ! 球の体積を計算する次元を配列として作成する

    call allocatable_with_arange( &!
        array  = dimension_sphere , &!
        start_ =  3_int32         , &!
        end_   = 11_int32           &!
    )



    ! 擬似乱数生成器のシード値を配列として作成する

    call allocatable_with_arange( &!
        array  = seed      , &!
        start_ = 1_int32   , &!
        end_   = num_seeds   &!
    )



    ! モンテカルロ法で計算した積分値を保存する配列を確保する

    allocate( num_samples_in ( size( num_samples_total(:) ), size( seed(:) ) ) )
    allocate( volume_sphere  ( size( num_samples_total(:) ), size( seed(:) ) ) )



    loop_dimension_sphere: &!
    do iter_dimension_sphere = 1_int32, size( dimension_sphere(:) )



        ! モンテカルロ法による球の体積の計算

        do iter_seed        = 1_int32, size( seed(:) )
        do iter_num_samples = 1_int32, size( num_samples_total(:) )

            call sphere_volume_MC( &!
                dimension_sphere  = dimension_sphere  ( iter_dimension_sphere                             ) , &!
                num_samples_total = num_samples_total (                       iter_num_samples            ) , &!
                seed              = seed              (                                         iter_seed ) , &!
                num_samples_in    = num_samples_in    (                       iter_num_samples, iter_seed ) , &!
                volume            = volume_sphere     (                       iter_num_samples, iter_seed )   &!
            )

        end do
        end do



        ! 異なるシード値から得られた
        ! 球の体積の近似値の統計値の計算

        loop_num_samples: &!
        do iter_num_samples = 1_int32, size( num_samples_total(:) )

            block

                !> 本 BLOCK 用の変数
                !> モンテカルロ法の実行時に採択されたサンプル点数
                !> の平均値
                real(real64) :: average_num_samples_in

                !> 本 BLOCK 用の変数
                !> 球の体積の平均値
                real(real64) :: average_volume



                average_num_samples_in = average( real( num_samples_in (iter_num_samples,:), real64 ) / num_samples_total(iter_num_samples) )
                average_volume         = average(       volume_sphere  (iter_num_samples,:)                                                 )



                write( unit=write_unit, fmt=* ) &!
                &   num_samples_total(iter_num_samples) , &!
                &   average_num_samples_in , std_corrected( real( num_samples_in (iter_num_samples,:), real64 ) / num_samples_total(iter_num_samples) ), &!
                &   average_volume         , std_corrected(       volume_sphere  (iter_num_samples,:)                                                 )

            end block

        end do &!
        loop_num_samples

        write( unit=write_unit, fmt=* ) ! BLANK_LINE



    end do &!
    loop_dimension_sphere



    close(unit=write_unit)

end program main
