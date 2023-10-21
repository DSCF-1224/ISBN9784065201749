
module statistics_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    implicit none

    private
    public  :: average
    public  :: std_corrected
    public  :: std_uncorrected
    public  :: var_corrected
    public  :: var_uncorrected
    public  :: welford_online_average



    interface std_corrected
        module procedure :: std_corrected_with_average
        module procedure :: std_corrected_without_average
    end interface std_corrected



    interface std_uncorrected
        module procedure :: std_uncorrected_with_average
        module procedure :: std_uncorrected_without_average
    end interface std_uncorrected



    interface sum_squared_deviations
        module procedure :: sum_squared_deviations_with_average
        module procedure :: sum_squared_deviations_without_average
    end interface sum_squared_deviations



    interface var_corrected
        module procedure :: var_corrected_with_average
        module procedure :: var_corrected_without_average
    end interface var_corrected



    interface var_uncorrected
        module procedure :: var_uncorrected_with_average
        module procedure :: var_uncorrected_without_average
    end interface var_uncorrected



    interface welford_online_average
        module procedure :: welford_online_average_array
    end interface welford_online_average



    contains



    pure function average(array)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        real(real64) :: average

        average = sum( array(:) ) / size( array(:) )

    end function average






    pure function std_corrected_with_average(array, average_) result(std_corrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: average_

        !> 本 FUNCTION の戻り値
        !> corrected variance
        real(real64) :: std_corrected_

        std_corrected_ = sqrt( var_corrected( array(:), average_ ) )

    end function std_corrected_with_average



    pure function std_corrected_without_average(array) result(std_corrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        !> corrected variance
        real(real64) :: std_corrected_

        std_corrected_ = sqrt( var_corrected( array(:) ) )

    end function std_corrected_without_average






    pure function std_uncorrected_with_average(array, average_) result(std_uncorrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: average_

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: std_uncorrected_

        std_uncorrected_ = sqrt( var_uncorrected( array(:), average_ ) )

    end function std_uncorrected_with_average



    pure function std_uncorrected_without_average(array) result(std_uncorrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: std_uncorrected_

        std_uncorrected_ = sqrt( var_uncorrected( array(:) ) )

    end function std_uncorrected_without_average






    pure function sum_squared_deviations_with_average(array, average_) result(sum_squared_deviations_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: average_

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: sum_squared_deviations_

        sum_squared_deviations_ = sum( ( array(:) - average_ ) ** 2 )

    end function sum_squared_deviations_with_average



    pure function sum_squared_deviations_without_average(array) result(sum_squared_deviations_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: sum_squared_deviations_

        sum_squared_deviations_ = sum_squared_deviations( array(:), average( array(:) ) )

    end function sum_squared_deviations_without_average






    pure function var_corrected_with_average(array, average_) result(var_corrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: average_

        !> 本 FUNCTION の戻り値
        !> corrected variance
        real(real64) :: var_corrected_

        var_corrected_ = sum_squared_deviations( array(:), average_ ) / size( array(:) - 1 )

    end function var_corrected_with_average



    pure function var_corrected_without_average(array) result(var_corrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        !> corrected variance
        real(real64) :: var_corrected_

        var_corrected_ = sum_squared_deviations( array(:) ) / size( array(:) - 1 )

    end function var_corrected_without_average






    pure function var_uncorrected_with_average(array, average_) result(var_uncorrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: average_

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: var_uncorrected_

        var_uncorrected_ = sum_squared_deviations( array(:), average_ ) / size( array(:) )

    end function var_uncorrected_with_average



    pure function var_uncorrected_without_average(array) result(var_uncorrected_)

        !> 本 FUNCTION の仮引数
        real(real64), dimension(:), intent(in) :: array

        !> 本 FUNCTION の戻り値
        !> uncorrected variance
        real(real64) :: var_uncorrected_

        var_uncorrected_ = sum_squared_deviations( array(:) ) / size( array(:) )

    end function var_uncorrected_without_average



    subroutine welford_online_average_array(source, average_)

        !> 本 SUBROUTINE の仮引数
        !> 平均を計算するデータ
        real(real64), dimension(:), intent(in) :: source

        !> 本 SUBROUTINE の仮引数
        !> 計算した平均値を格納する配列
        real(real64), dimension( size( source(:) ) ) :: average_



        !> 本 SUBROUTINE 用の補助変数
        integer(int32) :: iter



        average_(1) = source(1)



        do iter = 2_int32, size( source(:) )

            associate( new_average => average_ ( iter           ) )
            associate( ref_average => average_ ( iter - 1_int32 ) )
            associate( new_source  => source   ( iter           ) )

                new_average = &!
                ref_average + (new_source - ref_average) / iter

            end associate
            end associate
            end associate

        end do

    end subroutine welford_online_average_array

end module statistics_lib
