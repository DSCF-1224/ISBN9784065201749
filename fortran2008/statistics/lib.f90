
module statistics_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64

    implicit none

    private
    public  :: std_corrected
    public  :: std_uncorrected
    public  :: var_corrected
    public  :: var_uncorrected
    public  :: average



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

end module statistics_lib
