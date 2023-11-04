module gauss_function_lib

    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_fortran_env, only: real64



    implicit none



    private
    public  :: standard_gauss_function
    public  :: standard_gauss_function_action



    contains



    pure elemental function standard_gauss_function(x)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: standard_gauss_function

        standard_gauss_function = exp( - standard_gauss_function_action(x) )

    end function standard_gauss_function



    pure elemental function standard_gauss_function_action(x) result(action)

        !> 本 FUNCTION の仮引数
        real(real64), intent(in) :: x

        !> 本 FUNCTION の戻り値
        real(real64) :: action

        action = 0.5_real64 * x * x

    end function standard_gauss_function_action

end module gauss_function_lib
