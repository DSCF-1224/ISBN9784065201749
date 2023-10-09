module math_constants_lib

    use, intrinsic :: iso_fortran_env, only: real64

    implicit none

    !> 本 MODULE で定義する PARAMETER
    !> 円周率
    real(real64), parameter :: MATH_PI_DEF = acos(-1.0_REAL64)



    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_2PI = 2 * MATH_PI_DEF



    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_1_SQRT2PI = 1.0_REAL64 / sqrt( MATH_2PI )

end module math_constants_lib
