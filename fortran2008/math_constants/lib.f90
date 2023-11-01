module math_constants_lib

    use, intrinsic :: iso_fortran_env, only: real64

    implicit none

    !> 本 MODULE で定義する PARAMETER
    !> 円周率
    real(real64), parameter :: MATH_PI_DEF = acos(-1.0_REAL64)

    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: QUIET_NAN_REAL64 = transfer(source=-1_real64, mold=0.0_real64)



    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_2PI = 2 * MATH_PI_DEF

    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_2_PI = 2 / MATH_PI_DEF

    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_SQRTPI = sqrt(MATH_PI_DEF)



    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_1_SQRT2PI = 1.0_REAL64 / sqrt( MATH_2PI )

    !> 本 MODULE で定義する PARAMETER
    real(real64), parameter :: MATH_LOG_SQRT_2PI = log( sqrt(MATH_2PI) )

end module math_constants_lib
