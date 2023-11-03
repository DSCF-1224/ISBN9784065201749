module intrinsic_prng_initializer_lib

    use, intrinsic :: iso_fortran_env, only: int32

    implicit none

    private
    public  :: intrinsic_prng_initializer_type



    type :: intrinsic_prng_initializer_type

        !> `RANDOM_SEED` 文の `SIZE` 保持用
        integer(int32), private :: size_seed

        !> `RANDOM_SEED` 文の `PUT` 用変数
        integer(int32), allocatable, dimension(:), private :: seed_array

        contains

        procedure, pass(initializer), public :: put

    end type intrinsic_prng_initializer_type



    contains



    subroutine put(initializer, seed)

        !> 本 SUBROUTINE の仮引数
        class(intrinsic_prng_initializer_type), intent(inout) :: initializer

        !> 本 SUBROUTINE の仮引数
        !> `RANDOM_SEED` 文の `PUT` に与える値
        integer(int32), intent(in) :: seed



        if ( allocated(initializer%seed_array) ) then
            deallocate(initializer%seed_array)
        end if

        call random_seed(size=initializer%size_seed)

        allocate( initializer%seed_array( initializer%size_seed ), source=0_int32 )

        initializer%seed_array(1) = seed

        call random_seed( put=initializer%seed_array(:) )

    end subroutine put

end module intrinsic_prng_initializer_lib
