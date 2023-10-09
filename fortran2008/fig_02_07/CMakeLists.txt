# How to run (each command)
# $ ./reset_build.sh
# $ cd b*
# $ time cmake -DCMAKE_BUILD_TYPE=Debug/Relase ..
# $ time make ..
# $ time build/test.exe
# $ cd -

# How to run (1 line)
# $ ./reset_build.sh && cd b* && time cmake -DCMAKE_BUILD_TYPE=Debug .. && time make && time ./test.exe && cd -
# $ ./reset_build.sh && cd b* && time cmake -DCMAKE_BUILD_TYPE=Release .. && time make && time ./test.exe && cd -

cmake_minimum_required(VERSION 3.0)

enable_language(Fortran C)
project(test.exe Fortran)

set(CMAKE_C_COMPILER               "gcc")
set(CMAKE_Fortran_COMPILER         "gfortran")
set(CMAKE_Fortran_FLAGS            "-ffree-line-length-none -fimplicit-none -pedantic -std=f2008 -Wall -Werror -Wextra")
set(CMAKE_Fortran_FLAGS_DEBUG      "-fbacktrace -fbounds-check -g -O0")
set(CMAKE_Fortran_FLAGS_RELEASE    "-O2")
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran-modules)



if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/arange))
    add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../arange
		${CMAKE_Fortran_MODULE_DIRECTORY}/arange
	)
endif()



if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/intrinsic_prng_initializer))
    add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../intrinsic_prng_initializer
		${CMAKE_Fortran_MODULE_DIRECTORY}/intrinsic_prng_initializer
	)
endif()



if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/math_constants))
    add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../math_constants
		${CMAKE_Fortran_MODULE_DIRECTORY}/math_constants
	)
endif()



if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/statistics))
    add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../statistics
		${CMAKE_Fortran_MODULE_DIRECTORY}/statistics
	)
endif()



add_executable(${PROJECT_NAME}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/lib.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/src/main.f90
)



target_link_libraries( ${PROJECT_NAME} PRIVATE _arange_lib )
target_link_libraries( ${PROJECT_NAME} PRIVATE _intrinsic_prng_initializer_lib )
target_link_libraries( ${PROJECT_NAME} PRIVATE _math_constants_lib )
target_link_libraries( ${PROJECT_NAME} PRIVATE _statistics_lib )

# EOF
