if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/intrinsic_prng_initializer))
	add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../intrinsic_prng_initializer
		${CMAKE_Fortran_MODULE_DIRECTORY}/intrinsic_prng_initializer
	)
endif()

add_library(
	_pi_MC_lib
	STATIC
	${CMAKE_CURRENT_SOURCE_DIR}/lib.f90
)

target_link_libraries( _pi_MC_lib PRIVATE _intrinsic_prng_initializer_lib )
