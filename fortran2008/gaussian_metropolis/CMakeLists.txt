if (NOT (EXISTS ${CMAKE_Fortran_MODULE_DIRECTORY}/metropolis_sampling))
	add_subdirectory(
		${CMAKE_CURRENT_SOURCE_DIR}/../metropolis_sampling
		${CMAKE_Fortran_MODULE_DIRECTORY}/metropolis_sampling
	)
endif()



add_library(
	_gaussian_metropolis_lib
	STATIC
	${CMAKE_CURRENT_SOURCE_DIR}/lib.f90
)



target_link_libraries( _gaussian_metropolis_lib PRIVATE _metropolis_sampling_lib )
