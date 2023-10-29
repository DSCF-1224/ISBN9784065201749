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



add_library(
	_jackknife_statistics_lib
	STATIC
	${CMAKE_CURRENT_SOURCE_DIR}/lib.f90
)




target_link_libraries( _jackknife_statistics_lib PRIVATE _math_constants_lib )
target_link_libraries( _jackknife_statistics_lib PRIVATE _statistics_lib     )
