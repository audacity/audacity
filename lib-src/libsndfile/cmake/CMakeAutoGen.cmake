# CMake implementation of AutoGen
# Copyright (C) 2017 Anonymous Maarten <anonymous.maarten@gmail.com>

set(AUTOGEN_SCRIPT "${CMAKE_MODULE_PATH}/CMakeAutoGenScript.cmake")

function(lsf_autogen DIR_REL NAME_WE)
	set(EXTS ${ARGN})
	set(INPUT "${CMAKE_CURRENT_SOURCE_DIR}/${DIR_REL}/${NAME_WE}.def")
	set(OUTPUTS)
	foreach(EXT ${EXTS})
		list(APPEND OUTPUTS "${NAME_WE}.${EXT}")
	endforeach()
	add_autogen_target("${INPUT}" "${CMAKE_CURRENT_BINARY_DIR}/${DIR_REL}" ${OUTPUTS})
endfunction()

function(add_autogen_target INPUT OUTPUTDIR)
	set(OUTPUTFILES "${ARGN}")

	if (OUTPUTDIR)
		set(PREFIX "${OUTPUTDIR}/")
	else()
		set(PREFIX "")
	endif()

	set(ARTIFACTS)
	foreach(OUTPUTFILE ${OUTPUTFILES})
		list(APPEND ARTIFACTS "${PREFIX}${OUTPUTFILE}")
	endforeach()

	set(EXTRA_ARGS)
	if (AUTOGEN_DEBUG)
		list(APPEND EXTRA_ARGS "-DDEBUG=1")
	endif()
	if (OUTPUTDIR)
		list(APPEND EXTRA_ARGS "-DOUTPUTDIR=${OUTPUTDIR}")
	endif()

	add_custom_command(
		OUTPUT ${ARTIFACTS}
		COMMAND ${CMAKE_COMMAND} "-DDEFINITION=${INPUT}" ${EXTRA_ARGS} -P "${AUTOGEN_SCRIPT}"
		MAIN_DEPENDENCY "${INPUT}"
		DEPENDS "${AUTOGEN_SCRIPT}"
		COMMENT "CMakeAutoGen: generating ${OUTPUTFILES}"
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
	)
endfunction()
