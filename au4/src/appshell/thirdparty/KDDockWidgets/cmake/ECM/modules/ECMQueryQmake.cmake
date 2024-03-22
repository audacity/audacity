find_package(Qt5Core QUIET)

if (Qt5Core_FOUND)
    set(_qmake_executable_default "qmake-qt5")
endif ()
if (TARGET Qt5::qmake)
    get_target_property(_qmake_executable_default Qt5::qmake LOCATION)
endif()
set(QMAKE_EXECUTABLE ${_qmake_executable_default}
    CACHE FILEPATH "Location of the Qt5 qmake executable")

# Helper method
# This is not public API (yet)!
# Usage: query_qmake(<result_variable> <qt_variable> [TRY])
# Passing TRY will result in the method not failing fatal if no qmake executable
# has been found, but instead simply returning an empty string
function(query_qmake result_variable qt_variable)
    set(options TRY)
    set(oneValueArgs )
    set(multiValueArgs )

    cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if(NOT QMAKE_EXECUTABLE)
        if(ARGS_TRY)
            set(${result_variable} "" PARENT_SCOPE)
            message(STATUS "No qmake Qt5 binary found. Can't check ${qt_variable}")
            return()
        else()
            message(FATAL_ERROR "No qmake Qt5 binary found. Can't check ${qt_variable} as required")
        endif()
    endif()
    execute_process(
        COMMAND ${QMAKE_EXECUTABLE} -query "${qt_variable}"
        RESULT_VARIABLE return_code
        OUTPUT_VARIABLE output
    )
    if(return_code EQUAL 0)
        string(STRIP "${output}" output)
        file(TO_CMAKE_PATH "${output}" output_path)
        set(${result_variable} "${output_path}" PARENT_SCOPE)
    else()
        message(WARNING "Failed call: ${QMAKE_EXECUTABLE} -query \"${qt_variable}\"")
        message(FATAL_ERROR "QMake call failed: ${return_code}")
    endif()
endfunction()
