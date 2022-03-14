#Adds a Crash Reporting dialog which may be invoked by a crashing program

set(TARGET crashreporter)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR})

message( STATUS "========== Configuring ${TARGET} ==========" )

set(SOURCES
	PRIVATE
		warning.xpm
		CrashReportApp.h
		CrashReportApp.cpp
	)

add_executable(${TARGET})
target_sources(${TARGET} ${SOURCES})
target_link_libraries(${TARGET} breakpad::processor breakpad::sender wxwidgets::wxwidgets)

if( CMAKE_CONFIGURATION_TYPES )
   set( CRASH_REPORTER_EXEDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../crashreporter/${CMAKE_CFG_INTDIR}" )
else()
   set( CRASH_REPORTER_EXEDIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../crashreporter/${CMAKE_BUILD_TYPE}" )
endif()

set_target_property_all( ${TARGET} RUNTIME_OUTPUT_DIRECTORY "${CRASH_REPORTER_EXEDIR}" )
add_dependencies( ${TARGET} Audacity )

if(WIN32)
	set_target_properties(${TARGET} PROPERTIES WIN32_EXECUTABLE ON)
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}> "${_EXEDIR}/crashreporter"
      COMMAND
         ${CMAKE_COMMAND} -D SRC="${_EXEDIR}/crashreporter"
                          -D DST="${_PKGLIB}"
                          -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>"
                          -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
      POST_BUILD
   )
elseif(UNIX)
   target_compile_definitions(${TARGET} PRIVATE -DINSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")
   install(TARGETS ${TARGET} RUNTIME)
elseif(WIN32)
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}> "${_EXEDIR}/crashreporter.exe"
      POST_BUILD
   )
endif()

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
