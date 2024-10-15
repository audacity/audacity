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
target_link_libraries(${TARGET}
   breakpad::processor
   breakpad::sender
   wxwidgets::wxwidgets
   lib-strings-interface
)

target_compile_definitions(${TARGET} PRIVATE INSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")

set_target_properties(
	${TARGET}
	PROPERTIES
		RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${TARGET}/bin/$<CONFIG>
)

if(WIN32)
	set_target_properties(${TARGET} PROPERTIES WIN32_EXECUTABLE ON)
endif()

if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*")
   install( TARGETS ${TARGET} RUNTIME )
endif()

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
