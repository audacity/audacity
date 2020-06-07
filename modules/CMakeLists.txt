# Include the modules that we'll build

add_subdirectory( mod-null )
add_subdirectory( mod-nyq-bench )
add_subdirectory( mod-script-pipe )

if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*" )
   install( DIRECTORY "${_DEST}/modules"
            DESTINATION "${_PKGDATA}" )
endif()

