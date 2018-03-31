#directory audacity top level
cmake_minimum_required (VERSION 3.8) # so we can use cotire and source_group
cmake_policy(SET CMP0043 NEW) # just hide an annoying warning in 3.0.2

# There must be a symbol for this already, surely?
set( top_dir ${CMAKE_SOURCE_DIR} )

# These stop the results being mixed in with our source tree.
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# On Windows, WXWIN needs to be set to wxWidgets directory, for wxWidgets to 
# be found automatically OR you need to have wxWidgets installed in the 
# standard location on drive C: (or D:).
# If you are using wxWidgets 3.1 or later, you will need to update
# cmake's FindwxWidgets.cmake file, since 3.1.1 is not listed.

# These aren't needed, provided you took heed of the above.
#set( wxWidgets_ROOT_DIR "C:/wxWidgets-3.1.1" )
#set( wxWidgets_LIB_DIR "C:/wxWidgets-3.1.1/lib/vc_lib" )
#set( wxWidgets_CONFIGURATION "mswud" )


add_subdirectory( "lib-src" )
#add_subdirectory( "src" )

# Uncomment what follows for symbol values.
#[[
get_cmake_property(_variableNames VARIABLES)
foreach (_variableName ${_variableNames})
message(STATUS "${_variableName}=${${_variableName}}")
endforeach()
]]#

