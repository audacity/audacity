#directory audacity top level
# 3.8 so we can use source_group
# cotire only needs 2.8.12
cmake_minimum_required (VERSION 3.8) 
cmake_policy(SET CMP0043 NEW) # just hide an annoying warning in 3.0.2

# Renaming.  Could just use the official name throughout.
set( top_dir ${CMAKE_SOURCE_DIR} )

# Path for cotire.cmake, and later for our wxwidgets.cmake.
set( CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake-proxies/cmake-modules)
include(cotire)

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


#add_subdirectory( "lib-src" ) #All lib building happens via the proxies.
add_subdirectory( "cmake-proxies" )
add_subdirectory( "src" )

# Uncomment what follows for symbol values.
#[[
get_cmake_property(_variableNames VARIABLES)
foreach (_variableName ${_variableNames})
message(STATUS "${_variableName}=${${_variableName}}")
endforeach()
]]#

