# Copy library during build and, on the Mac, modify the dependent
# library paths.
#
# Defines required:
#
# SRC    source library name
# DST    destination directory
#

# enable IN_LIST operator
cmake_policy(SET CMP0057 NEW)

message( "==================================================================" )
message( "Copying shared libraries:" )
message( "==================================================================" )

# list command no longer ignores empty elements.
cmake_policy( SET CMP0007 NEW )

find_package( Python3 REQUIRED)

message(STATUS "Checking ${SRC} for libraries...")

set( plugins_args )
foreach( plugin ${PLUGINS} )
   list(APPEND plugins_args -g "${plugin}" )
endforeach()

get_filename_component(dir ${CMAKE_SCRIPT_MODE_FILE} DIRECTORY)

execute_process(
   COMMAND
      ${Python3_EXECUTABLE}
      "${dir}/../../scripts/build/fixup_libs.py"
      -i ${WXWIN}
      -o ${DST}
      ${plugins_args}
      ${SRC}
   OUTPUT_VARIABLE output
)

message(STATUS "${output}")
