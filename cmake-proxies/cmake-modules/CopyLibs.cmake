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

function( execute )
   list( POP_FRONT ARGV outlist )

   execute_process(
      COMMAND
         ${ARGV}
      OUTPUT_VARIABLE
         cmd_out
#      COMMAND_ECHO STDOUT
      OUTPUT_STRIP_TRAILING_WHITESPACE
   )

#message("OUTPUT\n${cmd_out}")

   # Convert output to list and strip
   string( REPLACE "\n" ";" cmd_out "${cmd_out}" )
   list( TRANSFORM cmd_out STRIP )

   set( ${outlist} ${cmd_out} PARENT_SCOPE )
endfunction()

set( VISITED )
function( gather_libs src )
   if( CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" )
      list( APPEND VISITED "${src}" )
      execute( output cmd /k dumpbin /dependents ${src} )

      foreach( line ${output} )
         set( lib ${WXWIN}/${line} )

         if( EXISTS "${lib}" AND NOT "${lib}" IN_LIST VISITED )
            list( APPEND libs ${lib} )

            gather_libs( ${lib} )
         elseif ( EXISTS "${DST}/${line}" AND NOT "${DST}/${line}" IN_LIST VISITED )
            gather_libs( "${DST}/${line}" )
         endif()
      endforeach()
   elseif( CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" )
      message(STATUS "Checking ${src} for libraries...")
      
      execute( output otool -L ${src} )

      set( libname "${src}" )
      
      foreach( line ${output} )
         if( line MATCHES "^.*\\.dylib " )
            string( REGEX REPLACE "dylib .*" "dylib" line "${line}" )

            get_filename_component( dylib_name "${line}" NAME)

            message(STATUS "Checking out ${line}")
            set( lib "${WXWIN}/${dylib_name}" )

            if( NOT lib STREQUAL "${src}" AND NOT line MATCHES "@executable" AND EXISTS "${lib}"
	       AND NOT "${src}///${lib}" IN_LIST VISITED )
               list( APPEND VISITED "${src}///${lib}" )
               message(STATUS "\tProcessing ${lib}...")

               list( APPEND libs ${lib} )

               get_filename_component( refname "${lib}" NAME )
               
               message(STATUS "\t\tAdding ${refname} to ${src}")

               list( APPEND postcmds "sh -c 'install_name_tool -change ${line} @executable_path/../Frameworks/${refname} ${src}'" )

               gather_libs( ${lib} )
            endif()
         endif()
      endforeach()
   elseif( CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" )
      list( APPEND VISITED "${src}" )
      message(STATUS "Executing LD_LIBRARY_PATH='${WXWIN}' ldd ${src}")

      execute( output sh -c "LD_LIBRARY_PATH='${WXWIN}' ldd ${src}" )

      get_filename_component( libname "${src}" NAME )

      foreach( line ${output} )
         string( REGEX REPLACE "(.*) => .* \\(.*$" "\\1" line "${line}" )

         message (STATUS "\tChecking ${line}...")

         set(line "${WXWIN}/${line}")
         
         if (EXISTS "${line}" AND NOT "${line}" IN_LIST VISITED)
            message (STATUS "\tAdding ${line}...")

            set( lib ${line} )

            list( APPEND libs ${lib} )

            gather_libs( ${lib} )
         endif()

      endforeach()
   endif()

   set( libs ${libs} PARENT_SCOPE )
   set( postcmds ${postcmds} PARENT_SCOPE )
   set( VISITED ${VISITED} PARENT_SCOPE )
endfunction()

gather_libs( "${SRC}" )

list( REMOVE_DUPLICATES postcmds )

foreach( cmd ${postcmds} )
   execute_process(
      COMMAND
         sh -c "${cmd}"
      COMMAND_ECHO STDOUT
   )
endforeach()

list( REMOVE_DUPLICATES libs )
file( INSTALL ${libs} DESTINATION ${DST} FOLLOW_SYMLINK_CHAIN )
