# Copy library during build and, on the Mac, modify the dependent
# library paths.
#
# Defines required:
#
# SRC    source library name
# DST    destination directory
#
message( "==================================================================" )
message( "Copying wxWidgets libraries:" )
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

function( gather_libs src )
   if( CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" )
      execute( output cmd /k dumpbin /dependents ${src} )

      foreach( line ${output} )
         if( line MATCHES "^ *wx.*\\.dll" )
            set( lib ${WXWIN}/${line} )

            list( APPEND libs ${lib} )

            gather_libs( ${lib} )
         endif()
      endforeach()
   elseif( CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" )
      execute( output otool -L ${src} )

      get_filename_component( libname "${src}" NAME )

      if( libname MATCHES ".*dylib" )
         string( PREPEND libname "${DST}/" )
      else()
         set( libname "${src}" )
      endif()

      foreach( line ${output} )
         if( line MATCHES "^.*libwx.*\\.dylib " )
            string( REGEX REPLACE "dylib .*" "dylib" line "${line}" )
            if( NOT line STREQUAL "${src}" AND NOT line MATCHES "@executable" )
               set( lib ${line} )

               list( APPEND libs ${lib} )

               get_filename_component( refname "${lib}" NAME )
               list( APPEND postcmds "sh -c 'install_name_tool -change ${lib} @executable_path/../Frameworks/${refname} ${libname}'" )

               gather_libs( ${lib} )
            endif()
         endif()
      endforeach()
   elseif( CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" )
      execute( output sh -c "LD_LIBRARY_PATH='${WXWIN}' ldd ${src}" )

      get_filename_component( libname "${src}" NAME )

      foreach( line ${output} )
         if( line MATCHES ".*libwx.*" )
            string( REGEX REPLACE ".* => (.*) \\(.*$" "\\1" line "${line}" )

            set( lib ${line} )

            list( APPEND libs ${lib} )

            gather_libs( ${lib} )
         endif()
      endforeach()
   endif()

   set( libs ${libs} PARENT_SCOPE )
   set( postcmds ${postcmds} PARENT_SCOPE )
endfunction()

gather_libs( "${SRC}" )

list( REMOVE_DUPLICATES libs )

file( INSTALL ${libs} DESTINATION ${DST} FOLLOW_SYMLINK_CHAIN )

foreach( cmd ${postcmds} )
   execute_process(
      COMMAND
         sh -c "${cmd}"
      COMMAND_ECHO STDOUT
   )
endforeach()

