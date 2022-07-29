cmake_policy(SET CMP0011 NEW)
cmake_policy(SET CMP0007 NEW)

set(install_dir "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}")
set(private_lib_dir "${install_dir}/${_LIBDIR}/audacity")

message(STATUS "Running fix_rpath in ${private_lib_dir}")

file(GLOB libs LIST_DIRECTORIES Off "${private_lib_dir}/*.so*")

function(get_rpath outvar lib)
   execute_process(
      COMMAND
         readelf -d "${lib}"
      RESULT_VARIABLE
         result
      OUTPUT_VARIABLE
         output
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_VARIABLE
         err
   )

   if( NOT result EQUAL 0 )
      message(FATAL_ERROR "readelf failed: (${result}) ${output} ${err}")
   endif()

   string( REPLACE "\n" ";" output "${output}" )
   list( TRANSFORM output STRIP )

   foreach(line ${output})
      if (line MATCHES ".*RUNPATH.*\\[(.*)\\]")
         set(${outvar} "${CMAKE_MATCH_1}" PARENT_SCOPE)
         return()
      elseif (line MATCHES ".*RPATH.*\\[(.*)\\]")
         set(${outvar} "${CMAKE_MATCH_1}" PARENT_SCOPE)
         return()
      endif()
   endforeach()

   set(${outvar} "" PARENT_SCOPE)
endfunction()

foreach(lib ${libs})
   message(STATUS "Processing ${lib}")

   get_rpath(current_rpath ${lib})

   if( current_rpath )
      if(NOT current_rpath STREQUAL "$ORIGIN")
         file(RPATH_CHANGE
               FILE "${lib}"
               OLD_RPATH "${current_rpath}"
               NEW_RPATH "$ORIGIN")
      endif()
   else()
      message(STATUS "\tNo rpath was set")
   endif()
endforeach()
