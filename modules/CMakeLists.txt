# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
   mod-audiounits
   mod-soundtouch
   mod-script-pipe
)
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows" )
   list( APPEND MODULES
      mod-null
      mod-nyq-bench
   )
endif()

# Visit each module directory
foreach( MODULE ${MODULES} )
   add_subdirectory("${MODULE}")
endforeach()

# Visit collected target names, and make a custom target for each to do the
# library copy step.  Make dependencies among these steps so that they are
# sequenced -- otherwise concurrent use of CopyLibs.cmake can cause races on
# the file system.  But this sequencing still allows copy steps to be
# concurrent with the compilation of other modules.
set( PREV_TARGET )
foreach( TARGET ${ALL_MODULE_TARGETS} )
   set( TARGET2 "${TARGET}-copy-libs" )
   if( CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" )
      set( src "${_MODDIR}/${TARGET}.dll" )
      set( dst "${_EXEDIR}" )
      set( wxwin "${_SHARED_PROXY_BASE_PATH}/$<CONFIG>/" )
   else()
      set( src "${_MODDIR}/${TARGET}.so" )
      set( dst "${_PKGLIB}" )
      set( wxwin "${_SHARED_PROXY_BASE_PATH}/$<CONFIG>" )
    endif()
   add_custom_target( ${TARGET2} ALL
      COMMAND ${CMAKE_COMMAND}
         -D SRC="${src}" -D DST="${dst}" -D WXWIN="${wxwin}"
	 -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
   )
   add_dependencies( ${TARGET2} ${TARGET} )
   if( PREV_TARGET )
      add_dependencies( ${TARGET2} ${PREV_TARGET} )
   endif()
   set_target_properties( ${TARGET2}
      PROPERTIES
         PREFIX ""
         FOLDER "modules/copy-steps" # for IDE organization
   )
   set( PREV_TARGET ${TARGET2} )
endforeach()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY "${_DEST}/modules"
               DESTINATION "${_PKGLIB}" )
   endif()
endif()

#propagate collected edges up to root CMakeLists.txt
set( GRAPH_EDGES "${GRAPH_EDGES}" PARENT_SCOPE )
