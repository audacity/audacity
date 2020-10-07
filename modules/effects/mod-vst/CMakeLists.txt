#[[
User interface for effects using the VST version 2 protocol
]]

set( TARGET mod-vst )

# Handle VST option
cmd_option(
   ${_OPT}use_vst
   "Use VST2 plug-in support [on, off]"
   ON
)
set( USE_VST ${${_OPT}use_vst} CACHE INTERNAL "" )

set( SOURCES
   VSTControl.h
   VSTEditor.cpp
   VSTEditor.h
   VSTEffect.cpp
   VSTEffect.h
   VSTEffectOptionsDialog.cpp
   VSTEffectOptionsDialog.h
   $<$<BOOL:${wxIS_GTK}>:
      VSTControlGTK.cpp
      VSTControlGTK.h
   >
   $<$<BOOL:${wxIS_WIN}>:
      VSTControlMSW.cpp
      VSTControlMSW.h
   >
   $<$<BOOL:${wxIS_MAC}>:
      VSTControlOSX.mm
      VSTControlOSX.h
   >
)

if(${wxIS_MAC})
   set_source_files_properties(
      VSTControlOSX.mm
      PROPERTIES
         SKIP_PRECOMPILE_HEADERS YES
   )
endif()

set( LIBRARIES
   PRIVATE
      Audacity
      lib-vst-interface
)

# Third party libraries used only by this module
set(EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

audacity_module( "${TARGET}" "${SOURCES}" "${LIBRARIES}"
   ""
   "$<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>" )

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set_source_files_properties(
      VSTControlOSX.mm
      PROPERTIES
         COMPILE_FLAGS "-x objective-c++"
         SKIP_PRECOMPILE_HEADERS YES
   )
else()
   pkg_check_modules( GTK REQUIRED IMPORTED_TARGET GLOBAL ${gtk} )
   if( $GTK_FOUND )
      target_compile_options( ${TARGET} PRIVATE "${GTK_INCLUDE_DIRS}" )
   endif()
endif()
