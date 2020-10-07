#[[
Adds support for Steinberg VST version 2 plug-ins
]]

set( TARGET mod-vst )

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( wxIS_MAC on )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set ( wxIS_WIN on )
else()
   set ( wxIS_GTK on)
endif()

# Handle VST option
cmd_option(
   ${_OPT}use_vst
   "Use VST2 plug-in support [on, off]"
   ON
)
set( USE_VST ${${_OPT}use_vst} CACHE INTERNAL "" )

set( SOURCES
   VSTControl.h
   VSTEffect.cpp
   VSTEffect.h
   aeffectx.h
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

audacity_module( "${TARGET}" "${SOURCES}" "Audacity"
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
