#[[
A module to look for OSS
]]

if( NOT OSS_FOUND )
   find_path(LINUX_OSS_INCLUDE_DIR "linux/soundcard.h"
      HINTS "/usr/include" "/usr/local/include"
   )

   find_path(SYS_OSS_INCLUDE_DIR "sys/soundcard.h"
      HINTS "/usr/include" "/usr/local/include"
   )

   find_path(MACHINE_OSS_INCLUDE_DIR "machine/soundcard.h"
      HINTS "/usr/include" "/usr/local/include"
   )

   set( HAVE_SYS_SOUNDCARD_H No CACHE BOOL "sys/soundcard.h is available" FORCE )
   set( HAVE_LINUX_SOUNDCARD_H No CACHE BOOL "linux/soundcard.h is available" FORCE )
   set( HAVE_MACHINE_SOUNDCARD_H No CACHE BOOL "machine/soundcard.h is available" FORCE )

   if( LINUX_OSS_INCLUDE_DIR )
      set( OSS_FOUND True )
      set( OSS_INCLUDE_DIR ${LINUX_OSS_INCLUDE_DIR} )
      set( HAVE_LINUX_SOUNDCARD_H Yes CACHE BOOL "sys/soundcard.h is available" FORCE )
      list( APPEND OSS_DEFINITIONS HAVE_LINUX_SOUNDCARD_H=1 )
   elseif( SYS_OSS_INCLUDE_DIR )
      set( OSS_FOUND True )
      set( OSS_INCLUDE_DIR ${SYS_OSS_INCLUDE_DIR} )
      set( HAVE_SYS_SOUNDCARD_H Yes CACHE BOOL "sys/soundcard.h is available" FORCE )
      list( APPEND OSS_DEFINITIONS HAVE_SYS_SOUNDCARD_H=1 )
      
   elseif( MACHINE_OSS_INCLUDE_DIR )
      set( OSS_FOUND True )
      set( OSS_INCLUDE_DIR ${MACHINE_OSS_INCLUDE_DIR} )
      set( HAVE_MACHINE_SOUNDCARD_H Yes CACHE BOOL "sys/soundcard.h is available" FORCE )
      list( APPEND OSS_DEFINITIONS HAVE_MACHINE_SOUNDCARD_H=1 )
   endif()

   if( OSS_FOUND )
      if( NOT OSS_FIND_QUIETLY )
         message( STATUS "Found OSS: \n\tOSS_INCLUDE_DIR: ${OSS_INCLUDE_DIR}" )
      endif()

      if( NOT TARGET OSS::OSS )
         add_library( OSS::OSS INTERFACE IMPORTED GLOBAL)

         target_include_directories( OSS::OSS INTERFACE ${OSS_INCLUDE_DIR} )
         target_compile_definitions( OSS::OSS INTERFACE ${OSS_DEFINITIONS} )
      endif()
   else()
      if( OSS_FIND_REQUIRED )
         message( FATAL_ERROR "Could not find OSS")
      endif()
   endif()

   mark_as_advanced(
      OSS_FOUND
      OSS_INCLUDE_DIR
      OSS_DEFINITIONS
      HAVE_LINUX_SOUNDCARD_H
      HAVE_SYS_SOUNDCARD_H
      HAVE_MACHINE_SOUNDCARD_H
   )
endif()
