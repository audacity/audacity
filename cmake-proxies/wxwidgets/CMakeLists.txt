
add_library( ${TARGET} INTERFACE )

def_vars()

message( STATUS "========== Configuring ${TARGET} ==========" )

option( use_system_wxwidgets "Use ${TARGET} system library if available" ${prefer_system_libs} )
if( use_system_wxwidgets )
   find_package(wxWidgets)
endif()

if( wxWidgets_FOUND )
   message( STATUS "Using '${TARGET}' system library" )

   if( wxWidgets_INCLUDE_DIRS_NO_SYSTEM )
      set( INCLUDES
         INTERFACE
            ${wxWidgets_INCLUDE_DIRS_NO_SYSTEM}
      )
   else()
      set( INCLUDES
         INTERFACE
            ${wxWidgets_INCLUDE_DIRS}
      )
   endif() 

   set( DEFINES
      INTERFACE
         ${wxWidgets_DEFINITIONS}
         $<$<CONFIG:Debug>:${wxWidgets_DEFINITIONS_DEBUG}>
   )

   set( LINKDIRS
      INTERFACE
         $<$<PLATFORM_ID:Windows>:${wxWidgets_LIB_DIR}>
   )

   set( LIBRARIES
      INTERFACE
         ${wxWidgets_LIBRARIES}
         $<$<NOT:$<PLATFORM_ID:Windows>>:z>
   )

   set( toolkit "${wxWidgets_LIBRARIES}" )
else()
   message( STATUS "Using '${TARGET}' system library" )

   set( use_system_wxwidgets OFF CACHE BOOL "Prefer wxWidgets system library if available" FORCE )

   set( WXWIN $ENV{WXWIN} )
   if( "${WXWIN}" STREQUAL "" )
      # XXX: Look into importing instead of adding to this project
      set( WXWIN "${_INTDIR}/wxwidgets" )
   endif()

   if( NOT EXISTS "${WXWIN}" )
      find_package( Git )
      if( NOT GIT_FOUND )
         message( FATAL_ERROR "Git is needed to clone wxWidgets" )
      endif()

      execute_process(
         COMMAND
            ${GIT_EXECUTABLE} clone
                              --depth 1
                              --single-branch
                              --recurse-submodules
                              https://github.com/audacity/wxwidgets
                              "${WXWIN}"
      )
   endif()

   if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      # Want accessibility
      set( wxUSE_ACCESSIBILITY YES )

      # Windows requires it due to missing "#include" directives
      set( wxBUILD_PRECOMP YES )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      # Want accessibility
      set( wxUSE_ACCESSIBILITY YES )

      # Causes problems on OSX, so turn it off
      set( wxBUILD_PRECOMP NO )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Linux" )
      # Doesn't yet have accessbility
      set( wxUSE_ACCESSIBILITY NO )

      # Linux can go either way, so might as well use it
      set( wxBUILD_PRECOMP YES )
   endif()

   # Just to be consistent with Audacity
   set( wxBUILD_CXX_STANDARD "14" )

   # Pull in wxWidgets
   add_subdirectory( ${WXWIN} ${WXWIN} )

   # And rearrange the folder structure
   set_dir_folder( ${WXWIN} "wxWidgets" )

   set( INCLUDES
      $<$<STREQUAL:"${wxUSE_ZLIB}","builtin">:${WXWIN}/src/zlib>
   )

   set( DEFINES
      WXUSINGDLL
   )

   set( LIBRARIES
      adv
      base
      core
      html
      net
      qa
      xml
      $<$<STREQUAL:"${wxUSE_EXPAT}","builtin">:wxexpat>
      $<$<STREQUAL:"${wxUSE_LIBJPEG}","builtin">:wxjpeg>
      $<$<STREQUAL:"${wxUSE_LIBPNG}","builtin">:wxpng>
      $<$<STREQUAL:"${wxUSE_LIBTIFF}","builtin">:wxtiff>
      $<$<STREQUAL:"${wxUSE_REGEX}","builtin">:wxregex>
      $<$<STREQUAL:"${wxUSE_ZLIB}","builtin">:wxzlib>
   )

   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      # When accessibility is enabled, the build will fail in "wx/chkconf.h"
      # since wxRegex compile defines do not include __WXOSX_COCOA__.  So,
      # add it here.
      target_compile_definitions( wxregex PRIVATE "__WXOSX_COCOA__" )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Linux" )
      set( toolkit "${wxBUILD_TOOLKIT}" )
   endif()
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Linux" )
   # We need the system GTK/GLIB packages
   if( "${toolkit}" MATCHES ".*gtk2.*" )
      set( gtk gtk+-2.0 )
      set( glib glib-2.0 )
   elseif( "${toolkit}" MATCHES ".*gtk3.*" )
      set( gtk gtk+-3.0 )
      set( glib glib-2.0 )
   elseif( "${toolkit}" MATCHES ".*gtk4.*" )
      set( gtk gtk+-4.0 )
      set( glib glib-2.0 )
   else()
      message( FATAL_ERROR "Unrecognized wxGTK version: ${wxBUILD_TOOLKIT}" )
   endif()
 
   pkg_check_modules( GTK REQUIRED IMPORTED_TARGET GLOBAL ${gtk} )
   pkg_check_modules( GLIB REQUIRED IMPORTED_TARGET GLOBAL ${glib} )
endif()

target_include_directories( ${TARGET} INTERFACE ${INCLUDES} )
target_compile_definitions( ${TARGET} INTERFACE ${DEFINES} )
target_link_directories( ${TARGET} INTERFACE ${LINKDIRS} )
target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )

