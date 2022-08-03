set( TARGET locale )
set( TARGET_ROOT ${topdir}/locale )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

# Load the list of catalogs
file( STRINGS ${TARGET_ROOT}/LINGUAS SOURCES )
list( TRANSFORM SOURCES APPEND ".po" )

# Look for gettext
find_package( Gettext QUIET )
if( GETTEXT_FOUND )
   mark_as_advanced( FORCE GETTEXT_MSGFMT_EXECUTABLE )
   mark_as_advanced( FORCE GETTEXT_MSGMERGE_EXECUTABLE )
   set( msgfmt "${GETTEXT_MSGFMT_EXECUTABLE}" --check-format )
elseif( PYTHON )
   set( msgfmt "${PYTHON}" "${TARGET_ROOT}/msgfmt.py" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   nuget_package( pkgdir "Gettext.Tools" "0.20.1.1" )
   file( TO_NATIVE_PATH "${pkgdir}/tools/bin/msgfmt.exe" msgfmt )
endif()

if( NOT DEFINED msgfmt )
   message( WARNING "The msgfmt program wasn't found...translations will not be provided." )
   return()
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( locale "${_DEST}/Resources" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set( locale "${_DEST}/Languages" )
else()
   set( locale "${_DEST}/locale" )
endif()

set( OUTPUTS )
foreach( source ${SOURCES} )
   get_filename_component( lang "${source}" NAME_WE )

   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      set( dst "${locale}/${lang}.lproj" )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      set( dst "${locale}/${lang}" )
   else()
      set( dst "${locale}/${lang}/LC_MESSAGES" )
   endif()

   set( po "${_SRCDIR}/${source}" )
   set( mo "${dst}/audacity.mo" )

   add_custom_command(
      DEPENDS
         "${po}"
      COMMAND
         "${CMAKE_COMMAND}" -E make_directory "${dst}"
      COMMAND
         ${msgfmt} -o "${mo}" "${po}"
      OUTPUT
         "${mo}"
   )

   # Always make sure there's an empty English folder.  Otherwise, some menu
   # items will be translated to the next language listed in System
   # Preferences/Language & Text
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      add_custom_command(
         COMMAND
            ${CMAKE_COMMAND} -E make_directory "${_DEST}/Resources/en.lproj"
         OUTPUT
            "${mo}"
         APPEND
      )
   endif()

   list( APPEND OUTPUTS ${mo} )
endforeach()

add_custom_target( "${TARGET}" ALL DEPENDS ${OUTPUTS} SOURCES "${SOURCES}" )

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY ${locale}/
               TYPE LOCALE )
   endif()
endif()

