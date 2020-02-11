
set( TARGET locale )
set( TARGET_ROOT ${topdir}/locale )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

list( APPEND SOURCES
   af.po
   ar.po
   be.po
   bg.po
   bn.po
   bs.po
   ca.po
   ca_ES@valencia.po
   cs.po
   cy.po
   da.po
   de.po
   el.po
   es.po
   eu.po
   eu_ES.po
   fa.po
   fi.po
   fr.po
   ga.po
   gl.po
   he.po
   hi.po
   hr.po
   hu.po
   hy.po
   id.po
   it.po
   ja.po
   ka.po
   km.po
   ko.po
   lt.po
   mk.po
   my.po
   nb.po
   nl.po
   oc.po
   pl.po
   pt_BR.po
   pt_PT.po
   ro.po
   ru.po
   sk.po
   sl.po
   sr_RS.po
   sr_RS@latin.po
   sv.po
   ta.po
   tg.po
   tr.po
   uk.po
   vi.po
   zh_CN.po
   zh_TW.po
)

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )

   file( TO_NATIVE_PATH "${CMAKE_BINARY_DIR}/packages/${GETTEXT_NAME}.${GETTEXT_VERSION}/tools/bin/msgfmt.exe" msgfmt )

elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   find_package( Gettext )
   if( GETTEXT_FOUND )
      set( msgfmt "${GETTEXT_MSGFMT_EXECUTABLE}" )
   else()
      set( root "${CMAKE_CURRENT_BINARY_DIR}/usr/local" )
      set( msgfmt "${CMAKE_CURRENT_BINARY_DIR}/msgfmt" )

      if( NOT EXISTS "${root}" )
         execute_process(
            COMMAND
               curl -L -o /tmp/gettext.pkg https://raw.githubusercontent.com/rudix-mac/packages/master/gettext-0.20.1-macos10.14.pkg
         )
         execute_process(
            COMMAND
               pkgutil --force --expand /tmp/gettext.pkg /tmp/gettext
         )
         execute_process(
            COMMAND
               tar -C "${CMAKE_CURRENT_BINARY_DIR}" -x -f /tmp/gettext/gettextinstall.pkg/Payload
         )
         execute_process(
            COMMAND
               rm -rf /tmp/gettext.pkg /tmp/gettext 
         )
      endif()

      file( WRITE ${CMAKE_BINARY_DIR}/msgfmt
         "#!/bin/sh\n"
         "export DYLD_LIBRARY_PATH=${root}/lib\n"
         "${root}/bin/msgfmt \$@ \n"
      )

      file( COPY ${CMAKE_BINARY_DIR}/msgfmt
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
         FILE_PERMISSIONS
            OWNER_READ OWNER_EXECUTE
            GROUP_READ GROUP_EXECUTE
            WORLD_READ WORLD_EXECUTE
      )

      file( REMOVE ${CMAKE_BINARY_DIR}/msgfmt )

   endif()

 
else()

   find_package( Gettext )
   if( GETTEXT_FOUND )
      set( msgfmt "${GETTEXT_MSGFMT_EXECUTABLE}" )
   endif()

endif()

if( NOT DEFINED msgfmt )
   message( WARNING "Gettext not found...translations will not be provided." )
   return()
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( locale "${_DEST}/Resources" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD" )
   set( locale "${_DEST}/locale" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set( locale "${_DEST}/Languanges" )
endif()

set( OUTPUTS )
foreach( source ${SOURCES} )
   get_filename_component( lang "${source}" NAME_WE )

   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      set( dst "${locale}/${lang}.lproj" )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD" )
      set( dst "${locale}/${lang}/LC_MESSAGES" )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      set( dst "${locale}/${lang}" )
   endif()

   set( po "${_SRCDIR}/${source}" )
   set( mo "${dst}/audacity.mo" )

   add_custom_command(
      DEPENDS
         "${po}"
      COMMAND
         "${CMAKE_COMMAND}" -E make_directory "${dst}"
      COMMAND
         "${msgfmt}" -o "${mo}" "${po}"
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

if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*" )
   install( DIRECTORY ${locale}/
            TYPE LOCALE )
endif()

