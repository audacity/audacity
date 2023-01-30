#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET lib-cloud-upload )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   CloudExporterPlugin.cpp
   CloudExporterPlugin.h
   CloudExportersRegistry.cpp
   CloudExportersRegistry.h
   MimeTypesList.h
)


set ( LIBRARIES
   lib-strings-interface
)

set ( DEFINES
   INTERFACE
      HAS_CLOUD_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
