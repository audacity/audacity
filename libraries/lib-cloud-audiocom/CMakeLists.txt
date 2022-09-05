#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET lib-cloud-audiocom )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   OAuthService.cpp
   OAuthService.h
   ServiceConfig.cpp
   ServiceConfig.h
   UploadService.cpp
   UploadService.h
   UserService.cpp
   UserService.h
)


set ( LIBRARIES
   lib-cloud-upload-interface
   lib-network-manager-interface # Required for the networking
   lib-string-utils-interface # ToUtf8
   lib-preferences-interface
   lib-basic-ui-interface # CallAfter
   lib-files-interface # FileNames

   PRIVATE
      RapidJSON::RapidJSON # Protocol is JSON based
      wxwidgets::base # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
