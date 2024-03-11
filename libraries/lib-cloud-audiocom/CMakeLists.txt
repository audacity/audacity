#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET lib-cloud-audiocom )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   CloudLibrarySettings.cpp
   CloudLibrarySettings.h
   NetworkUtils.cpp
   NetworkUtils.h
   OAuthService.cpp
   OAuthService.h
   ServiceConfig.cpp
   ServiceConfig.h
   UploadService.cpp
   UploadService.h
   UserService.cpp
   UserService.h
   sync/CloudSyncDTO.cpp
   sync/CloudSyncDTO.h
)


set ( LIBRARIES
   lib-network-manager-interface # Required for the networking
   lib-files-interface # FileNames
   lib-strings-interface # Languages
   PUBLIC
      rapidjson::rapidjson # Protocol is JSON based
   PRIVATE
      wxwidgets::base # Required to retrieve the OS information
      wxBase # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
