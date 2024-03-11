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

   sync/BlockHasher.cpp
   sync/BlockHasher.h
   sync/CloudProjectsDatabase.cpp
   sync/CloudProjectsDatabase.h
   sync/CloudSyncDTO.cpp
   sync/CloudSyncDTO.h
)


set ( LIBRARIES
PUBLIC
      lib-network-manager-interface # Required for the networking
      lib-files-interface # FileNames
      lib-strings-interface # Languages
      lib-wave-track-interface
      rapidjson::rapidjson # Protocol is JSON based
   PRIVATE
      lib-sqlite-helpers-interface
      lib-crypto-interface
      wxBase # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
