#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET lib-cloud-audiocom )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   CloudSyncService.cpp
   CloudSyncService.h
   OAuthService.cpp
   OAuthService.h
   ServiceConfig.cpp
   ServiceConfig.h
   UploadService.cpp
   UploadService.h
   UserService.cpp
   UserService.h

   sync/CloudProjectSnapshot.cpp
   sync/CloudProjectSnapshot.h
   sync/CloudSyncUtils.cpp
   sync/CloudSyncUtils.h
   sync/CloudSyncTask.cpp
   sync/CloudSyncTask.h
   sync/MissingBlocksUploader.cpp
   sync/MissingBlocksUploader.h
   sync/WavPackCompressor.cpp
   sync/WavPackCompressor.h
)


set ( LIBRARIES
   lib-network-manager-interface # Required for the networking
   lib-files-interface # FileNames
   lib-strings-interface # Languages
   lib-project-file-io-interface # ProjectFileIOExtension
   PUBLIC
      rapidjson::rapidjson # Protocol is JSON based
   PRIVATE
      sqlite
      wavpack::wavpack # Required for the WavPackCompressor
      wxwidgets::base # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
