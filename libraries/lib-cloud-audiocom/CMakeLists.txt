#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET lib-cloud-audiocom )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   CloudSettings.cpp
   CloudSettings.h
   CloudSyncService.cpp
   CloudSyncService.h
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
   sync/CloudSyncError.cpp
   sync/CloudSyncError.h
   sync/CloudSyncUtils.cpp
   sync/CloudSyncUtils.h
   sync/DataUploader.cpp
   sync/DataUploader.h
   sync/LocalProjectSnapshot.cpp
   sync/LocalProjectSnapshot.h
   sync/MissingBlocksUploader.cpp
   sync/MissingBlocksUploader.h
   sync/MixdownUploader.cpp
   sync/MixdownUploader.h
   sync/ProjectCloudExtension.cpp
   sync/ProjectCloudExtension.h
   sync/ProjectUploadOperation.h
   sync/RemoteProjectSnapshot.cpp
   sync/RemoteProjectSnapshot.h
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
      lib-concurrency-interface
   PRIVATE
      lib-sqlite-helpers-interface
      lib-crypto-interface
      lib-import-export-interface
      wavpack::wavpack # Required for the WavPackCompressor
      wxBase # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
