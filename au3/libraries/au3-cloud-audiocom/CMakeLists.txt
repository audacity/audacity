#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET au3-cloud-audiocom )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   CloudLibrarySettings.cpp
   CloudLibrarySettings.h
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
   sync/CloudSyncDTO.cpp
   sync/CloudSyncDTO.h
   sync/CloudSyncError.cpp
   sync/CloudSyncError.h
   sync/CloudSyncHousekeeper.cpp
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
   sync/ResumedSnaphotUploadOperation.cpp
   sync/ResumedSnaphotUploadOperation.h
   sync/WavPackCompressor.cpp
   sync/WavPackCompressor.h
)


set ( LIBRARIES
PUBLIC
      au3-network-manager-interface # Required for the networking
      au3-files-interface # FileNames
      au3-strings-interface # Languages
      au3-wave-track-interface
      au3-project-file-io-interface # ProjectFileIOExtension
      au3-concurrency-interface
   PRIVATE
      au3-sqlite-helpers-interface
      au3-crypto-interface
      au3-import-export-interface
      wavpack::wavpack # Required for the WavPackCompressor
      wxBase # Required to retrieve the OS information
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
