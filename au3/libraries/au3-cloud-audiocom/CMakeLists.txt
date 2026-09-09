#[[
A library, that allows uploading to audio.com sharing service
]]

set( TARGET au3-cloud-audiocom )

set( SOURCES
   CloudLibrarySettings.cpp
   CloudLibrarySettings.h
   CloudSyncService.cpp
   CloudSyncService.h
   TaskExecutionService.cpp
   TaskExecutionService.h
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
   au3-concurrency
   au3-preferences
   au3-registries
   au3-sqlite-helpers
   au3-utility
PRIVATE
   au3-basic-ui
   au3-crypto
   au3-exceptions
   au3-files # FileNames
   au3-import-export
   au3-math
   au3-network-manager # Required for the networking
   au3-project
   au3-project-file-io # ProjectFileIOExtension
   au3-project-rate
   au3-string-utils
   au3-strings # Languages
   au3-track
   au3-wave-track
   wavpack::wavpack # Required for the WavPackCompressor
   wxBase # Required to retrieve the OS information
   rapidjson::rapidjson
)

set ( DEFINES
   INTERFACE
      HAS_AUDIOCOM_UPLOAD=1
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
