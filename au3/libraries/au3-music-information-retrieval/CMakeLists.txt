#[[
Library for Music Information Retrieval (MIR) algorithms.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   DecimatingMirAudioReader.cpp
   DecimatingMirAudioReader.h
   GetMeterUsingTatumQuantizationFit.cpp
   GetMeterUsingTatumQuantizationFit.h
   MirDsp.cpp
   MirDsp.h
   MirProjectInterface.h
   MirTypes.h
   MirUtils.cpp
   MirUtils.h
   MusicInformationRetrieval.cpp
   MusicInformationRetrieval.h
   StftFrameProvider.cpp
   StftFrameProvider.h
)

set( LIBRARIES
PUBLIC
   lib-fft
   lib-utility
   lib-file-formats-interface
)

audacity_library( lib-music-information-retrieval "${SOURCES}" "${LIBRARIES}"
   "" ""
)
