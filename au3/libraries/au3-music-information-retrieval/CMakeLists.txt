#[[
Library for Music Information Retrieval (MIR) algorithms.
]]

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
   au3-fft
   au3-utility
   au3-file-formats-interface
)

audacity_library( au3-music-information-retrieval "${SOURCES}" "${LIBRARIES}"
   "" ""
)
