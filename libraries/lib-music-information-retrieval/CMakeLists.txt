#[[
Library for Music Information Retrieval (MIR) algorithms.
]]

set( SOURCES
   GetMeterUsingTatumQuantizationFit.cpp
   GetMeterUsingTatumQuantizationFit.h
   MirAudioReader.cpp
   MirAudioReader.h
   MirDsp.cpp
   MirDsp.h
   MirTypes.h
   MirUtils.cpp
   MirUtils.h
   MusicInformationRetrieval.cpp
   MusicInformationRetrieval.h
   StftFrameProvider.cpp
   StftFrameProvider.h
)

set( LIBRARIES
   lib-math
   lib-file-formats-interface
)

audacity_library( lib-music-information-retrieval "${SOURCES}" "${LIBRARIES}"
   "" ""
)
