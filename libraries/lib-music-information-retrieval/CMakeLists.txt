#[[
Library for Music Information Retrieval (MIR) algorithms.
]]

set( SOURCES
   DecimatingMirAudioReader.cpp
   DecimatingMirAudioReader.h
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
   PowerSpectrumGetter.cpp
   PowerSpectrumGetter.h
   StftFrameProvider.cpp
   StftFrameProvider.h
)

set( LIBRARIES
PUBLIC
   pffft
   lib-concurrency-interface
   lib-file-formats-interface
   lib-utility-interface
)

audacity_library( lib-music-information-retrieval "${SOURCES}" "${LIBRARIES}"
   "" ""
)
