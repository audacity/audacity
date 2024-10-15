#[[
Unit tests for lib-music-information-retrieval
]]

add_compile_definitions(
   CMAKE_CURRENT_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}"
)

add_unit_test(
   NAME
      lib-music-information-retrieval
   WAV_FILE_IO
   SOURCES
      MirFakes.h
      MirTestUtils.cpp
      MirTestUtils.h
      MusicInformationRetrievalTests.cpp
      StftFrameProviderTests.cpp
      TatumQuantizationFitBenchmarking.cpp
      TatumQuantizationFitVisualization.cpp
      WavMirAudioReader.cpp
      WavMirAudioReader.h
   LIBRARIES
      lib-music-information-retrieval
)
