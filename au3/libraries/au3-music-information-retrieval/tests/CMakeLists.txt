#[[
Unit tests for au3-music-information-retrieval
]]

add_compile_definitions(
   CMAKE_CURRENT_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}"
)

add_unit_test(
   NAME
      au3-music-information-retrieval
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
      au3-music-information-retrieval
)
