#[[
Unit tests for au3-stretching-sequence
]]

# The tests build "tests/samples/..." paths from this; point it at the AU3 root (au3/), not the AU4 one
add_compile_definitions(CMAKE_SOURCE_DIR="${AUDACITY_ROOT}")

add_unit_test(
   NAME
      au3-stretching-sequence
   SOURCES
      AudioContainerHelper.h
      AudioSegmentSampleViewTest.cpp
      ClipSegmentTest.cpp
      ClipTimeAndPitchSourceTest.cpp
      FloatVectorClip.cpp
      FloatVectorClip.h
      MockAudioSegmentFactory.h
      MockSampleBlock.cpp
      MockSampleBlock.h
      MockSampleBlockFactory.cpp
      MockSampleBlockFactory.h
      MockPlayableSequence.h
      SilenceSegmentTest.cpp
      StretchingSequenceTest.cpp
      StretchingSequenceIntegrationTest.cpp
      TestWaveClipMaker.cpp
      TestWaveClipMaker.h
      TestWaveTrackMaker.cpp
      TestWaveTrackMaker.h
   MOCK_PREFS
   MOCK_AUDIO
   WAV_FILE_IO
   LIBRARIES
      au3-stretching-sequence
      au3-wave-track
      au3-project
      au3-xml
)
