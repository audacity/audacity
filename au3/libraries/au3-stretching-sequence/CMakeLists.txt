#[[
A library providing a time-stretching-capable implementation of
`PlayableSequence`.

Please refer to this directory's readme for a presentation of the constraints
inherent to such an implementation.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AudioSegment.cpp
   AudioSegment.h
   AudioSegmentFactory.cpp
   AudioSegmentFactory.h
   AudioSegmentFactoryInterface.cpp
   AudioSegmentFactoryInterface.h
   AudioSegmentSampleView.cpp
   AudioSegmentSampleView.h
   ClipInterface.cpp
   ClipInterface.h
   ClipSegment.cpp
   ClipSegment.h
   PlaybackDirection.h
   SilenceSegment.cpp
   SilenceSegment.h
   StretchingSequence.cpp
   StretchingSequence.h
   ClipTimeAndPitchSource.cpp
   ClipTimeAndPitchSource.h
   TempoChange.cpp
   TempoChange.h
)
set( LIBRARIES
   au3-channel
   au3-mixer
   au3-time-and-pitch
)
audacity_library( au3-stretching-sequence "${SOURCES}" "${LIBRARIES}"
   "" ""
)
