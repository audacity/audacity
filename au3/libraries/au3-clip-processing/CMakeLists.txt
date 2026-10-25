#[[
A library providing a time-stretching-capable implementation of
`PlayableSequence`.

Please refer to this directory's readme for a presentation of the constraints
inherent to such an implementation.
]]

set( TARGET au3-clip-processing )

set( SOURCES
   clip-graph/IClipProcessingNode.h
   sequence-graph/ISequenceProcessingNode.h
   AudioSegmentSampleView.cpp
   AudioSegmentSampleView.h
   clip-graph/ClipBufferingNode.cpp
   clip-graph/ClipBufferingNode.h
   clip-graph/ClipProcessingGraphFactory.cpp
   clip-graph/ClipProcessingGraphFactory.h
   ClipInterface.cpp
   ClipInterface.h
   clip-graph/ClipStretchingNode.cpp
   clip-graph/ClipStretchingNode.h
   PlaybackDirection.h
   PlayableSequenceImpl.cpp
   PlayableSequenceImpl.h
   sequence-graph/ClipTimelineNode.cpp
   sequence-graph/ClipTimelineNode.h
   PlayableSequenceFactory.cpp
   PlayableSequenceFactory.h
   ClipTimeAndPitchSource.cpp
   ClipTimeAndPitchSource.h
   TempoChange.cpp
   TempoChange.h
)
set( LIBRARIES
PUBLIC
   au3-math
   au3-mixer
   au3-registries
   au3-time-and-pitch
   au3-utility
PRIVATE
   au3-channel
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
