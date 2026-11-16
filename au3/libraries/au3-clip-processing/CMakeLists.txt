#[[
A library providing a time-stretching-capable implementation of
`PlayableSequence`.

Please refer to this directory's readme for a presentation of the constraints
inherent to such an implementation.
]]

set( TARGET au3-clip-processing )

set( SOURCES
   # What a clip offers to playback, and what it carries along
   AudioSegmentSampleView.cpp
   AudioSegmentSampleView.h
   ClipFades.cpp
   ClipFades.h
   ClipInterface.cpp
   ClipInterface.h
   ClipTimeAndPitchSource.cpp
   ClipTimeAndPitchSource.h
   PlaybackDirection.h
   TempoChange.cpp
   TempoChange.h

   # Per clip: stretching, buffering, fades
   clip-graph/ClipBufferingNode.cpp
   clip-graph/ClipBufferingNode.h
   clip-graph/ClipFadesNode.cpp
   clip-graph/ClipFadesNode.h
   clip-graph/ClipProcessingGraphFactory.cpp
   clip-graph/ClipProcessingGraphFactory.h
   clip-graph/ClipStretchingNode.cpp
   clip-graph/ClipStretchingNode.h
   clip-graph/IClipProcessingNode.h

   # Per track: placing the clips on the timeline, smoothing the transitions
   sequence-graph/ClipTimelineNode.cpp
   sequence-graph/ClipTimelineNode.h
   sequence-graph/CrossFadingSmoother.cpp
   sequence-graph/CrossFadingSmoother.h
   sequence-graph/ISequenceProcessingNode.h

   # The PlayableSequence built from these graphs
   PlayableSequenceFactory.cpp
   PlayableSequenceFactory.h
   PlayableSequenceImpl.cpp
   PlayableSequenceImpl.h
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
   au3-xml
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
