#[[
A library providing the graphs that process sequences as they are played:
time-stretching, fades, etc. of their clips.

Please refer to this directory's readme for a presentation of the constraints
inherent to such an implementation.
]]

set( TARGET au3-clip-processing )

set( SOURCES
   clip-graph/IClipProcessingNode.h
   AudioSegmentSampleView.cpp
   AudioSegmentSampleView.h
   clip-graph/ClipBufferingNode.cpp
   clip-graph/ClipBufferingNode.h
   clip-graph/ClipEnvelopeNode.cpp
   clip-graph/ClipEnvelopeNode.h
   ClipFades.cpp
   ClipFades.h
   clip-graph/ClipFadesNode.cpp
   clip-graph/ClipFadesNode.h
   clip-graph/ClipProcessingGraphFactory.cpp
   clip-graph/ClipProcessingGraphFactory.h
   ClipInterface.cpp
   ClipInterface.h
   clip-graph/ClipStretchingNode.cpp
   clip-graph/ClipStretchingNode.h
   sequence-graph/CrossFadingSmoother.cpp
   sequence-graph/CrossFadingSmoother.h
   sequence-graph/ResamplingNode.cpp
   sequence-graph/ResamplingNode.h
   PlaybackDirection.h
   sequence-graph/ClipTimelineNode.cpp
   sequence-graph/ClipTimelineNode.h
   SequenceProcessingGraphFactory.cpp
   SequenceProcessingGraphFactory.h
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
   au3-xml
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
