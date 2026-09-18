#[[
Defines a type of track that stores sample data.

The samples are organized into clips that may cover time intervals with gaps
between them and may have hidden sample data at each end, and also embedded
cut-lines which are nested clips.

Each clip stores an unbroken Sequence of samples at some rate.

The Sequence object (which does not know its rate) manages a list of SampleBlock
objects.  This is an abstract class.  They are produced by a SampleBlockFactory
object, also an abstract class. A function that is a factory of block factories
must be injected by the application at startup.

Some other utilities related to wave tracks are also included.  MixAndRender
and WaveTrackSink make new WaveTrack objects.
]]

set( TARGET au3-wave-track )

set( SOURCES
   SampleBlock.cpp
   SampleBlock.h
   Sequence.cpp
   Sequence.h
   TimeStretching.cpp
   TimeStretching.h
   WaveChannelUtilities.cpp
   WaveChannelUtilities.h
   WaveClip.cpp
   WaveClip.h
   WaveClipUtilities.cpp
   WaveClipUtilities.h
   WaveTrack.cpp
   WaveTrack.h
   WaveTrackSink.cpp
   WaveTrackSink.h
   WaveTrackUtilities.cpp
   WaveTrackUtilities.h
   WaveChannelViewConstants.cpp
   WaveChannelViewConstants.h
)
set( LIBRARIES
PUBLIC
   au3-audio-graph
   au3-channel
   au3-components
   au3-math
   au3-mixer
   au3-preferences
   au3-registries
   au3-sample-track
   au3-stretching-sequence
   au3-strings
   au3-utility
   au3-xml
PRIVATE
   au3-basic-ui
   au3-exceptions
   au3-project
   au3-project-rate
   au3-time-and-pitch
   au3-track
   au3-track-selection
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

# this is a temporary hack to access the muse_framework from au3 code
include_directories(../../../muse)
