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
   au3-project-rate-interface
   au3-sample-track-interface
   au3-stretching-sequence-interface
   au3-track-selection-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

# this is a temporary hack to access the muse_framework from au3 code
include_directories(../../../muse_framework)
