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

set( SOURCES
   PasteOverPreservingClips.cpp
   PasteOverPreservingClips.h
   SampleBlock.cpp
   SampleBlock.h
   Sequence.cpp
   Sequence.h
   WaveClip.cpp
   WaveClip.h
   WaveTrack.cpp
   WaveTrack.h
   WaveTrackSink.cpp
   WaveTrackSink.h
)
set( LIBRARIES
   lib-project-rate-interface
   lib-sample-track-interface
   lib-track-selection-interface
)
audacity_library( lib-wave-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
