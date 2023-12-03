#[[
The Track abstraction.  An abstract base class, whose subclasses associate
data of some kind with points on a timeline.  It supports attached objects
by inheriting ClientData::Site.  It supports persistency.

There is also Track::TypeSwitch() which accepts an ad-hoc "visitor" defined
as a sequence of lambda functions, and even allows for open-endedness of the
set of subclasses of Track.

The (per-project) TrackList object maintains a sequence of tracks, with an
interface for iteration over tracks, possibly filtered by subtype.  It emits
events when tracks are inserted, removed, or permuted, or when certain data
in individual tracks changes.

Class TimeWarper can describe various non-decreasing mappings of the timeline,
which describe what certain effects do to one track, so the warp might be
reapplied to another track that must stay synchronized.
]]

set( SOURCES
   ChannelAttachments.cpp
   ChannelAttachments.h
   PendingTracks.cpp
   PendingTracks.h
   TimeWarper.cpp
   TimeWarper.h
   Track.cpp
   Track.h
   TrackAttachment.cpp
   TrackAttachment.h
   UndoTracks.cpp
   UndoTracks.h
)
set( LIBRARIES
   lib-channel-interface
   lib-project-history-interface
)
audacity_library( lib-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
