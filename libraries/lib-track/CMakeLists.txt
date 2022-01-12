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

Class Envelope represents piecewise linear or exponential curves, and is
sometimes used with an independent variable that is not time.  Perhaps a base
class Curve should be extracted from it and lowered into lib-math.

Class TimeWarper can describe various non-decreasing mappings of the timeline,
which describe what certain effects do to one track, so the warp might be
reapplied to another track that must stay synchronized.
]]

set( SOURCES
   Envelope.cpp
   Envelope.h
   TimeWarper.cpp
   TimeWarper.h
   Track.cpp
   Track.h
   TrackAttachment.cpp
   TrackAttachment.h
)
set( LIBRARIES
   lib-project-interface
   lib-xml-interface
   PRIVATE
   wxBase
)
audacity_library( lib-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
