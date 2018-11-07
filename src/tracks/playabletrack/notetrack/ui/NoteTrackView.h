/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackView.h

Paul Licameli split from class NoteTrack

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VIEW__
#define __AUDACITY_NOTE_TRACK_VIEW__

#include "../../../ui/TrackView.h"

class NoteTrackView final : public TrackView
{
   NoteTrackView( const NoteTrackView& ) = delete;
   NoteTrackView &operator=( const NoteTrackView& ) = delete;

public:
   explicit
   NoteTrackView( const std::shared_ptr<Track> &pTrack )
      : TrackView{ pTrack } {}
   ~NoteTrackView() override;
};

#endif
