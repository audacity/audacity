/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_CONTROLS__
#define __AUDACITY_NOTE_TRACK_CONTROLS__

#include "../../../ui/TrackControls.h"

class NoteTrackControls : public TrackControls
{
   NoteTrackControls(const NoteTrackControls&) = delete;
   NoteTrackControls &operator=(const NoteTrackControls&) = delete;

public:
   explicit
   NoteTrackControls( std::shared_ptr<Track> pTrack )
      : TrackControls( pTrack ) {}
   ~NoteTrackControls();

   HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;
};

#endif
