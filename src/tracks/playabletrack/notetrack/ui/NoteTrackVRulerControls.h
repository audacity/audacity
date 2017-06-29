/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VRULER_CONTROLS__
#define __AUDACITY_NOTE_TRACK_VRULER_CONTROLS__

#include "../../../ui/TrackVRulerControls.h"

class NoteTrackVRulerControls final : public TrackVRulerControls
{
   NoteTrackVRulerControls(const NoteTrackVRulerControls&) = delete;
   NoteTrackVRulerControls &operator=(const NoteTrackVRulerControls&) = delete;

public:
   explicit
   NoteTrackVRulerControls( std::shared_ptr<Track> pTrack )
      : TrackVRulerControls( pTrack ) {}
   ~NoteTrackVRulerControls();

   HitTestResult HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   unsigned HandleWheelRotation
      (const TrackPanelMouseEvent &event,
       AudacityProject *pProject) override;
};

#endif
