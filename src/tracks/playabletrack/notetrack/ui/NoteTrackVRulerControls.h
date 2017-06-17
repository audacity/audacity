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
   NoteTrackVRulerControls();
   NoteTrackVRulerControls(const NoteTrackVRulerControls&) = delete;
   NoteTrackVRulerControls &operator=(const NoteTrackVRulerControls&) = delete;

public:
   static NoteTrackVRulerControls &Instance();
   ~NoteTrackVRulerControls();

   HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *pProject) override;
};

#endif
