/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "NoteTrackVRulerControls.h"
#include "../../../../HitTestResult.h"

NoteTrackVRulerControls::NoteTrackVRulerControls()
   : TrackVRulerControls()
{
}

NoteTrackVRulerControls &NoteTrackVRulerControls::Instance()
{
   static NoteTrackVRulerControls instance;
   return instance;
}

NoteTrackVRulerControls::~NoteTrackVRulerControls()
{
}

HitTestResult NoteTrackVRulerControls::HitTest
(const TrackPanelMouseEvent &,
 const AudacityProject *)
{
   return {};
}

#endif