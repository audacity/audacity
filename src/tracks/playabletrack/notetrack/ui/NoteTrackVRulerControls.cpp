/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "NoteTrackVRulerControls.h"
#include "NoteTrackVZoomHandle.h"

#include "../../../../HitTestResult.h"
#include "../../../../NoteTrack.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"


///////////////////////////////////////////////////////////////////////////////
NoteTrackVRulerControls::~NoteTrackVRulerControls()
{
}

HitTestResult NoteTrackVRulerControls::HitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *)
{
   return NoteTrackVZoomHandle::HitTest(evt.event);
}

unsigned NoteTrackVRulerControls::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const wxMouseEvent &event = evt.event;

   if (!(event.ShiftDown() || event.CmdDown()))
      return RefreshNone;

   // Always stop propagation even if the ruler didn't change.  The ruler
   // is a narrow enough target.
   evt.event.Skip(false);

   const auto pTrack = FindTrack();
   if (!pTrack)
      return RefreshNone;
   wxASSERT(pTrack->GetKind() == Track::Note);
   auto steps = evt.steps;

   const auto nt = static_cast<NoteTrack*>(pTrack.get());
   if (event.CmdDown() && !event.ShiftDown()) {
      nt->Zoom(evt.rect, evt.event.m_y, (int) (steps), false);
   } else if (!event.CmdDown() && event.ShiftDown()) {
      // Scroll some fixed number of notes, independent of zoom level or track height:
      static const int movement = 6; // 6 semitones is half an octave
      nt->SetBottomNote(nt->GetBottomNote() + (int) (steps * movement));
   } else {
      return RefreshNone;
   }

   pProject->ModifyState(true);

   return RefreshCell | UpdateVRuler;
}

#endif
