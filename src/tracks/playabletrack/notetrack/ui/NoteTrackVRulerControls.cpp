/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros

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

std::vector<UIHandlePtr> NoteTrackVRulerControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;
   UIHandlePtr result;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto track = std::static_pointer_cast<NoteTrack>(FindTrack());
      result = NoteTrackVZoomHandle::HitTest(
         mVZoomHandle, st.state, track, st.rect);
      if (result)
         results.push_back(result);
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
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

   auto steps = evt.steps;
   const auto nt = static_cast<NoteTrack*>(pTrack.get());

   if (event.CmdDown() && !event.ShiftDown()) {
      if (steps > 0)
         nt->ZoomIn(evt.rect, evt.event.m_y);
      else
         nt->ZoomOut(evt.rect, evt.event.m_y);
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
