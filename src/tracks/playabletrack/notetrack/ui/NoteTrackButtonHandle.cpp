/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackButtonHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "NoteTrackButtonHandle.h"

#include "../../../../HitTestResult.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../NoteTrack.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanel.h"

NoteTrackButtonHandle::NoteTrackButtonHandle()
{
}

NoteTrackButtonHandle::~NoteTrackButtonHandle()
{
}

NoteTrackButtonHandle &NoteTrackButtonHandle::Instance()
{
   static NoteTrackButtonHandle instance;
   return instance;
}

HitTestResult NoteTrackButtonHandle::HitTest
   (const wxMouseState &state, const wxRect &rect,
    const std::shared_ptr<NoteTrack> &pTrack)
{
   wxRect midiRect;
   TrackInfo::GetMidiControlsRect(rect, midiRect);
   if ( TrackInfo::HideTopItem( rect, midiRect ) )
      return {};
   if (pTrack->GetKind() == Track::Note &&
       midiRect.Contains(state.m_x, state.m_y)) {
         Instance().mpTrack = pTrack;
         Instance().mRect = midiRect;
         return {
            HitTestPreview(),
            &Instance()
         };
   }
   else
      return {};
}

UIHandle::Result NoteTrackButtonHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result NoteTrackButtonHandle::Drag
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

HitTestPreview NoteTrackButtonHandle::Preview
(const TrackPanelMouseState &, const AudacityProject *)
{
   // No special message or cursor
   return {};
}

UIHandle::Result NoteTrackButtonHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject, wxWindow *)
{
   using namespace RefreshCode;

   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   if (pTrack->LabelClick(mRect, event.m_x, event.m_y,
      event.Button(wxMOUSE_BTN_RIGHT))) {
      // No undo items needed??
      pProject->ModifyState(false);
      return RefreshAll;
   }
   return RefreshNone;
}

UIHandle::Result NoteTrackButtonHandle::Cancel(AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

