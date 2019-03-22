/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackButtonHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros

#ifdef USE_MIDI
#include "NoteTrackButtonHandle.h"

#include "../../../../HitTestResult.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../NoteTrack.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanel.h"

NoteTrackButtonHandle::NoteTrackButtonHandle
( const std::shared_ptr<NoteTrack> &pTrack,
  int channel, const wxRect &rect )
   : mpTrack{ pTrack }
   , mChannel{ channel }
   , mRect{ rect }
{
}

void NoteTrackButtonHandle::Enter(bool)
{
   mChangeHighlight = RefreshCode::RefreshCell;
}

NoteTrackButtonHandle::~NoteTrackButtonHandle()
{
}

UIHandle::Result NoteTrackButtonHandle::NeedChangeHighlight
(const NoteTrackButtonHandle &oldState, const NoteTrackButtonHandle &newState)
{
   if (oldState.GetChannel() != newState.GetChannel())
      // Repaint whenever the highlighted button is different
      return RefreshCode::RefreshCell;
   return 0;
}

UIHandlePtr NoteTrackButtonHandle::HitTest
   (std::weak_ptr<NoteTrackButtonHandle> &holder,
    const wxMouseState &state, const wxRect &rect,
    const std::shared_ptr<NoteTrack> &pTrack)
{
   wxRect midiRect;
   TrackInfo::GetMidiControlsRect(rect, midiRect);
   if ( TrackInfo::HideTopItem( rect, midiRect ) )
      return {};
   if (midiRect.Contains(state.m_x, state.m_y)) {
      auto channel = pTrack->FindChannel(midiRect, state.m_x, state.m_y);
      auto result = std::make_shared<NoteTrackButtonHandle>(
         pTrack, channel, midiRect );
      result = AssignUIHandlePtr(holder, result);
      return result;
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
   // auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   auto pTrack = mpTrack.lock();
   if ( !pTrack )
      return {};
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

#endif
