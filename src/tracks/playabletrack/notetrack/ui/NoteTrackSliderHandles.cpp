/**********************************************************************

 Audacity: A Digital Audio Editor

 NoteTrackSliderHandles.cpp

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#include "../../../../Audacity.h"
#include "NoteTrackSliderHandles.h"

#include "../../../../Experimental.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanel.h" // for TrackInfo
#include "../../../../UndoManager.h"
#include "../../../../NoteTrack.h"

VelocitySliderHandle::VelocitySliderHandle
( SliderFn sliderFn, const wxRect &rect,
  const std::shared_ptr<Track> &pTrack )
   : SliderHandle{ sliderFn, rect, pTrack }
{}

VelocitySliderHandle::~VelocitySliderHandle()
{
}

std::shared_ptr<NoteTrack> VelocitySliderHandle::GetNoteTrack()
{
   return std::static_pointer_cast<NoteTrack>(mpTrack.lock());
}

float VelocitySliderHandle::GetValue()
{
   if (GetNoteTrack())
      return GetNoteTrack()->GetVelocity();
   else
      return 0;
}

UIHandle::Result VelocitySliderHandle::SetValue
(AudacityProject *pProject, float newValue)
{
   (void)pProject;//Compiler food
   auto pTrack = GetNoteTrack();

   if (pTrack) {
      pTrack->SetVelocity(newValue);
   }

   return RefreshCode::RefreshCell;
}

UIHandle::Result VelocitySliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   pProject->PushState(_("Moved velocity slider"), _("Velocity"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}



UIHandlePtr VelocitySliderHandle::HitTest
(std::weak_ptr<VelocitySliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetVelocityRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return TrackInfo::VelocitySlider
            (sliderRect, static_cast<NoteTrack*>( pTrack ), true,
             const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      };
      auto result = std::make_shared<VelocitySliderHandle>(
         sliderFn, sliderRect, pTrack );
      result = AssignUIHandlePtr(holder, result);

      return result;
   }
   else
      return {};
}

#endif
