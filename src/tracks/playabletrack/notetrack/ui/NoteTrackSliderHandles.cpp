/**********************************************************************

 Audacity: A Digital Audio Editor

 NoteTrackSliderHandles.cpp

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/


#include "NoteTrackSliderHandles.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include "NoteTrackControls.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackInfo.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelAx.h"
#include "../../../../UndoManager.h"
#include "../../../../NoteTrack.h"
#include "ViewInfo.h"

VelocitySliderHandle::VelocitySliderHandle
( SliderFn sliderFn, const wxRect &rect,
  const std::shared_ptr<Track> &pTrack )
   : SliderHandle{ sliderFn, rect, pTrack }
{}

VelocitySliderHandle::~VelocitySliderHandle()
{
}

std::shared_ptr<NoteTrack> VelocitySliderHandle::GetNoteTrack() const
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
   ProjectHistory::Get( *pProject )
      .PushState(XO("Moved velocity slider"), XO("Velocity"),
         UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

TranslatableString VelocitySliderHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   TranslatableString val;
   float value = 0;

   auto pTrack = GetNoteTrack();
   if (pTrack)
      value = pTrack->GetVelocity();

   // LLL: Can't access the slider since Tip() is a const method and getting the slider
   //      is not, so duplicate what LWSlider does.

   if (value > 0.0f)
      // Signed
      val = Verbatim("%+d").Format((int)value);
   else
      // Zero, or signed negative
      val = Verbatim("%d").Format((int)value);

   /* i18n-hint: An item name followed by a value, with appropriate separating punctuation */
   return XO("%s: %s").Format(XO("Velocity"), val);
}

UIHandlePtr VelocitySliderHandle::HitTest
(std::weak_ptr<VelocitySliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   NoteTrackControls::GetVelocityRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return NoteTrackControls::VelocitySlider
            (sliderRect, static_cast<NoteTrack*>( pTrack ), true,
             &TrackPanel::Get( *pProject ));
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
