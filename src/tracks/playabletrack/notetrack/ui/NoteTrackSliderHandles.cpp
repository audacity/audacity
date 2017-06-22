/**********************************************************************

 Audacity: A Digital Audio Editor

 NoteTrackSliderHandles.cpp

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#include "../../../../Audacity.h"
#include "NoteTrackSliderHandles.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include "../../../../HitTestResult.h"
#include "../../../../MixerBoard.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanel.h" // for TrackInfo
#include "../../../../UndoManager.h"
#include "../../../../NoteTrack.h"

VelocitySliderHandle::VelocitySliderHandle()
: SliderHandle()
{
}

VelocitySliderHandle::~VelocitySliderHandle()
{
}

VelocitySliderHandle &VelocitySliderHandle::Instance()
{
   static VelocitySliderHandle instance;
   return instance;
}

NoteTrack *VelocitySliderHandle::GetTrack()
{
   return static_cast<NoteTrack*>(mpTrack);
}

float VelocitySliderHandle::GetValue()
{
   return GetTrack()->GetVelocity();
}

UIHandle::Result VelocitySliderHandle::SetValue
(AudacityProject *pProject, float newValue)
{
   GetTrack()->SetVelocity(newValue);

   MixerBoard *const pMixerBoard = pProject->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->UpdateVelocity(GetTrack());

   return RefreshCode::RefreshCell;
}

UIHandle::Result VelocitySliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   pProject->PushState(_("Moved velocity slider"), _("Velocity"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}



HitTestResult VelocitySliderHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect,
 const AudacityProject *pProject, Track *pTrack)
{
   if (!event.Button(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetVelocityRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(event.m_x, event.m_y)) {
      Instance().mSliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return TrackInfo::VelocitySlider
            (sliderRect, static_cast<NoteTrack*>( pTrack ), true,
             const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      };
      Instance().mRect = sliderRect;
      Instance().mpTrack = pTrack;

      return { HitPreview(), &Instance() };
   }
   else
      return {};
}

#endif
