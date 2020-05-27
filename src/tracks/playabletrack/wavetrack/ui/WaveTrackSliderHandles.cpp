/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackSliderHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackSliderHandles.h"

#include "WaveTrackControls.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackInfo.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelAx.h"
#include "../../../../UndoManager.h"
#include "../../../../WaveTrack.h"

GainSliderHandle::GainSliderHandle
( SliderFn sliderFn, const wxRect &rect, const std::shared_ptr<Track> &pTrack )
   : SliderHandle{ sliderFn, rect, pTrack }
{}

GainSliderHandle::~GainSliderHandle()
{
}

std::shared_ptr<WaveTrack> GainSliderHandle::GetWaveTrack() const
{
   return std::static_pointer_cast<WaveTrack>(mpTrack.lock());
}

float GainSliderHandle::GetValue()
{
   if (GetWaveTrack())
      return GetWaveTrack()->GetGain();
   else
      return 0;
}

UIHandle::Result GainSliderHandle::SetValue
(AudacityProject *pProject, float newValue)
{
   (void)pProject;//Compiler food
   auto pTrack = GetWaveTrack();

   if (pTrack) {
      for (auto channel :
           TrackList::Channels(pTrack.get()))
         channel->SetGain(newValue);
   }

   return RefreshCode::RefreshNone;
}

UIHandle::Result GainSliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   ProjectHistory::Get( *pProject )
      .PushState(XO("Moved gain slider"), XO("Gain"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

TranslatableString GainSliderHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   TranslatableString val;
   float value = 0;

   auto pTrack = GetWaveTrack();
   if (pTrack)
      value = pTrack->GetGain();

   // LLL: Can't access the slider since Tip() is a const method and getting the slider
   //      is not, so duplicate what LWSlider does.

   /* i18n-hint dB abbreviates decibels */
   val = XO("%+.1f dB").Format(LINEAR_TO_DB(value));

   /* i18n-hint: An item name followed by a value, with appropriate separating punctuation */
   return XO("%s: %s").Format(XO("Gain"), val);
}

UIHandlePtr GainSliderHandle::HitTest
(std::weak_ptr<GainSliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   WaveTrackControls::GetGainRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect))
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      wxRect sliderRect2;
      WaveTrackControls::GetGainRect(rect.GetTopLeft(), sliderRect2);
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return WaveTrackControls::GainSlider
            (sliderRect, static_cast<WaveTrack*>( pTrack ), true,
             &TrackPanel::Get( *pProject ));
      };
      auto result =
         std::make_shared<GainSliderHandle>( sliderFn, sliderRect2, pTrack );
      result = AssignUIHandlePtr(holder, result);

      return result;
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

PanSliderHandle::PanSliderHandle
( SliderFn sliderFn, const wxRect &rect, const std::shared_ptr<Track> &pTrack )
   : SliderHandle{ sliderFn, rect, pTrack }
{}

PanSliderHandle::~PanSliderHandle()
{
}

std::shared_ptr<WaveTrack> PanSliderHandle::GetWaveTrack() const
{
   return std::static_pointer_cast<WaveTrack>(mpTrack.lock());
}

float PanSliderHandle::GetValue()
{
   if (GetWaveTrack())
      return GetWaveTrack()->GetPan();
   else
      return 0;
}

UIHandle::Result PanSliderHandle::SetValue(AudacityProject *pProject, float newValue)
{
   (void)pProject;//Compiler food
   using namespace RefreshCode;
   Result result = RefreshNone;
   auto pTrack = GetWaveTrack();

   if (pTrack) {
      for (auto channel :
           TrackList::Channels(pTrack.get()))
         channel->SetPan(newValue);
   }

   return result;
}

UIHandle::Result PanSliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   ProjectHistory::Get( *pProject )
      .PushState(XO("Moved pan slider"), XO("Pan"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

TranslatableString PanSliderHandle::Tip(
   const wxMouseState &, AudacityProject &project) const
{
   TranslatableString val;
   float value = 0.0;

   auto pTrack = GetWaveTrack();
   if (pTrack)
      value = pTrack->GetPan();

   // LLL: Can't access the slider since Tip() is a const method and getting the slider
   //      is not, so duplicate what LWSlider does.

   if (value == 0.0)
   {
      val = XO("Center");
   }
   else
   {
      const auto v = 100.0f * fabsf(value);
      if (value < 0.0)
         /* i18n-hint: Stereo pan setting */
         val = XO("%.0f%% Left").Format(v);
      else
         /* i18n-hint: Stereo pan setting */
         val = XO("%.0f%% Right").Format(v);
   }

   /* i18n-hint: An item name followed by a value, with appropriate separating punctuation */
   return XO("%s: %s").Format(XO("Pan"), val);
}

UIHandlePtr PanSliderHandle::HitTest
(std::weak_ptr<PanSliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   WaveTrackControls::GetPanRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect))
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return WaveTrackControls::PanSlider
            (sliderRect, static_cast<WaveTrack*>( pTrack ), true,
             &TrackPanel::Get( *pProject ));
      };
      auto result = std::make_shared<PanSliderHandle>(
         sliderFn, sliderRect, pTrack );
      result = AssignUIHandlePtr(holder, result);

      return result;
   }
   else
      return {};
}
