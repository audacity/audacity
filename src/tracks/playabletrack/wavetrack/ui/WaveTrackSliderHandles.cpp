/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackSliderHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanel.h"
#include "../../../../UndoManager.h"
#include "../../../../WaveTrack.h"

GainSliderHandle::GainSliderHandle
( SliderFn sliderFn, const wxRect &rect, const std::shared_ptr<Track> &pTrack )
   : SliderHandle{ sliderFn, rect, pTrack }
{}

GainSliderHandle::~GainSliderHandle()
{
}

std::shared_ptr<WaveTrack> GainSliderHandle::GetWaveTrack()
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
   pProject->PushState(_("Moved gain slider"), _("Gain"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

UIHandlePtr GainSliderHandle::HitTest
(std::weak_ptr<GainSliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetGainRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect))
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      wxRect sliderRect2;
      TrackInfo::GetGainRect(rect.GetTopLeft(), sliderRect2);
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return TrackInfo::GainSlider
            (sliderRect, static_cast<WaveTrack*>( pTrack ), true,
             const_cast<TrackPanel*>(pProject->GetTrackPanel()));
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

std::shared_ptr<WaveTrack> PanSliderHandle::GetWaveTrack()
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
   pProject->PushState(_("Moved pan slider"), _("Pan"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

UIHandlePtr PanSliderHandle::HitTest
(std::weak_ptr<PanSliderHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const std::shared_ptr<Track> &pTrack)
{
   if (!state.ButtonIsDown(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetPanRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect))
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return TrackInfo::PanSlider
            (sliderRect, static_cast<WaveTrack*>( pTrack ), true,
             const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      };
      auto result = std::make_shared<PanSliderHandle>(
         sliderFn, sliderRect, pTrack );
      result = AssignUIHandlePtr(holder, result);

      return result;
   }
   else
      return {};
}
