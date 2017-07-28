/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackSliderHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../MixerBoard.h"
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
   auto pTrack = GetWaveTrack();

   if (pTrack) {
      pTrack->SetGain(newValue);

      // Assume linked track is wave or null
      const auto link = static_cast<WaveTrack*>(mpTrack.lock()->GetLink());
      if (link)
         link->SetGain(newValue);

      MixerBoard *const pMixerBoard = pProject->GetMixerBoard();
      if (pMixerBoard)
         pMixerBoard->UpdateGain(pTrack.get());
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
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(state.m_x, state.m_y)) {
      wxRect sliderRect;
      TrackInfo::GetGainRect(rect.GetTopLeft(), sliderRect);
      auto sliderFn =
      []( AudacityProject *pProject, const wxRect &sliderRect, Track *pTrack ) {
         return TrackInfo::GainSlider
            (sliderRect, static_cast<WaveTrack*>( pTrack ), true,
             const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      };
      auto result =
         std::make_shared<GainSliderHandle>( sliderFn, sliderRect, pTrack );
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
   using namespace RefreshCode;
   Result result = RefreshNone;
   auto pTrack = GetWaveTrack();

   if (pTrack) {
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      bool panZero = false;
      panZero = static_cast<WaveTrack*>(mpTrack)->SetPan(newValue);
#else
      pTrack->SetPan(newValue);
#endif

      // Assume linked track is wave or null
      const auto link = static_cast<WaveTrack*>(pTrack->GetLink());
      if (link)
         link->SetPan(newValue);

      MixerBoard *const pMixerBoard = pProject->GetMixerBoard();
      if (pMixerBoard)
         pMixerBoard->UpdatePan(pTrack.get());

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(panZero)
         result |= FixScrollbars;
#endif
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
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
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
