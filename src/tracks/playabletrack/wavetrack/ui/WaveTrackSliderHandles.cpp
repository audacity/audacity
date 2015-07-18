#include "WaveTrackSliderHandles.h"

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

GainSliderHandle::GainSliderHandle()
   : SliderHandle()
{
}

GainSliderHandle::~GainSliderHandle()
{
}

GainSliderHandle &GainSliderHandle::Instance()
{
   static GainSliderHandle instance;
   return instance;
}

WaveTrack *GainSliderHandle::GetTrack()
{
   return static_cast<WaveTrack*>(mpTrack);
}

float GainSliderHandle::GetValue()
{
   return static_cast<WaveTrack*>(mpTrack)->GetGain();
}

UIHandle::Result GainSliderHandle::SetValue
(AudacityProject *pProject, float newValue)
{
   GetTrack()->SetGain(newValue);

   // Assume linked track is wave or null
   const auto link = static_cast<WaveTrack*>(mpTrack->GetLink());
   if (link)
      link->SetGain(newValue);

   MixerBoard *const pMixerBoard = pProject->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->UpdateGain(GetTrack());

   return RefreshCode::RefreshNone;
}

UIHandle::Result GainSliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   pProject->PushState(_("Moved gain slider"), _("Gain"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

HitTestResult GainSliderHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect,
 const AudacityProject *pProject, Track *pTrack)
{
   if (!event.Button(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetGainRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(event.m_x, event.m_y)) {
      WaveTrack *const wavetrack = static_cast<WaveTrack*>(pTrack);
      wxRect sliderRect;
      TrackInfo::GetGainRect(rect.GetTopLeft(), sliderRect);
      auto slider = TrackInfo::GainSlider
         (sliderRect, wavetrack, true,
          const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      Instance().mpSlider = slider;
      Instance().mpTrack = wavetrack;
      return {
         HitPreview(),
         &Instance()
      };
   }
   else
      return {};
}

////////////////////////////////////////////////////////////////////////////////

PanSliderHandle::PanSliderHandle()
   : SliderHandle()
{
}

PanSliderHandle::~PanSliderHandle()
{
}

PanSliderHandle &PanSliderHandle::Instance()
{
   static PanSliderHandle instance;
   return instance;
}

WaveTrack *PanSliderHandle::GetTrack()
{
   return static_cast<WaveTrack*>(mpTrack);
}

float PanSliderHandle::GetValue()
{
   return GetTrack()->GetPan();
}

UIHandle::Result PanSliderHandle::SetValue(AudacityProject *pProject, float newValue)
{
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   bool panZero = false;
   panZero = static_cast<WaveTrack*>(mpTrack)->SetPan(newValue);
#else
   mpTrack->SetPan(newValue);
#endif

   // Assume linked track is wave or null
   const auto link = static_cast<WaveTrack*>(mpTrack->GetLink());
   if (link)
      link->SetPan(newValue);

   MixerBoard *const pMixerBoard = pProject->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->UpdatePan(GetTrack());

   using namespace RefreshCode;
   Result result = RefreshNone;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(panZero)
      result |= FixScrollbars;
#endif

   return result;
}

UIHandle::Result PanSliderHandle::CommitChanges
(const wxMouseEvent &, AudacityProject *pProject)
{
   pProject->PushState(_("Moved pan slider"), _("Pan"), UndoPush::CONSOLIDATE);
   return RefreshCode::RefreshCell;
}

HitTestResult PanSliderHandle::HitTest
(const wxMouseEvent &event, const wxRect &rect,
 const AudacityProject *pProject, Track *pTrack)
{
   if (!event.Button(wxMOUSE_BTN_LEFT))
      return {};

   wxRect sliderRect;
   TrackInfo::GetPanRect(rect.GetTopLeft(), sliderRect);
   if ( TrackInfo::HideTopItem( rect, sliderRect, kTrackInfoSliderAllowance ) )
      return {};
   if (sliderRect.Contains(event.m_x, event.m_y)) {
      WaveTrack *const wavetrack = static_cast<WaveTrack*>(pTrack);
      auto slider = TrackInfo::PanSlider
         (sliderRect, wavetrack, true,
          const_cast<TrackPanel*>(pProject->GetTrackPanel()));
      Instance().mpSlider = slider;
      Instance().mpTrack = wavetrack;
      return {
         HitPreview(),
         &Instance()
      };
   }
   else
      return {};
}
