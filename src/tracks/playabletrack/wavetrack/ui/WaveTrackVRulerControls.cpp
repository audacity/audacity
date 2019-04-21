/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackVRulerControls.h"

#include "WaveTrackVZoomHandle.h"

#include "../../../../HitTestResult.h"
#include "../../../../NumberScale.h"
#include "../../../../prefs/SpectrogramSettings.h"
#include "../../../../prefs/WaveformSettings.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"

///////////////////////////////////////////////////////////////////////////////
WaveTrackVRulerControls::~WaveTrackVRulerControls()
{
}

std::vector<UIHandlePtr> WaveTrackVRulerControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto pTrack = FindTrack()->SharedPointer<WaveTrack>(  );
      if (pTrack) {
         auto result = std::make_shared<WaveTrackVZoomHandle>(
            pTrack, st.rect, st.state.m_y );
         result = AssignUIHandlePtr(mVZoomHandle, result);
         results.push_back(result);
      }
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
}

void WaveTrackVRulerControls::DoZoomPreset( int i)
{

   const auto pTrack = FindTrack();
   if (!pTrack)
      return;

   const auto wt = static_cast<WaveTrack*>(pTrack.get());

   // Don't do all channels, that causes problems when updating display
   // during recording and there are special pending tracks.
   // This function implements WaveTrack::DoSetMinimized which is always
   // called in a context that loops over linked tracks too and reinvokes.
   WaveTrackVZoomHandle::DoZoom(
         NULL, wt, false, (i==1)?kZoomHalfWave: kZoom1to1,
         wxRect(0,0,0,0), 0,0, true);
}


unsigned WaveTrackVRulerControls::HandleWheelRotation
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
   const auto wt = static_cast<WaveTrack*>(pTrack.get());
   auto steps = evt.steps;

   const bool isDB =
      wt->GetDisplay() == WaveTrack::Waveform &&
      wt->GetWaveformSettings().scaleType == WaveformSettings::stLogarithmic;
   // Special cases for Waveform dB only.
   // Set the bottom of the dB scale but only if it's visible
   if (isDB && event.ShiftDown() && event.CmdDown()) {
      float min, max;
      wt->GetDisplayBounds(&min, &max);
      if (!(min < 0.0 && max > 0.0))
         return RefreshNone;

      WaveformSettings &settings =
         wt->GetIndependentWaveformSettings();
      float olddBRange = settings.dBRange;
      for (auto channel : TrackList::Channels(wt)) {
         WaveformSettings &channelSettings =
            channel->GetIndependentWaveformSettings();
         if (steps < 0)
            // Zoom out
            channelSettings.NextLowerDBRange();
         else
            channelSettings.NextHigherDBRange();
      }

      float newdBRange = settings.dBRange;

      // Is y coordinate within the rectangle half-height centered about
      // the zero level?
      const auto &rect = evt.rect;
      const auto zeroLevel = wt->ZeroLevelYCoordinate(rect);
      const bool fixedMagnification =
      (4 * std::abs(event.GetY() - zeroLevel) < rect.GetHeight());

      if (fixedMagnification) {
         // Vary the db limit without changing
         // magnification; that is, peaks and troughs move up and down
         // rigidly, as parts of the wave near zero are exposed or hidden.
         const float extreme = (LINEAR_TO_DB(2) + newdBRange) / newdBRange;
         max = std::min(extreme, max * olddBRange / newdBRange);
         min = std::max(-extreme, min * olddBRange / newdBRange);
         for (auto channel : TrackList::Channels(wt)) {
            channel->SetLastdBRange();
            channel->SetDisplayBounds(min, max);
         }
      }
   }
   else if (event.CmdDown() && !event.ShiftDown()) {
      const int yy = event.m_y;
      WaveTrackVZoomHandle::DoZoom(
         pProject, wt, true, (steps < 0)?kZoomOut:kZoomIn,
         evt.rect, yy, yy, true);
   }
   else if (!event.CmdDown() && event.ShiftDown()) {
      // Scroll some fixed number of pixels, independent of zoom level or track height:
      static const float movement = 10.0f;
      const int height = evt.rect.GetHeight();
      const bool spectral = (wt->GetDisplay() == WaveTrack::Spectrum);
      if (spectral) {
         const float delta = steps * movement / height;
         SpectrogramSettings &settings = wt->GetIndependentSpectrogramSettings();
         const bool isLinear = settings.scaleType == SpectrogramSettings::stLinear;
         float bottom, top;
         wt->GetSpectrumBounds(&bottom, &top);
         const double rate = wt->GetRate();
         const float bound = rate / 2;
         const NumberScale numberScale(settings.GetScale(bottom, top));
         float newTop =
            std::min(bound, numberScale.PositionToValue(1.0f + delta));
         const float newBottom =
            std::max((isLinear ? 0.0f : 1.0f),
                     numberScale.PositionToValue(numberScale.ValueToPosition(newTop) - 1.0f));
         newTop =
            std::min(bound,
                     numberScale.PositionToValue(numberScale.ValueToPosition(newBottom) + 1.0f));

         for (auto channel : TrackList::Channels(wt))
            channel->SetSpectrumBounds(newBottom, newTop);
      }
      else {
         float topLimit = 2.0;
         if (isDB) {
            const float dBRange = wt->GetWaveformSettings().dBRange;
            topLimit = (LINEAR_TO_DB(topLimit) + dBRange) / dBRange;
         }
         const float bottomLimit = -topLimit;
         float top, bottom;
         wt->GetDisplayBounds(&bottom, &top);
         const float range = top - bottom;
         const float delta = range * steps * movement / height;
         float newTop = std::min(topLimit, top + delta);
         const float newBottom = std::max(bottomLimit, newTop - range);
         newTop = std::min(topLimit, newBottom + range);
         for (auto channel : TrackList::Channels(wt))
            channel->SetDisplayBounds(newBottom, newTop);
      }
   }
   else
      return RefreshNone;

   pProject->ModifyState(true);

   return RefreshCell | UpdateVRuler;
}
