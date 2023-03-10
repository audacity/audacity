/**********************************************************************

Audacity: A Digital Audio Editor

WaveformVRulerControls.cpp

Paul Licameli split from WaveTrackVRulerControls.cpp

**********************************************************************/

#include "WaveformVRulerControls.h"

#include "WaveformVZoomHandle.h"
#include "WaveTrackVRulerControls.h"

#include "NumberScale.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../UIHandle.h"
#include "WaveTrack.h"
#include "../../../../prefs/WaveformSettings.h"
#include "../../../../widgets/Ruler.h"
#include "../../../../widgets/LinearUpdater.h"
#include "../../../../widgets/RealFormat.h"
#include "../../../../widgets/CustomUpdaterValue.h"

WaveformVRulerControls::~WaveformVRulerControls() = default;

// These are doubles beacuse of the type of value in Label,
// but for the purpose of labelling the linear dB waveform ruler,
// these should always be integer numbers.
using LinearDBValues = std::vector<double>;

static LinearDBValues majorValues{}, minorValues{}, minorMinorValues{};

void RegenerateLinearDBValues(int dBRange, float min, float max, int height)
{
   majorValues.clear();
   minorValues.clear();
   minorMinorValues.clear();

   majorValues.push_back(0);
   majorValues.push_back(-dBRange);
   majorValues.push_back(2 * -dBRange);

   const double EPSILON = .1e-5;

   // No marks allowed within CENTER_SPACING pixels from the center
   // Calculate the closest allowed major / minor and remove everything around those
   const double CENTER = height / 2;
   const int CENTER_SPACING = 30;
   const double SIZE_SCALE = (max - min) / 2;

   double centerSpacingMark = 0;

   for (double major = 0.1; major < 2; major += .1) {
      double val = std::round(major * 10) / 10;
      double mappedVal = std::trunc(-dBRange * val);
      double pixVal = mappedVal < -dBRange ? - 2 * dBRange - mappedVal : mappedVal;
      double pixDist = fabs(CENTER - ((1 - DB_TO_LINEAR(pixVal)) * CENTER)) / SIZE_SCALE;
      if (pixDist > CENTER_SPACING) {
         if (major - EPSILON <= 1)
            centerSpacingMark = major;
         if (fabs(major - 1) > EPSILON)
            majorValues.push_back(mappedVal);
      }
   }
   for (double minor = 0.05; minor <= 1.95 + EPSILON; minor += .1) {
      double val = std::round(minor * 100) / 100;
      double mappedVal = std::trunc(-dBRange * val);
      double spacing = fabs(fabs(1 - minor) - 1);
      if (spacing < centerSpacingMark) {
         minorValues.push_back(mappedVal);
      }
   }
   for (int minorMinor = 1; minorMinor < 2 * dBRange; minorMinor++) {
      double absDist = fabs(fabs(dBRange - minorMinor) - dBRange) / dBRange;
      if (absDist < centerSpacingMark) {
         if ((minorMinor % (int)std::round(dBRange / 20)) != 0) {
            minorMinorValues.push_back(-minorMinor);
         }
      }
   }

}

std::vector<UIHandlePtr> WaveformVRulerControls::HitTest(
   const TrackPanelMouseState &st,
   const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto pTrack = FindTrack()->SharedPointer<WaveTrack>(  );
      if (pTrack) {
         auto result = std::make_shared<WaveformVZoomHandle>(
            pTrack, st.rect, st.state.m_y );
         result = AssignUIHandlePtr(mVZoomHandle, result);
         results.push_back(result);
      }
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
}

unsigned WaveformVRulerControls::HandleWheelRotation(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const auto pTrack = FindTrack();
   if (!pTrack)
      return RefreshNone;
   const auto wt = static_cast<WaveTrack*>(pTrack.get());
   return DoHandleWheelRotation( evt, pProject, wt );
}

namespace {
void SetLastdBRange(
   WaveformScale &cache, const WaveTrack &track)
{
   cache.SetLastDBRange(WaveformSettings::Get(track).dBRange);
}

void SetLastScaleType(
   WaveformScale &cache, const WaveTrack &track)
{
   cache.SetLastScaleType(WaveformSettings::Get(track).scaleType);
}
}

unsigned WaveformVRulerControls::DoHandleWheelRotation(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject, WaveTrack *wt)
{
   using namespace RefreshCode;
   const wxMouseEvent &event = evt.event;

   if (!(event.ShiftDown() || event.CmdDown()))
      return RefreshNone;

   // Always stop propagation even if the ruler didn't change.  The ruler
   // is a narrow enough target.
   evt.event.Skip(false);

   auto steps = evt.steps;

   using namespace WaveTrackViewConstants;
   auto &settings = WaveformSettings::Get(*wt);
   auto &cache = WaveformScale::Get(*wt);
   const bool isDB = !settings.isLinear();
   // Special cases for Waveform (logarithmic) dB only.
   // Set the bottom of the dB scale but only if it's visible
   if (isDB && event.ShiftDown() && event.CmdDown()) {
      float min, max;
      cache.GetDisplayBounds(min, max);
      if (!(min < 0.0 && max > 0.0))
         return RefreshNone;

      float olddBRange = settings.dBRange;
      for (auto channel : TrackList::Channels(wt)) {
         auto &channelSettings = WaveformSettings::Get(*channel);
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
      const auto zeroLevel = cache.ZeroLevelYCoordinate(rect);
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
            auto &cache = WaveformScale::Get(*channel);
            SetLastdBRange(cache, *channel);
            cache.SetDisplayBounds(min, max);
         }
      }
   }
   else if (event.CmdDown() && !event.ShiftDown()) {
      const int yy = event.m_y;
      WaveformVZoomHandle::DoZoom(
         pProject, wt,
         (steps < 0)
            ? kZoomOut
            : kZoomIn,
         evt.rect, yy, yy, true);
   }
   else if (!event.CmdDown() && event.ShiftDown()) {
      // Scroll some fixed number of pixels, independent of zoom level or track height:
      static const float movement = 10.0f;
      const int height = evt.rect.GetHeight();
      {
         float topLimit = 2.0;
         if (isDB) {
            const float dBRange = WaveformSettings::Get(*wt).dBRange;
            topLimit = (LINEAR_TO_DB(topLimit) + dBRange) / dBRange;
         }
         const float bottomLimit = -topLimit;
         float top, bottom;
         auto &cache = WaveformScale::Get(*wt);
         cache.GetDisplayBounds(bottom, top);
         const float range = top - bottom;
         const float delta = range * steps * movement / height;
         float newTop = std::min(topLimit, top + delta);
         const float newBottom = std::max(bottomLimit, newTop - range);
         newTop = std::min(topLimit, newBottom + range);
         for (auto channel : TrackList::Channels(wt))
            WaveformScale::Get(*channel)
               .SetDisplayBounds(newBottom, newTop);
      }
   }
   else
      return RefreshNone;

   ProjectHistory::Get( *pProject ).ModifyState(true);

   return RefreshCell | UpdateVRuler;
}

void WaveformVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   TrackVRulerControls::Draw( context, rect_, iPass );
   WaveTrackVRulerControls::DoDraw( *this, context, rect_, iPass );
}

void WaveformVRulerControls::UpdateRuler( const wxRect &rect )
{
   const auto wt = std::static_pointer_cast< WaveTrack >( FindTrack() );
   if (!wt)
      return;
   DoUpdateVRuler( rect, wt.get() );
}

static CustomUpdaterValue updater;

void WaveformVRulerControls::DoUpdateVRuler(
   const wxRect &rect, const WaveTrack *wt )
{
   auto vruler = &WaveTrackVRulerControls::ScratchRuler();

   // All waves have a ruler in the info panel
   // The ruler needs a bevelled surround.
   auto &settings = WaveformSettings::Get(*wt);
   const float dBRange = settings.dBRange;

   float min, max;
   auto &cache = WaveformScale::Get(*wt);
   cache.GetDisplayBounds(min, max);
   const float lastdBRange = cache.GetLastDBRange();
   if (dBRange != lastdBRange)
      SetLastdBRange(cache, *wt);

   auto scaleType = settings.scaleType;
   
   if (settings.isLinear()) {
      // Waveform
      
      if (cache.GetLastScaleType() != WaveformSettings::stLinearAmp &&
          cache.GetLastScaleType() != WaveformSettings::stLinearDb &&
          cache.GetLastScaleType() != -1)
      {
         // do a translation into the linear space
         SetLastScaleType(cache, *wt);
         SetLastdBRange(cache, *wt);
         float sign = (min >= 0 ? 1 : -1);
         if (min != 0.) {
            min = DB_TO_LINEAR(fabs(min) * dBRange - dBRange);
            if (min < 0.0)
               min = 0.0;
            min *= sign;
         }
         sign = (max >= 0 ? 1 : -1);
         
         if (max != 0.) {
            max = DB_TO_LINEAR(fabs(max) * dBRange - dBRange);
            if (max < 0.0)
               max = 0.0;
            max *= sign;
         }
         cache.SetDisplayBounds(min, max);
      }
      
      vruler->SetDbMirrorValue( 0.0 );
      vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height - 1);
      vruler->SetOrientation(wxVERTICAL);
      vruler->SetRange(max, min);
      vruler->SetFormat(&RealFormat::LinearInstance());
      if (scaleType == WaveformSettings::stLinearAmp) {
         vruler->SetLabelEdges(false);
         vruler->SetUnits({});
         vruler->SetUpdater(&LinearUpdater::Instance());
      }
      else {
         RegenerateLinearDBValues(dBRange, min, max, rect.GetHeight());
         vruler->SetLabelEdges(true);
         vruler->SetUnits(XO("dB"));
         vruler->SetUpdater(&updater);
         RulerUpdater::Labels major, minor, minorMinor;
         std::vector<LinearDBValues> values = { majorValues, minorValues, minorMinorValues };
         for (int ii = 0; ii < 3; ii++) {
            RulerUpdater::Labels labs;
            int size = (ii == 0) ? majorValues.size() :
               (ii == 1) ? minorValues.size() : minorMinorValues.size();
            for (int i = 0; i < size; i++) {
               double value = (ii == 0) ? majorValues[i] :
                  (ii == 1) ? minorValues[i] : minorMinorValues[i];
               RulerUpdater::Label lab;

               if (value == -dBRange)
                  lab.value = 0;
               else {
                  float sign = (value > -dBRange ? 1 : -1);
                  if (value < -dBRange)
                     value = -2 * dBRange - value;
                  lab.value = DB_TO_LINEAR(value) * sign;
               }

               wxString s = (value == -dBRange) ?
                  wxString(L"-\u221e") : wxString::FromDouble(value);
               // \u221e represents the infinity symbol
               // Should this just be -dBRange so it is consistent?
               //wxString s = wxString::FromDouble(value);
               lab.text = Verbatim(s);

               labs.push_back(lab);
            }
            if (ii == 0)
               major = labs;
            else if (ii == 1)
               minor = labs;
            else
               minorMinor = labs;
         }
         updater.SetData(major, minor, minorMinor);
      }
   }
   else {
      vruler->SetUnits({});
      
      auto &cache = WaveformScale::Get(*wt);
      
      if (cache.GetLastScaleType() != WaveformSettings::stLogarithmicDb &&
         // When Logarithmic Amp happens, put that here
          cache.GetLastScaleType() != -1)
      {
         // do a translation into the dB space
         SetLastScaleType(cache, *wt);
         SetLastdBRange(cache, *wt);
         float sign = (min >= 0 ? 1 : -1);
         if (min != 0.) {
            min = (LINEAR_TO_DB(fabs(min)) + dBRange) / dBRange;
            if (min < 0.0)
               min = 0.0;
            min *= sign;
         }
         sign = (max >= 0 ? 1 : -1);
         
         if (max != 0.) {
            max = (LINEAR_TO_DB(fabs(max)) + dBRange) / dBRange;
            if (max < 0.0)
               max = 0.0;
            max *= sign;
         }
         cache.SetDisplayBounds(min, max);
      }
      else if (dBRange != lastdBRange) {
         SetLastdBRange(cache, *wt);
         // Remap the max of the scale
         float newMax = max;
         
         // This commented out code is problematic.
         // min and max may be correct, and this code cause them to change.
#ifdef ONLY_LABEL_POSITIVE
         const float sign = (max >= 0 ? 1 : -1);
         if (max != 0.) {
            
            // Ugh, duplicating from TrackPanel.cpp
#define ZOOMLIMIT 0.001f
            
            const float extreme = LINEAR_TO_DB(2);
            // recover dB value of max
            const float dB = std::min(extreme, (float(fabs(max)) * lastdBRange - lastdBRange));
            // find NEW scale position, but old max may get trimmed if the db limit rises
            // Don't trim it to zero though, but leave max and limit distinct
            newMax = sign * std::max(ZOOMLIMIT, (dBRange + dB) / dBRange);
            // Adjust the min of the scale if we can,
            // so the db Limit remains where it was on screen, but don't violate extremes
            if (min != 0.)
               min = std::max(-extreme, newMax * min / max);
         }
#endif
         cache.SetDisplayBounds(min, newMax);
      }
      
      // Old code was if ONLY_LABEL_POSITIVE were defined.
      // it uses the +1 to 0 range only.
      // the enabled code uses +1 to -1, and relies on set ticks labelling knowing about
      // the dB scale.
#ifdef ONLY_LABEL_POSITIVE
      if (max > 0) {
#endif
         int top = 0;
         float topval = 0;
         int bot = rect.height;
         float botval = -dBRange;
         
#ifdef ONLY_LABEL_POSITIVE
         if (min < 0) {
            bot = top + (int)((max / (max - min))*(bot - top));
            min = 0;
         }
         
         if (max > 1) {
            top += (int)((max - 1) / (max - min) * (bot - top));
            max = 1;
         }
         
         if (max < 1 && max > 0)
            topval = -((1 - max) * dBRange);
         
         if (min > 0) {
            botval = -((1 - min) * dBRange);
         }
#else
         topval = -((1 - max) * dBRange);
         botval = -((1 - min) * dBRange);
         vruler->SetDbMirrorValue( dBRange );
#endif
         vruler->SetBounds(rect.x, rect.y + top, rect.x + rect.width, rect.y + bot - 1);
         vruler->SetOrientation(wxVERTICAL);
         vruler->SetRange(topval, botval);
#ifdef ONLY_LABEL_POSITIVE
      }
      else
         vruler->SetBounds(0.0, 0.0, 0.0, 0.0); // A.C.H I couldn't find a way to just disable it?
#endif
      vruler->SetFormat(&RealFormat::LogInstance());
      vruler->SetLabelEdges(true);
      vruler->SetUpdater(&LinearUpdater::Instance());
   }
   vruler->GetMaxSize( &wt->vrulerSize.first, &wt->vrulerSize.second );
}
