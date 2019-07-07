/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"

#include "../../../../Experimental.h"

#include <wx/graphics.h>

#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

#include "WaveTrackControls.h"
#include "WaveTrackVRulerControls.h"

#include "SpectrumView.h"
#include "WaveformView.h"

#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../prefs/SpectrogramSettings.h"

#include "../../../ui/TimeShiftHandle.h"

WaveTrackView::WaveTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
   , mWaveformView{ std::make_shared< WaveformView >( pTrack ) }
   , mSpectrumView{ std::make_shared< SpectrumView >( pTrack ) }
{
   DoSetHeight( WaveTrackControls::DefaultWaveTrackHeight() );
}

WaveTrackView::~WaveTrackView()
{
}

std::vector<UIHandlePtr> WaveTrackView::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool)
{
   const auto pTrack = std::static_pointer_cast< WaveTrack >( FindTrack() );
   bool isWaveform = (pTrack->GetDisplay() == WaveTrackViewConstants::Waveform);
   if ( isWaveform )
      return WaveformView::DoDetailedHitTest(
         st, pProject, currentTool, bMultiTool, pTrack, *this);
   else
      return SpectrumView::DoDetailedHitTest(
         st, pProject, currentTool, bMultiTool, pTrack, *this);
}

std::pair< bool, std::vector<UIHandlePtr> >
WaveTrackView::DoDetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool,
 const std::shared_ptr<WaveTrack> &pTrack,
 CommonTrackView &view)
{
   // This is the only override of Track::DetailedHitTest that still
   // depends on the state of the Tools toolbar.
   // If that toolbar were eliminated, this could simplify to a sequence of
   // hit test routines describable by a table.

   UIHandlePtr result;
   std::vector<UIHandlePtr> results;

   if (bMultiTool && st.state.CmdDown()) {
      // Ctrl modifier key in multi-tool overrides everything else
      // (But this does not do the time shift constrained to the vertical only,
      //  which is what happens when you hold Ctrl in the Time Shift tool mode)
      result = TimeShiftHandle::HitAnywhere(
         view.mTimeShiftHandle, pTrack, false);
      if (result)
         results.push_back(result);
      return { true, results };
   }
   return { false, results };
}

void WaveTrackView::DoSetMinimized( bool minimized )
{
   auto wt = static_cast<WaveTrack*>( FindTrack().get() );

#ifdef EXPERIMENTAL_HALF_WAVE
   bool bHalfWave;
   gPrefs->Read(wxT("/GUI/CollapseToHalfWave"), &bHalfWave, false);
   if( bHalfWave )
   {
      const bool spectral =
         (wt->GetDisplay() == WaveTrackViewConstants::Spectrum);
      if ( spectral ) {
         // It is all right to set the top of scale to a huge number,
         // not knowing the track rate here -- because when retrieving the
         // value, then we pass in a sample rate and clamp it above to the
         // Nyquist frequency.
         constexpr auto max = std::numeric_limits<float>::max();
         const bool spectrumLinear =
            (wt->GetSpectrogramSettings().scaleType ==
               SpectrogramSettings::stLinear);
         // Zoom out full
         wt->SetSpectrumBounds( spectrumLinear ? 0.0f : 1.0f, max );
      }
      else {
         if (minimized)
            // Zoom to show fractionally more than the top half of the wave.
            wt->SetDisplayBounds( -0.01f, 1.0f );
         else
            // Zoom out full
            wt->SetDisplayBounds( -1.0f, 1.0f );
      }
   }
#endif

   TrackView::DoSetMinimized( minimized );
}

using DoGetWaveTrackView = DoGetView::Override< WaveTrack >;
template<> template<> auto DoGetWaveTrackView::Implementation() -> Function {
   return [](WaveTrack &track) {
      return std::make_shared<WaveTrackView>( track.SharedPointer() );
   };
}
static DoGetWaveTrackView registerDoGetWaveTrackView;

std::shared_ptr<TrackVRulerControls> WaveTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<WaveTrackVRulerControls>( shared_from_this() );
}

#undef PROFILE_WAVEFORM
#ifdef PROFILE_WAVEFORM
#ifdef __WXMSW__
#include <time.h>
#else
#include <sys/time.h>
#endif
double gWaveformTimeTotal = 0;
int gWaveformTimeCount = 0;

namespace {
   struct Profiler {
      Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv0);
#   else
         gettimeofday(&tv0, NULL);
#   endif
      }
      
      ~Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv1);
         double elapsed = _difftime64(tv1, tv0);
#   else
         gettimeofday(&tv1, NULL);
         double elapsed =
         (tv1.tv_sec + tv1.tv_usec*0.000001) -
         (tv0.tv_sec + tv0.tv_usec*0.000001);
#   endif
         gWaveformTimeTotal += elapsed;
         gWaveformTimeCount++;
         wxPrintf(wxT("Avg waveform drawing time: %f\n"),
                  gWaveformTimeTotal / gWaveformTimeCount);
      }
      
#   ifdef __WXMSW__
      __time64_t tv0, tv1;
#else
      struct timeval tv0, tv1;
#endif
   };
}
#endif

ClipParameters::ClipParameters
   (bool spectrum, const WaveTrack *track, const WaveClip *clip, const wxRect &rect,
   const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo)
{
   tOffset = clip->GetOffset();
   rate = clip->GetRate();

   h = zoomInfo.PositionToTime(0, 0
      , true
   );
   h1 = zoomInfo.PositionToTime(rect.width, 0
      , true
   );

   double sel0 = selectedRegion.t0();    //left selection bound
   double sel1 = selectedRegion.t1();    //right selection bound

   //If the track isn't selected, make the selection empty
   if (!track->GetSelected() &&
      (spectrum || !track->IsSyncLockSelected())) { // PRL: why was there a difference for spectrum?
      sel0 = sel1 = 0.0;
   }

   const double trackLen = clip->GetEndTime() - clip->GetStartTime();

   tpre = h - tOffset;                 // offset corrected time of
   //  left edge of display
   tpost = h1 - tOffset;               // offset corrected time of
   //  right edge of display

   const double sps = 1. / rate;            //seconds-per-sample

   // Determine whether we should show individual samples
   // or draw circular points as well
   averagePixelsPerSample = rect.width / (rate * (h1 - h));
   showIndividualSamples = averagePixelsPerSample > 0.5;

   // Calculate actual selection bounds so that t0 > 0 and t1 < the
   // end of the track
   t0 = (tpre >= 0.0 ? tpre : 0.0);
   t1 = (tpost < trackLen - sps * .99 ? tpost : trackLen - sps * .99);
   if (showIndividualSamples) {
      // adjustment so that the last circular point doesn't appear
      // to be hanging off the end
      t1 += 2. / (averagePixelsPerSample * rate);
   }

   // Make sure t1 (the right bound) is greater than 0
   if (t1 < 0.0) {
      t1 = 0.0;
   }

   // Make sure t1 is greater than t0
   if (t0 > t1) {
      t0 = t1;
   }

   // Use the WaveTrack method to show what is selected and 'should' be copied, pasted etc.
   ssel0 = std::max(sampleCount(0), spectrum
      ? sampleCount((sel0 - tOffset) * rate + .99) // PRL: why?
      : track->TimeToLongSamples(sel0 - tOffset)
   );
   ssel1 = std::max(sampleCount(0), spectrum
      ? sampleCount((sel1 - tOffset) * rate + .99) // PRL: why?
      : track->TimeToLongSamples(sel1 - tOffset)
   );

   //trim selection so that it only contains the actual samples
   if (ssel0 != ssel1 && ssel1 > (sampleCount)(0.5 + trackLen * rate)) {
      ssel1 = sampleCount( 0.5 + trackLen * rate );
   }

   // The variable "hiddenMid" will be the rectangle containing the
   // actual waveform, as opposed to any blank area before
   // or after the track, as it would appear without the fisheye.
   hiddenMid = rect;

   // If the left edge of the track is to the right of the left
   // edge of the display, then there's some unused area to the
   // left of the track.  Reduce the "hiddenMid"
   hiddenLeftOffset = 0;
   if (tpre < 0) {
      // Fix Bug #1296 caused by premature conversion to (int).
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , true);
      if( time64 < 0 )
         time64 = 0;
      hiddenLeftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      hiddenMid.x += hiddenLeftOffset;
      hiddenMid.width -= hiddenLeftOffset;
   }

   // If the right edge of the track is to the left of the the right
   // edge of the display, then there's some unused area to the right
   // of the track.  Reduce the "hiddenMid" rect by the
   // size of the blank area.
   if (tpost > t1) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , true);
      if( time64 < 0 )
         time64 = 0;
      const int hiddenRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      hiddenMid.width = std::max(0, hiddenRightOffset - hiddenLeftOffset);
   }
   // The variable "mid" will be the rectangle containing the
   // actual waveform, as distorted by the fisheye,
   // as opposed to any blank area before or after the track.
   mid = rect;

   // If the left edge of the track is to the right of the left
   // edge of the display, then there's some unused area to the
   // left of the track.  Reduce the "mid"
   leftOffset = 0;
   if (tpre < 0) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , false);
      if( time64 < 0 )
         time64 = 0;
      leftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      mid.x += leftOffset;
      mid.width -= leftOffset;
   }

   // If the right edge of the track is to the left of the the right
   // edge of the display, then there's some unused area to the right
   // of the track.  Reduce the "mid" rect by the
   // size of the blank area.
   if (tpost > t1) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , false);
      if( time64 < 0 )
         time64 = 0;
      const int distortedRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      mid.width = std::max(0, distortedRightOffset - leftOffset);
   }
}

void WaveTrackView::Reparent( const std::shared_ptr<Track> &parent )
{
   CommonTrackView::Reparent( parent );
   if ( mWaveformView )
      mWaveformView->Reparent( parent );
   if ( mSpectrumView )
      mSpectrumView->Reparent( parent );
}

void WaveTrackView::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      auto &dc = context.dc;
      const auto wt = std::static_pointer_cast<const WaveTrack>(
         FindTrack()->SubstitutePendingChangedTrack());

      for (const auto &clip : wt->GetClips()) {
         clip->ClearDisplayRect();
      }

      const auto artist = TrackArtist::Get( context );
      const auto hasSolo = artist->hasSolo;
      bool muted = (hasSolo || wt->GetMute()) &&
      !wt->GetSolo();
      
#if defined(__WXMAC__)
      wxAntialiasMode aamode = dc.GetGraphicsContext()->GetAntialiasMode();
      dc.GetGraphicsContext()->SetAntialiasMode(wxANTIALIAS_NONE);
#endif
      
      switch (wt->GetDisplay()) {
         case WaveTrackViewConstants::Waveform:
            WaveformView::DoDraw(context, wt.get(), rect, muted);
            break;
         case WaveTrackViewConstants::Spectrum:
            SpectrumView::DoDraw( context, wt.get(), rect );
            break;
         default:
            wxASSERT(false);
      }
      
#if defined(__WXMAC__)
      dc.GetGraphicsContext()->SetAntialiasMode(aamode);
#endif
   }
   CommonTrackView::Draw( context, rect, iPass );
}
