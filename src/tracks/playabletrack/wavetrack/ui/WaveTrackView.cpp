/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"

#include "../../../../Experimental.h"

#include <wx/dcmemory.h>
#include <wx/graphics.h>

#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

#include "WaveTrackControls.h"
#include "WaveTrackVRulerControls.h"

#include "../../../../AColor.h"
#include "../../../../Envelope.h"
#include "../../../../EnvelopeEditor.h"
#include "../../../../HitTestResult.h"
#include "../../../../NumberScale.h"
#include "../../../../TrackInfo.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../prefs/SpectrogramSettings.h"
#include "../../../../prefs/WaveformSettings.h"

#include "CutlineHandle.h"
#include "../../../ui/SelectHandle.h"
#include "../../../ui/EnvelopeHandle.h"
#include "SampleHandle.h"
#include "../../../ui/TimeShiftHandle.h"
#include "../../../../ProjectSettings.h"

WaveTrackView::WaveTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
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
   // This is the only override of Track::DetailedHitTest that still
   // depends on the state of the Tools toolbar.
   // If that toolbar were eliminated, this could simplify to a sequence of
   // hit test routines describable by a table.

   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const auto pTrack = std::static_pointer_cast< WaveTrack >( FindTrack() );
   bool isWaveform = (pTrack->GetDisplay() == WaveTrackViewConstants::Waveform);

   if (bMultiTool && st.state.CmdDown()) {
      // Ctrl modifier key in multi-tool overrides everything else
      // (But this does not do the time shift constrained to the vertical only,
      //  which is what happens when you hold Ctrl in the Time Shift tool mode)
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, pTrack, false);
      if (result)
         results.push_back(result);
      return results;
   }

   // Some special targets are not drawn in spectrogram,
   // so don't hit them in such views.
   else if (isWaveform) {

      if (NULL != (result = CutlineHandle::HitTest(
         mCutlineHandle, st.state, st.rect,
         pProject, pTrack )))
         // This overriding test applies in all tools
         results.push_back(result);
      if (bMultiTool) {
         // Conditional hit tests
         // If Tools toolbar were eliminated, we would keep these
         // The priority of these, in case more than one might apply at one
         // point, seems arbitrary
         if (NULL != (result = EnvelopeHandle::WaveTrackHitTest(
            mEnvelopeHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
         if (NULL != (result = TimeShiftHandle::HitTest(
            mTimeShiftHandle, st.state, st.rect, pTrack )))
            // This is the hit test on the "grips" drawn left and
            // right in Multi only
            results.push_back(result);
         if (NULL != (result = SampleHandle::HitTest(
            mSampleHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
      }
      else {
         switch ( currentTool ) {
               // Unconditional hits appropriate to the tool
               // If tools toolbar were eliminated, we would eliminate these
            case ToolCodes::envelopeTool: {
               auto envelope = pTrack->GetEnvelopeAtX( st.state.m_x );
               result = EnvelopeHandle::HitAnywhere(
                  mEnvelopeHandle, envelope, false);
               break;
            }
            case ToolCodes::drawTool:
               result = SampleHandle::HitAnywhere(
                  mSampleHandle, st.state, pTrack );
               break;
            default:
               result = {};
               break;
         }
         if (result)
            results.push_back(result);
      }
   }

   return results;
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

namespace {

struct ClipParameters
{
   // Do a bunch of calculations common to waveform and spectrum drawing.
   ClipParameters
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

   double tOffset;
   double rate;
   double h; // absolute time of left edge of display
   double tpre; // offset corrected time of left edge of display
   double h1;
   double tpost; // offset corrected time of right edge of display

   // Calculate actual selection bounds so that t0 > 0 and t1 < the
   // end of the track
   double t0;
   double t1;

   double averagePixelsPerSample;
   bool showIndividualSamples;

   sampleCount ssel0;
   sampleCount ssel1;

   wxRect hiddenMid;
   int hiddenLeftOffset;

   wxRect mid;
   int leftOffset;
};

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif
   
struct WavePortion {
   wxRect rect;
   CONST double averageZoom;
   CONST bool inFisheye;
   WavePortion(int x, int y, int w, int h, double zoom, bool i)
      : rect(x, y, w, h), averageZoom(zoom), inFisheye(i)
   {}
};

void FindWavePortions
   (std::vector<WavePortion> &portions, const wxRect &rect, const ZoomInfo &zoomInfo,
    const ClipParameters &params)
{
   // If there is no fisheye, then only one rectangle has nonzero width.
   // If there is a fisheye, make rectangles for before and after
   // (except when they are squeezed to zero width), and at least one for inside
   // the fisheye.

   ZoomInfo::Intervals intervals;
   zoomInfo.FindIntervals(params.rate, intervals, rect.width, rect.x);
   ZoomInfo::Intervals::const_iterator it = intervals.begin(), end = intervals.end(), prev;
   wxASSERT(it != end && it->position == rect.x);
   const int rightmost = rect.x + rect.width;
   for (int left = rect.x; left < rightmost;) {
      while (it != end && it->position <= left)
         prev = it++;
      if (it == end)
         break;
      const int right = std::max(left, (int)(it->position));
      const int width = right - left;
      if (width > 0)
         portions.push_back(
            WavePortion(left, rect.y, width, rect.height,
                        prev->averageZoom, prev->inFisheye)
         );
      left = right;
   }
}

void DrawWaveformBackground(TrackPanelDrawingContext &context,
                                         int leftOffset, const wxRect &rect,
                                         const double env[],
                                         float zoomMin, float zoomMax,
                                         int zeroLevelYCoordinate,
                                         bool dB, float dBRange,
                                         double t0, double t1,
                                         bool bIsSyncLockSelected,
                                         bool highlightEnvelope)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &zoomInfo = *artist->pZoomInfo;

   // Visually (one vertical slice of the waveform background, on its side;
   // the "*" is the actual waveform background we're drawing
   //
   //1.0                              0.0                             -1.0
   // |--------------------------------|--------------------------------|
   //      ***************                           ***************
   //      |             |                           |             |
   //    maxtop        maxbot                      mintop        minbot

   int h = rect.height;
   int halfHeight = wxMax(h / 2, 1);
   int maxtop, lmaxtop = 0;
   int mintop, lmintop = 0;
   int maxbot, lmaxbot = 0;
   int minbot, lminbot = 0;
   bool sel, lsel = false;
   int xx, lx = 0;
   int l, w;

   const auto &blankBrush = artist->blankBrush;
   const auto &selectedBrush = artist->selectedBrush;
   const auto &unselectedBrush = artist->unselectedBrush;

   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(blankBrush);
   dc.DrawRectangle(rect);

   double time = zoomInfo.PositionToTime(0, -leftOffset), nextTime;
   for (xx = 0; xx < rect.width; ++xx, time = nextTime) {
      nextTime = zoomInfo.PositionToTime(xx + 1, -leftOffset);
      // First we compute the truncated shape of the waveform background.
      // If drawEnvelope is true, then we compute the lower border of the
      // envelope.

      maxtop = GetWaveYPos(env[xx], zoomMin, zoomMax,
                               h, dB, true, dBRange, true);
      maxbot = GetWaveYPos(env[xx], zoomMin, zoomMax,
                               h, dB, false, dBRange, true);

      mintop = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                               h, dB, false, dBRange, true);
      minbot = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                               h, dB, true, dBRange, true);

      // Make sure it's odd so that a that max and min mirror each other
      mintop +=1;
      minbot +=1;

      const auto drawEnvelope = artist->drawEnvelope;
      if (!drawEnvelope || maxbot > mintop) {
         maxbot = halfHeight;
         mintop = halfHeight;
      }

      // We don't draw selection color for sync-lock selected tracks.
      sel = (t0 <= time && nextTime < t1) && !bIsSyncLockSelected;

      if (lmaxtop == maxtop &&
          lmintop == mintop &&
          lmaxbot == maxbot &&
          lminbot == minbot &&
          lsel == sel) {
         continue;
      }

      dc.SetBrush(lsel ? selectedBrush : unselectedBrush);

      l = rect.x + lx;
      w = xx - lx;
      if (lmaxbot < lmintop - 1) {
         dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
         dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
      }
      else {
         dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
      }

      if (highlightEnvelope && lmaxbot < lmintop - 1) {
         dc.SetBrush( AColor::uglyBrush );
         dc.DrawRectangle(l, rect.y + lmaxbot, w, lmintop - lmaxbot);
      }

      lmaxtop = maxtop;
      lmintop = mintop;
      lmaxbot = maxbot;
      lminbot = minbot;
      lsel = sel;
      lx = xx;
   }

   dc.SetBrush(lsel ? selectedBrush : unselectedBrush);
   l = rect.x + lx;
   w = xx - lx;
   if (lmaxbot < lmintop - 1) {
      dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
      dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
   }
   else {
      dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
   }
   if (highlightEnvelope && lmaxbot < lmintop - 1) {
      dc.SetBrush( AColor::uglyBrush );
      dc.DrawRectangle(l, rect.y + lmaxbot, w, lmintop - lmaxbot);
   }

   // If sync-lock selected, draw in linked graphics.
   if (bIsSyncLockSelected && t0 < t1) {
      const int begin = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t0, -leftOffset))));
      const int end = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t1, -leftOffset))));
      TrackArt::DrawSyncLockTiles( context,
         { rect.x + begin, rect.y, end - 1 - begin, rect.height } );
   }

   //OK, the display bounds are between min and max, which
   //is spread across rect.height.  Draw the line at the proper place.
   if (zeroLevelYCoordinate >= rect.GetTop() &&
       zeroLevelYCoordinate <= rect.GetBottom()) {
      dc.SetPen(*wxBLACK_PEN);
      AColor::Line(dc, rect.x, zeroLevelYCoordinate,
                   rect.x + rect.width, zeroLevelYCoordinate);
   }
}

void DrawMinMaxRMS(
   TrackPanelDrawingContext &context, const wxRect & rect, const double env[],
   float zoomMin, float zoomMax,
   bool dB, float dBRange,
   const float *min, const float *max, const float *rms, const int *bl,
   bool /* showProgress */, bool muted)
{
   auto &dc = context.dc;

   // Display a line representing the
   // min and max of the samples in this region
   int lasth1 = std::numeric_limits<int>::max();
   int lasth2 = std::numeric_limits<int>::min();
   int h1;
   int h2;
   ArrayOf<int> r1{ size_t(rect.width) };
   ArrayOf<int> r2{ size_t(rect.width) };
   ArrayOf<int> clipped;
   int clipcnt = 0;

   const auto artist = TrackArtist::Get( context );
   const auto bShowClipping = artist->mShowClipping;
   if (bShowClipping) {
      clipped.reinit( size_t(rect.width) );
   }

   long pixAnimOffset = (long)fabs((double)(wxDateTime::Now().GetTicks() * -10)) +
      wxDateTime::Now().GetMillisecond() / 100; //10 pixels a second

   bool drawStripes = true;
   bool drawWaveform = true;

   const auto &muteSamplePen = artist->muteSamplePen;
   const auto &samplePen = artist->samplePen;

   dc.SetPen(muted ? muteSamplePen : samplePen);
   for (int x0 = 0; x0 < rect.width; ++x0) {
      int xx = rect.x + x0;
      double v;
      v = min[x0] * env[x0];
      if (clipped && bShowClipping && (v <= -MAX_AUDIO))
      {
         if (clipcnt == 0 || clipped[clipcnt - 1] != xx) {
            clipped[clipcnt++] = xx;
         }
      }
      h1 = GetWaveYPos(v, zoomMin, zoomMax,
                       rect.height, dB, true, dBRange, true);

      v = max[x0] * env[x0];
      if (clipped && bShowClipping && (v >= MAX_AUDIO))
      {
         if (clipcnt == 0 || clipped[clipcnt - 1] != xx) {
            clipped[clipcnt++] = xx;
         }
      }
      h2 = GetWaveYPos(v, zoomMin, zoomMax,
                       rect.height, dB, true, dBRange, true);

      // JKC: This adjustment to h1 and h2 ensures that the drawn
      // waveform is continuous.
      if (x0 > 0) {
         if (h1 < lasth2) {
            h1 = lasth2 - 1;
         }
         if (h2 > lasth1) {
            h2 = lasth1 + 1;
         }
      }
      lasth1 = h1;
      lasth2 = h2;

      r1[x0] = GetWaveYPos(-rms[x0] * env[x0], zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
      r2[x0] = GetWaveYPos(rms[x0] * env[x0], zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
      // Make sure the rms isn't larger than the waveform min/max
      if (r1[x0] > h1 - 1) {
         r1[x0] = h1 - 1;
      }
      if (r2[x0] < h2 + 1) {
         r2[x0] = h2 + 1;
      }
      if (r2[x0] > r1[x0]) {
         r2[x0] = r1[x0];
      }

      if (bl[x0] <= -1) {
         if (drawStripes) {
            // TODO:unify with buffer drawing.
            dc.SetPen((bl[x0] % 2) ? muteSamplePen : samplePen);
            for (int yy = 0; yy < rect.height / 25 + 1; ++yy) {
               // we are drawing over the buffer, but I think DrawLine takes care of this.
               AColor::Line(dc,
                            xx,
                            rect.y + 25 * yy + (x0 /*+pixAnimOffset*/) % 25,
                            xx,
                            rect.y + 25 * yy + (x0 /*+pixAnimOffset*/) % 25 + 6); //take the min so we don't draw past the edge
            }
         }

         // draw a dummy waveform - some kind of sinusoid.  We want to animate it so the user knows it's a dummy.  Use the second's unit of a get time function.
         // Lets use a triangle wave for now since it's easier - I don't want to use sin() or make a wavetable just for this.
         if (drawWaveform) {
            int triX;
            dc.SetPen(samplePen);
            triX = fabs((double)((x0 + pixAnimOffset) % (2 * rect.height)) - rect.height) + rect.height;
            for (int yy = 0; yy < rect.height; ++yy) {
               if ((yy + triX) % rect.height == 0) {
                  dc.DrawPoint(xx, rect.y + yy);
               }
            }
         }

         // Restore the pen for remaining pixel columns!
         dc.SetPen(muted ? muteSamplePen : samplePen);
      }
      else {
         AColor::Line(dc, xx, rect.y + h2, xx, rect.y + h1);
      }
   }

   // Stroke rms over the min-max
   const auto &muteRmsPen = artist->muteRmsPen;
   const auto &rmsPen = artist->rmsPen;

   dc.SetPen(muted ? muteRmsPen : rmsPen);
   for (int x0 = 0; x0 < rect.width; ++x0) {
      int xx = rect.x + x0;
      if (bl[x0] <= -1) {
      }
      else if (r1[x0] != r2[x0]) {
         AColor::Line(dc, xx, rect.y + r2[x0], xx, rect.y + r1[x0]);
      }
   }

   // Draw the clipping lines
   if (clipcnt) {
      const auto &muteClippedPen = artist->muteClippedPen;
      const auto &clippedPen = artist->clippedPen;

      dc.SetPen(muted ? muteClippedPen : clippedPen);
      while (--clipcnt >= 0) {
         int xx = clipped[clipcnt];
         AColor::Line(dc, xx, rect.y, xx, rect.y + rect.height);
      }
   }
}

void DrawIndividualSamples(TrackPanelDrawingContext &context,
                                        int leftOffset, const wxRect &rect,
                                        float zoomMin, float zoomMax,
                                        bool dB, float dBRange,
                                        const WaveClip *clip,
                                        bool showPoints, bool muted,
                                        bool highlight)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &zoomInfo = *artist->pZoomInfo;

   const double toffset = clip->GetOffset();
   double rate = clip->GetRate();
   const double t0 = std::max(0.0, zoomInfo.PositionToTime(0, -leftOffset) - toffset);
   const auto s0 = sampleCount(floor(t0 * rate));
   const auto snSamples = clip->GetNumSamples();
   if (s0 > snSamples)
      return;

   const double t1 = zoomInfo.PositionToTime(rect.width - 1, -leftOffset) - toffset;
   const auto s1 = sampleCount(ceil(t1 * rate));

   // Assume size_t will not overflow, else we wouldn't be here drawing the
   // few individual samples
   auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

   if (slen <= 0)
      return;

   Floats buffer{ size_t(slen) };
   clip->GetSamples((samplePtr)buffer.get(), floatSample, s0, slen,
                    // Suppress exceptions in this drawing operation:
                    false);

   ArrayOf<int> xpos{ size_t(slen) };
   ArrayOf<int> ypos{ size_t(slen) };
   ArrayOf<int> clipped;
   int clipcnt = 0;

   const auto bShowClipping = artist->mShowClipping;
   if (bShowClipping)
      clipped.reinit( size_t(slen) );

   const auto &muteSamplePen = artist->muteSamplePen;
   const auto &samplePen = artist->samplePen;
   auto &pen = highlight ? AColor::uglyPen : muted ? muteSamplePen : samplePen;
   dc.SetPen( pen );

   for (decltype(slen) s = 0; s < slen; s++) {
      const double time = toffset + (s + s0).as_double() / rate;
      const int xx = // An offset into the rectangle rect
         std::max(-10000, std::min(10000,
            (int)(zoomInfo.TimeToPosition(time, -leftOffset))));
      xpos[s] = xx;

      // Calculate sample as it would be rendered, so quantize time
      double value =
         clip->GetEnvelope()->GetValue( time, 1.0 / clip->GetRate() );
      const double tt = buffer[s] * value;

      if (clipped && bShowClipping && ((tt <= -MAX_AUDIO) || (tt >= MAX_AUDIO)))
         clipped[clipcnt++] = xx;
      ypos[s] =
         std::max(-1,
            std::min(rect.height,
               GetWaveYPos(tt, zoomMin, zoomMax,
                                  rect.height, dB, true, dBRange, false)));
   }


   if (showPoints) {
      // Draw points where spacing is enough
      const auto bigPoints = artist->bigPoints;
      const int tickSize = bigPoints ? 4 : 3;// Bigger ellipses when draggable.
      wxRect pr;
      pr.width = tickSize;
      pr.height = tickSize;
      //different colour when draggable.
      const auto &dragsampleBrush = artist->dragsampleBrush;
      const auto &sampleBrush = artist->sampleBrush;
      auto &brush = highlight
         ? AColor::uglyBrush
         : bigPoints ? dragsampleBrush : sampleBrush;
      dc.SetBrush( brush );
      for (decltype(slen) s = 0; s < slen; s++) {
         if (ypos[s] >= 0 && ypos[s] < rect.height) {
            pr.x = rect.x + xpos[s] - tickSize/2;
            pr.y = rect.y + ypos[s] - tickSize/2;
            dc.DrawEllipse(pr);
         }
      }
   }

   const auto sampleDisplay = artist->mSampleDisplay;
   if (showPoints && (sampleDisplay == (int) WaveTrackViewConstants::StemPlot)) {
      // Draw vertical lines
      int yZero = GetWaveYPos(0.0, zoomMin, zoomMax, rect.height, dB, true, dBRange, false);
      yZero = rect.y + std::max(-1, std::min(rect.height, yZero));
      for (decltype(slen) s = 0; s < slen; s++) {
         AColor::Line(dc,
                     rect.x + xpos[s], rect.y + ypos[s],
                     rect.x + xpos[s], yZero);
      }
   }
   else {
      // Connect samples with straight lines
      for (decltype(slen) s = 0; s < slen - 1; s++) {
         AColor::Line(dc,
                     rect.x + xpos[s], rect.y + ypos[s],
                     rect.x + xpos[s + 1], rect.y + ypos[s + 1]);
      }
   }

   // Draw clipping
   if (clipcnt) {
      const auto &muteClippedPen = artist->muteClippedPen;
      const auto &clippedPen = artist->clippedPen;
      dc.SetPen(muted ? muteClippedPen : clippedPen);
      while (--clipcnt >= 0) {
         auto s = clipped[clipcnt];
         AColor::Line(dc, rect.x + s, rect.y, rect.x + s, rect.y + rect.height);
      }
   }
}

void DrawEnvLine(
   TrackPanelDrawingContext &context,
   const wxRect &rect, int x0, int y0, int cy, bool top )
{
   auto &dc = context.dc;

   int xx = rect.x + x0;
   int yy = rect.y + cy;

   if (y0 < 0) {
      if (x0 % 4 != 3) {
         AColor::Line(dc, xx, yy, xx, yy + 3);
      }
   }
   else if (y0 > rect.height) {
      if (x0 % 4 != 3) {
         AColor::Line(dc, xx, yy - 3, xx, yy);
      }
   }
   else {
      if (top) {
         AColor::Line(dc, xx, yy, xx, yy + 3);
      }
      else {
         AColor::Line(dc, xx, yy - 3, xx, yy);
      }
   }
}

void DrawEnvelope(TrackPanelDrawingContext &context,
                               const wxRect &rect, const double env[],
                               float zoomMin, float zoomMax,
                               bool dB, float dBRange, bool highlight)
{
   auto &dc = context.dc;

   int h = rect.height;

   auto &pen = highlight ? AColor::uglyPen : AColor::envelopePen;
   dc.SetPen( pen );

   for (int x0 = 0; x0 < rect.width; ++x0) {
      int cenvTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                                h, dB, true, dBRange, true);

      int cenvBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                                h, dB, true, dBRange, true);

      int envTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                               h, dB, true, dBRange, false);

      int envBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                               h, dB, true, dBRange, false);

      // Make the collision at zero actually look solid
      if (cenvBot - cenvTop < 9) {
         int value = (int)((zoomMax / (zoomMax - zoomMin)) * h);
         cenvTop = value - 4;
         cenvBot = value + 4;
      }

      DrawEnvLine( context, rect, x0, envTop, cenvTop, true );
      DrawEnvLine( context, rect, x0, envBot, cenvBot, false );
   }
}

// Headers needed only for experimental drawing below
//#include "tracks/playabletrack/wavetrack/ui/SampleHandle.h"
//#include "tracks/ui/EnvelopeHandle.h"
void DrawClipWaveform(TrackPanelDrawingContext &context,
                                   const WaveTrack *track,
                                   const WaveClip *clip,
                                   const wxRect & rect,
                                   bool dB,
                                   bool muted)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &selectedRegion = *artist->pSelectedRegion;
   const auto &zoomInfo = *artist->pZoomInfo;

#ifdef PROFILE_WAVEFORM
   Profiler profiler;
#endif

   bool highlightEnvelope = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   auto target = dynamic_cast<EnvelopeHandle*>(context.target.get());
   highlightEnvelope = target && target->GetEnvelope() == clip->GetEnvelope();
#endif

   const ClipParameters params{
      false, track, clip, rect, selectedRegion, zoomInfo };
   const wxRect &hiddenMid = params.hiddenMid;
   // The "hiddenMid" rect contains the part of the display actually
   // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
   if (hiddenMid.width <= 0) {
      return;
   }

   const double &t0 = params.t0;
   const double &tOffset = params.tOffset;
   const double &h = params.h;
   const double &tpre = params.tpre;
   const double &tpost = params.tpost;
   const double &t1 = params.t1;
   const double &averagePixelsPerSample = params.averagePixelsPerSample;
   const double &rate = params.rate;
   double leftOffset = params.leftOffset;
   const wxRect &mid = params.mid;

   const float dBRange = track->GetWaveformSettings().dBRange;

   dc.SetPen(*wxTRANSPARENT_PEN);
   int iColorIndex = clip->GetColourIndex();
   artist->SetColours( iColorIndex );

   // If we get to this point, the clip is actually visible on the
   // screen, so remember the display rectangle.
   clip->SetDisplayRect(hiddenMid);

   // The bounds (controlled by vertical zooming; -1.0...1.0
   // by default)
   float zoomMin, zoomMax;
   track->GetDisplayBounds(&zoomMin, &zoomMax);

   std::vector<double> vEnv(mid.width);
   double *const env = &vEnv[0];
   Envelope::GetValues( *clip->GetEnvelope(),
       tOffset,

        // PRL: change back to make envelope evaluate only at sample times
        // and then interpolate the display
        0, // 1.0 / rate,

        env, mid.width, leftOffset, zoomInfo );

   // Draw the background of the track, outlining the shape of
   // the envelope and using a colored pen for the selected
   // part of the waveform
   {
      double tt0, tt1;
      if (track->GetSelected() || track->IsSyncLockSelected()) {
         tt0 = track->LongSamplesToTime(track->TimeToLongSamples(selectedRegion.t0())),
            tt1 = track->LongSamplesToTime(track->TimeToLongSamples(selectedRegion.t1()));
      }
      else
         tt0 = tt1 = 0.0;
      DrawWaveformBackground(context, leftOffset, mid,
         env,
         zoomMin, zoomMax,
         track->ZeroLevelYCoordinate(mid),
         dB, dBRange,
         tt0, tt1,
         !track->GetSelected(), highlightEnvelope);
   }

   WaveDisplay display(hiddenMid.width);
   bool isLoadingOD = false;//true if loading on demand block in sequence.

   const double pps =
      averagePixelsPerSample * rate;

   // For each portion separately, we will decide to draw
   // it as min/max/rms or as individual samples.
   std::vector<WavePortion> portions;
   FindWavePortions(portions, rect, zoomInfo, params);
   const unsigned nPortions = portions.size();

   // Require at least 1/2 pixel per sample for drawing individual samples.
   const double threshold1 = 0.5 * rate;
   // Require at least 3 pixels per sample for drawing the draggable points.
   const double threshold2 = 3 * rate;

   {
      bool showIndividualSamples = false;
      for (unsigned ii = 0; !showIndividualSamples && ii < nPortions; ++ii) {
         const WavePortion &portion = portions[ii];
         showIndividualSamples =
            !portion.inFisheye && portion.averageZoom > threshold1;
      }

      if (!showIndividualSamples) {
         // The WaveClip class handles the details of computing the shape
         // of the waveform.  The only way GetWaveDisplay will fail is if
         // there's a serious error, like some of the waveform data can't
         // be loaded.  So if the function returns false, we can just exit.

         // Note that we compute the full width display even if there is a
         // fisheye hiding part of it, because of the caching.  If the
         // fisheye moves over the background, there is then less to do when
         // redrawing.

         if (!clip->GetWaveDisplay(display,
            t0, pps, isLoadingOD))
            return;
      }
   }

   // TODO Add a comment to say what this loop does.
   // Possily make it into a subroutine.
   for (unsigned ii = 0; ii < nPortions; ++ii) {
      WavePortion &portion = portions[ii];
      const bool showIndividualSamples = portion.averageZoom > threshold1;
      const bool showPoints = portion.averageZoom > threshold2;
      wxRect& rectPortion = portion.rect;
      rectPortion.Intersect(mid);
      wxASSERT(rectPortion.width >= 0);

      float *useMin = 0, *useMax = 0, *useRms = 0;
      int *useBl = 0;
      WaveDisplay fisheyeDisplay(rectPortion.width);
      int skipped = 0, skippedLeft = 0, skippedRight = 0;
      if (portion.inFisheye) {
         if (!showIndividualSamples) {
            fisheyeDisplay.Allocate();
            const auto numSamples = clip->GetNumSamples();
            // Get wave display data for different magnification
            int jj = 0;
            for (; jj < rectPortion.width; ++jj) {
               const double time =
                  zoomInfo.PositionToTime(jj, -leftOffset) - tOffset;
               const auto sample = (sampleCount)floor(time * rate + 0.5);
               if (sample < 0) {
                  ++rectPortion.x;
                  ++skippedLeft;
                  continue;
               }
               if (sample >= numSamples)
                  break;
               fisheyeDisplay.where[jj - skippedLeft] = sample;
            }

            skippedRight = rectPortion.width - jj;
            skipped = skippedRight + skippedLeft;
            rectPortion.width -= skipped;

            // where needs a sentinel
            if (jj > 0)
               fisheyeDisplay.where[jj - skippedLeft] =
               1 + fisheyeDisplay.where[jj - skippedLeft - 1];
            fisheyeDisplay.width -= skipped;
            // Get a wave display for the fisheye, uncached.
            if (rectPortion.width > 0)
               if (!clip->GetWaveDisplay(
                     fisheyeDisplay, t0, -1.0, // ignored
                     isLoadingOD))
                  continue; // serious error.  just don't draw??
            useMin = fisheyeDisplay.min;
            useMax = fisheyeDisplay.max;
            useRms = fisheyeDisplay.rms;
            useBl = fisheyeDisplay.bl;
         }
      }
      else {
         const int pos = leftOffset - params.hiddenLeftOffset;
         useMin = display.min + pos;
         useMax = display.max + pos;
         useRms = display.rms + pos;
         useBl = display.bl + pos;
      }

      leftOffset += skippedLeft;

      if (rectPortion.width > 0) {
         if (!showIndividualSamples) {
            std::vector<double> vEnv2(rectPortion.width);
            double *const env2 = &vEnv2[0];
            Envelope::GetValues( *clip->GetEnvelope(),
                tOffset,

                 // PRL: change back to make envelope evaluate only at sample times
                 // and then interpolate the display
                 0, // 1.0 / rate,

                 env2, rectPortion.width, leftOffset, zoomInfo );
            DrawMinMaxRMS( context, rectPortion, env2,
               zoomMin, zoomMax,
               dB, dBRange,
               useMin, useMax, useRms, useBl,
               isLoadingOD, muted );
         }
         else {
            bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
            auto target = dynamic_cast<SampleHandle*>(context.target.get());
            highlight = target && target->GetTrack().get() == track;
#endif
            DrawIndividualSamples(
               context, leftOffset, rectPortion, zoomMin, zoomMax,
               dB, dBRange,
               clip,
               showPoints, muted, highlight );
         }
      }

      leftOffset += rectPortion.width + skippedRight;
   }

   const auto drawEnvelope = artist->drawEnvelope;
   if (drawEnvelope) {
      DrawEnvelope(
         context, mid, env, zoomMin, zoomMax, dB, dBRange, highlightEnvelope );
      EnvelopeEditor::DrawPoints( *clip->GetEnvelope(),
          context, rect, dB, dBRange, zoomMin, zoomMax, true );
   }

   // Draw arrows on the left side if the track extends to the left of the
   // beginning of time.  :)
   if (h == 0.0 && tOffset < 0.0) {
      TrackArt::DrawNegativeOffsetTrackArrows( context, rect );
   }

   // Draw clip edges
   dc.SetPen(*wxGREY_PEN);
   if (tpre < 0) {
      AColor::Line(dc,
                   mid.x - 1, mid.y,
                   mid.x - 1, mid.y + rect.height);
   }
   if (tpost > t1) {
      AColor::Line(dc,
                   mid.x + mid.width, mid.y,
                   mid.x + mid.width, mid.y + rect.height);
   }
}

void DrawTimeSlider( TrackPanelDrawingContext &context,
                                  const wxRect & rect,
                                  bool rightwards, bool highlight )
{
   auto &dc = context.dc;

   const int border = 3; // 3 pixels all round.
   const int width = 6; // width of the drag box.
   const int taper = 6; // how much the box tapers by.
   const int barSpacing = 4; // how far apart the bars are.
   const int barWidth = 3;
   const int xFlat = 3;

   //Enough space to draw in?
   if (rect.height <= ((taper+border + barSpacing) * 2)) {
      return;
   }
   if (rect.width <= (width * 2 + border * 3)) {
      return;
   }

   // The draggable box is tapered towards the direction you drag it.
   int leftTaper  = rightwards ? 0 : 6;
   int rightTaper = rightwards ? 6 : 0;

   int xLeft = rightwards ? (rect.x + border - 2)
                          : (rect.x + rect.width + 1 - (border + width));
   int yTop  = rect.y + border;
   int yBot  = rect.y + rect.height - border - 1;

   AColor::Light(&dc, false, highlight);
   AColor::Line(dc, xLeft,         yBot - leftTaper, xLeft,         yTop + leftTaper);
   AColor::Line(dc, xLeft,         yTop + leftTaper, xLeft + xFlat, yTop);
   AColor::Line(dc, xLeft + xFlat, yTop,             xLeft + width, yTop + rightTaper);

   AColor::Dark(&dc, false, highlight);
   AColor::Line(dc, xLeft + width,         yTop + rightTaper, xLeft + width,       yBot - rightTaper);
   AColor::Line(dc, xLeft + width,         yBot - rightTaper, xLeft + width-xFlat, yBot);
   AColor::Line(dc, xLeft + width - xFlat, yBot,              xLeft,               yBot - leftTaper);

   int firstBar = yTop + taper + taper / 2;
   int nBars    = (yBot - yTop - taper * 3) / barSpacing + 1;
   xLeft += (width - barWidth + 1) / 2;
   int yy;
   int i;

   AColor::Light(&dc, false, highlight);
   for (i = 0;i < nBars; i++) {
      yy = firstBar + barSpacing * i;
      AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
   }
   AColor::Dark(&dc, false, highlight);
   for(i = 0;i < nBars; i++){
      yy = firstBar + barSpacing * i + 1;
      AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
   }
}

// Headers needed only for experimental drawing below
//#include "tracks/ui/TimeShiftHandle.h"
//#include "tracks/playabletrack/wavetrack/ui/CutlineHandle.h"
void DrawWaveform(TrackPanelDrawingContext &context,
                               const WaveTrack *track,
                               const wxRect & rect,
                               bool muted)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );

   bool highlight = false;
   bool gripHit = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   auto target = dynamic_cast<TimeShiftHandle*>(context.target.get());
   gripHit = target && target->IsGripHit();
   highlight = target && target->GetTrack().get() == track;
#endif

   const bool dB = !track->GetWaveformSettings().isLinear();

   const auto &blankSelectedBrush = artist->blankSelectedBrush;
   const auto &blankBrush = artist->blankBrush;
   TrackArt::DrawBackgroundWithSelection(
      context, rect, track, blankSelectedBrush, blankBrush );

   for (const auto &clip: track->GetClips())
      DrawClipWaveform(context, track, clip.get(), rect,
                       dB, muted);

   // Update cache for locations, e.g. cutlines and merge points
   track->UpdateLocationsCache();

   const auto &zoomInfo = *artist->pZoomInfo;

#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   auto target2 = dynamic_cast<CutlineHandle*>(context.target.get());
#endif
   for (const auto loc : track->GetCachedLocations()) {
      bool highlightLoc = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
      highlightLoc =
         target2 && target2->GetTrack().get() == track &&
         target2->GetLocation() == loc;
#endif
      const int xx = zoomInfo.TimeToPosition(loc.pos);
      if (xx >= 0 && xx < rect.width) {
         dc.SetPen( highlightLoc ? AColor::uglyPen : *wxGREY_PEN );
         AColor::Line(dc, (int) (rect.x + xx - 1), rect.y, (int) (rect.x + xx - 1), rect.y + rect.height);
         if (loc.typ == WaveTrackLocation::locationCutLine) {
            dc.SetPen( highlightLoc ? AColor::uglyPen : *wxRED_PEN );
         }
         else {
#ifdef EXPERIMENTAL_DA
            // JKC Black does not show up enough.
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxWHITE_PEN);
#else
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxBLACK_PEN);
#endif
         }
         AColor::Line(dc, (int) (rect.x + xx), rect.y, (int) (rect.x + xx), rect.y + rect.height);
         dc.SetPen( highlightLoc ? AColor::uglyPen : *wxGREY_PEN );
         AColor::Line(dc, (int) (rect.x + xx + 1), rect.y, (int) (rect.x + xx + 1), rect.y + rect.height);
      }
   }

   const auto drawSliders = artist->drawSliders;
   if (drawSliders) {
      DrawTimeSlider( context, rect, true, highlight && gripHit );  // directed right
      DrawTimeSlider( context, rect, false, highlight && gripHit ); // directed left
   }
}

static inline float findValue
(const float *spectrum, float bin0, float bin1, unsigned nBins,
 bool autocorrelation, int gain, int range)
{
   float value;


#if 0
   // Averaging method
   if ((int)(bin1) == (int)(bin0)) {
      value = spectrum[(int)(bin0)];
   } else {
      float binwidth= bin1 - bin0;
      value = spectrum[(int)(bin0)] * (1.f - bin0 + (int)bin0);

      bin0 = 1 + (int)(bin0);
      while (bin0 < (int)(bin1)) {
         value += spectrum[(int)(bin0)];
         bin0 += 1.0;
      }
      // Do not reference past end of freq array.
      if ((int)(bin1) >= (int)nBins) {
         bin1 -= 1.0;
      }

      value += spectrum[(int)(bin1)] * (bin1 - (int)(bin1));
      value /= binwidth;
   }
#else
   // Maximum method, and no apportionment of any single bins over multiple pixel rows
   // See Bug971
   int index, limitIndex;
   if (autocorrelation) {
      // bin = 2 * nBins / (nBins - 1 - array_index);
      // Solve for index
      index = std::max(0.0f, std::min(float(nBins - 1),
         (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin0))
      ));
      limitIndex = std::max(0.0f, std::min(float(nBins - 1),
         (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin1))
      ));
   }
   else {
      index = std::min<int>(nBins - 1, (int)(floor(0.5 + bin0)));
      limitIndex = std::min<int>(nBins, (int)(floor(0.5 + bin1)));
   }
   value = spectrum[index];
   while (++index < limitIndex)
      value = std::max(value, spectrum[index]);
#endif
   if (!autocorrelation) {
      // Last step converts dB to a 0.0-1.0 range
      value = (value + range + gain) / (double)range;
   }
   value = std::min(1.0f, std::max(0.0f, value));
   return value;
}

// dashCount counts both dashes and the spaces between them.
inline AColor::ColorGradientChoice
ChooseColorSet( float bin0, float bin1, float selBinLo,
   float selBinCenter, float selBinHi, int dashCount, bool isSpectral )
{
   if (!isSpectral)
      return  AColor::ColorGradientTimeSelected;
   if ((selBinCenter >= 0) && (bin0 <= selBinCenter) &&
       (selBinCenter < bin1))
      return AColor::ColorGradientEdge;
   if ((0 == dashCount % 2) &&
       (((selBinLo >= 0) && (bin0 <= selBinLo) && ( selBinLo < bin1))  ||
        ((selBinHi >= 0) && (bin0 <= selBinHi) && ( selBinHi < bin1))))
      return AColor::ColorGradientEdge;
   if ((selBinLo < 0 || selBinLo < bin1) && (selBinHi < 0 || selBinHi > bin0))
      return  AColor::ColorGradientTimeAndFrequencySelected;
   
   return  AColor::ColorGradientTimeSelected;
}

void DrawClipSpectrum(TrackPanelDrawingContext &context,
                                   WaveTrackCache &waveTrackCache,
                                   const WaveClip *clip,
                                   const wxRect & rect)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &selectedRegion = *artist->pSelectedRegion;
   const auto &zoomInfo = *artist->pZoomInfo;

#ifdef PROFILE_WAVEFORM
   Profiler profiler;
#endif

   const WaveTrack *const track = waveTrackCache.GetTrack().get();
   const SpectrogramSettings &settings = track->GetSpectrogramSettings();
   const bool autocorrelation = (settings.algorithm == SpectrogramSettings::algPitchEAC);

   enum { DASH_LENGTH = 10 /* pixels */ };

   const ClipParameters params{
      true, track, clip, rect, selectedRegion, zoomInfo };
   const wxRect &hiddenMid = params.hiddenMid;
   // The "hiddenMid" rect contains the part of the display actually
   // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
   if (hiddenMid.width <= 0) {
      return;
   }

   const double &t0 = params.t0;
   const double &tOffset = params.tOffset;
   const auto &ssel0 = params.ssel0;
   const auto &ssel1 = params.ssel1;
   const double &averagePixelsPerSample = params.averagePixelsPerSample;
   const double &rate = params.rate;
   const double &hiddenLeftOffset = params.hiddenLeftOffset;
   const double &leftOffset = params.leftOffset;
   const wxRect &mid = params.mid;

   // If we get to this point, the clip is actually visible on the
   // screen, so remember the display rectangle.
   clip->SetDisplayRect(hiddenMid);

   double freqLo = SelectedRegion::UndefinedFrequency;
   double freqHi = SelectedRegion::UndefinedFrequency;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   freqLo = selectedRegion.f0();
   freqHi = selectedRegion.f1();
#endif

   const bool &isGrayscale = settings.isGrayscale;
   const int &range = settings.range;
   const int &gain = settings.gain;

#ifdef EXPERIMENTAL_FIND_NOTES
   const bool &fftFindNotes = settings.fftFindNotes;
   const double &findNotesMinA = settings.findNotesMinA;
   const int &numberOfMaxima = settings.numberOfMaxima;
   const bool &findNotesQuantize = settings.findNotesQuantize;
#endif
#ifdef EXPERIMENTAL_FFT_Y_GRID
   const bool &fftYGrid = settings.fftYGrid;
#endif

   dc.SetPen(*wxTRANSPARENT_PEN);

   // We draw directly to a bit image in memory,
   // and then paint this directly to our offscreen
   // bitmap.  Note that this could be optimized even
   // more, but for now this is not bad.  -dmazzoni
   wxImage image((int)mid.width, (int)mid.height);
   if (!image.IsOk())
      return;
#ifdef EXPERIMENTAL_SPECTROGRAM_OVERLAY
   image.SetAlpha();
   unsigned char *alpha = image.GetAlpha();
#endif
   unsigned char *data = image.GetData();

   const auto half = settings.GetFFTLength() / 2;
   const double binUnit = rate / (2 * half);
   const float *freq = 0;
   const sampleCount *where = 0;
   bool updated;
   {
      const double pps = averagePixelsPerSample * rate;
      updated = clip->GetSpectrogram(waveTrackCache, freq, where,
                                     (size_t)hiddenMid.width,
         t0, pps);
   }
   auto nBins = settings.NBins();

   float minFreq, maxFreq;
   track->GetSpectrumBounds(&minFreq, &maxFreq);

   const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

   // nearest frequency to each pixel row from number scale, for selecting
   // the desired fft bin(s) for display on that row
   float *bins = (float*)alloca(sizeof(*bins)*(hiddenMid.height + 1));
   {
      const NumberScale numberScale( settings.GetScale( minFreq, maxFreq ) );

      NumberScale::Iterator it = numberScale.begin(mid.height);
      float nextBin = std::max( 0.0f, std::min( float(nBins - 1),
         settings.findBin( *it, binUnit ) ) );

      int yy;
      for (yy = 0; yy < hiddenMid.height; ++yy) {
         bins[yy] = nextBin;
         nextBin = std::max( 0.0f, std::min( float(nBins - 1),
            settings.findBin( *++it, binUnit ) ) );
      }
      bins[yy] = nextBin;
   }

#ifdef EXPERIMENTAL_FFT_Y_GRID
   const float
      log2 = logf(2.0f),
      scale2 = (lmax - lmin) / log2,
      lmin2 = lmin / log2;

   ArrayOf<bool> yGrid{size_t(mid.height)};
   for (int yy = 0; yy < mid.height; ++yy) {
      float n = (float(yy) / mid.height*scale2 - lmin2) * 12;
      float n2 = (float(yy + 1) / mid.height*scale2 - lmin2) * 12;
      float f = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n / 12.0f + lmin2);
      float f2 = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n2 / 12.0f + lmin2);
      n = logf(f / 440) / log2 * 12;
      n2 = logf(f2 / 440) / log2 * 12;
      if (floor(n) < floor(n2))
         yGrid[yy] = true;
      else
         yGrid[yy] = false;
   }
#endif //EXPERIMENTAL_FFT_Y_GRID

   if (!updated && clip->mSpecPxCache->valid &&
      ((int)clip->mSpecPxCache->len == hiddenMid.height * hiddenMid.width)
      && scaleType == clip->mSpecPxCache->scaleType
      && gain == clip->mSpecPxCache->gain
      && range == clip->mSpecPxCache->range
      && minFreq == clip->mSpecPxCache->minFreq
      && maxFreq == clip->mSpecPxCache->maxFreq
#ifdef EXPERIMENTAL_FFT_Y_GRID
   && fftYGrid==fftYGridOld
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
   && fftFindNotes==fftFindNotesOld
   && findNotesMinA==findNotesMinAOld
   && numberOfMaxima==findNotesNOld
   && findNotesQuantize==findNotesQuantizeOld
#endif
   ) {
      // Wave clip's spectrum cache is up to date,
      // and so is the spectrum pixel cache
   }
   else {
      // Update the spectrum pixel cache
      clip->mSpecPxCache = std::make_unique<SpecPxCache>(hiddenMid.width * hiddenMid.height);
      clip->mSpecPxCache->valid = true;
      clip->mSpecPxCache->scaleType = scaleType;
      clip->mSpecPxCache->gain = gain;
      clip->mSpecPxCache->range = range;
      clip->mSpecPxCache->minFreq = minFreq;
      clip->mSpecPxCache->maxFreq = maxFreq;
#ifdef EXPERIMENTAL_FIND_NOTES
      fftFindNotesOld = fftFindNotes;
      findNotesMinAOld = findNotesMinA;
      findNotesNOld = numberOfMaxima;
      findNotesQuantizeOld = findNotesQuantize;
#endif

#ifdef EXPERIMENTAL_FIND_NOTES
      float log2 = logf( 2.0f ),
         lmin = logf( minFreq ), lmax = logf( maxFreq ), scale = lmax - lmin,
         lmins = lmin,
         lmaxs = lmax
         ;
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef EXPERIMENTAL_FIND_NOTES
      int maxima[128];
      float maxima0[128], maxima1[128];
      const float
         f2bin = half / (rate / 2.0f),
         bin2f = 1.0f / f2bin,
         minDistance = powf(2.0f, 2.0f / 12.0f),
         i0 = expf(lmin) / binUnit,
         i1 = expf(scale + lmin) / binUnit,
         minColor = 0.0f;
      const size_t maxTableSize = 1024;
      ArrayOf<int> indexes{ maxTableSize };
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef _OPENMP
#pragma omp parallel for
#endif
      for (int xx = 0; xx < hiddenMid.width; ++xx) {
#ifdef EXPERIMENTAL_FIND_NOTES
         int maximas = 0;
         const int x0 = nBins * xx;
         if (fftFindNotes) {
            for (int i = maxTableSize - 1; i >= 0; i--)
               indexes[i] = -1;

            // Build a table of (most) values, put the index in it.
            for (int i = (int)(i0); i < (int)(i1); i++) {
               float freqi = freq[x0 + (int)(i)];
               int value = (int)((freqi + gain + range) / range*(maxTableSize - 1));
               if (value < 0)
                  value = 0;
               if (value >= maxTableSize)
                  value = maxTableSize - 1;
               indexes[value] = i;
            }
            // Build from the indices an array of maxima.
            for (int i = maxTableSize - 1; i >= 0; i--) {
               int index = indexes[i];
               if (index >= 0) {
                  float freqi = freq[x0 + index];
                  if (freqi < findNotesMinA)
                     break;

                  bool ok = true;
                  for (int m = 0; m < maximas; m++) {
                     // Avoid to store very close maxima.
                     float maxm = maxima[m];
                     if (maxm / index < minDistance && index / maxm < minDistance) {
                        ok = false;
                        break;
                     }
                  }
                  if (ok) {
                     maxima[maximas++] = index;
                     if (maximas >= numberOfMaxima)
                        break;
                  }
               }
            }

// The f2pix helper macro converts a frequency into a pixel coordinate.
#define f2pix(f) (logf(f)-lmins)/(lmaxs-lmins)*hiddenMid.height

            // Possibly quantize the maxima frequencies and create the pixel block limits.
            for (int i = 0; i < maximas; i++) {
               int index = maxima[i];
               float f = float(index)*bin2f;
               if (findNotesQuantize)
               {
                  f = expf((int)(log(f / 440) / log2 * 12 - 0.5) / 12.0f*log2) * 440;
                  maxima[i] = f*f2bin;
               }
               float f0 = expf((log(f / 440) / log2 * 24 - 1) / 24.0f*log2) * 440;
               maxima0[i] = f2pix(f0);
               float f1 = expf((log(f / 440) / log2 * 24 + 1) / 24.0f*log2) * 440;
               maxima1[i] = f2pix(f1);
            }
         }

         int it = 0;
         bool inMaximum = false;
#endif //EXPERIMENTAL_FIND_NOTES

         for (int yy = 0; yy < hiddenMid.height; ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy+1];

            if (settings.scaleType != SpectrogramSettings::stLogarithmic) {
               const float value = findValue
                  (freq + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
               clip->mSpecPxCache->values[xx * hiddenMid.height + yy] = value;
            }
            else {
               float value;

#ifdef EXPERIMENTAL_FIND_NOTES
               if (fftFindNotes) {
                  if (it < maximas) {
                     float i0 = maxima0[it];
                     if (yy >= i0)
                        inMaximum = true;

                     if (inMaximum) {
                        float i1 = maxima1[it];
                        if (yy + 1 <= i1) {
                           value = findValue(freq + x0, bin, nextBin, nBins, autocorrelation, gain, range);
                           if (value < findNotesMinA)
                              value = minColor;
                        }
                        else {
                           it++;
                           inMaximum = false;
                           value = minColor;
                        }
                     }
                     else {
                        value = minColor;
                     }
                  }
                  else
                     value = minColor;
               }
               else
#endif //EXPERIMENTAL_FIND_NOTES
               {
                  value = findValue
                     (freq + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
               }
               clip->mSpecPxCache->values[xx * hiddenMid.height + yy] = value;
            } // logF
         } // each yy
      } // each xx
   } // updating cache

   float selBinLo = settings.findBin( freqLo, binUnit);
   float selBinHi = settings.findBin( freqHi, binUnit);
   float selBinCenter = (freqLo < 0 || freqHi < 0)
      ? -1
      : settings.findBin( sqrt(freqLo * freqHi), binUnit );

   const bool isSpectral = settings.SpectralSelectionEnabled();
   const bool hidden = (ZoomInfo::HIDDEN == zoomInfo.GetFisheyeState());
   const int begin = hidden
      ? 0
      : std::max(0, (int)(zoomInfo.GetFisheyeLeftBoundary(-leftOffset)));
   const int end = hidden
      ? 0
      : std::min(mid.width, (int)(zoomInfo.GetFisheyeRightBoundary(-leftOffset)));
   const size_t numPixels = std::max(0, end - begin);

   SpecCache specCache;

   // need explicit resize since specCache.where[] accessed before Populate()
   specCache.Grow(numPixels, settings, -1, t0);

   if (numPixels > 0) {
      for (int ii = begin; ii < end; ++ii) {
         const double time = zoomInfo.PositionToTime(ii, -leftOffset) - tOffset;
         specCache.where[ii - begin] = sampleCount(0.5 + rate * time);
      }
      specCache.Populate
         (settings, waveTrackCache,
          0, 0, numPixels,
          clip->GetNumSamples(),
          tOffset, rate,
          0 // FIXME: PRL -- make reassignment work with fisheye
       );
   }

   // build color gradient tables (not thread safe)
   if (!AColor::gradient_inited)
      AColor::PreComputeGradient();

   // left pixel column of the fisheye
   int fisheyeLeft = zoomInfo.GetFisheyeLeftBoundary(-leftOffset);

#ifdef _OPENMP
#pragma omp parallel for
#endif
   for (int xx = 0; xx < mid.width; ++xx) {

      int correctedX = xx + leftOffset - hiddenLeftOffset;

      // in fisheye mode the time scale has changed, so the row values aren't cached
      // in the loop above, and must be fetched from fft cache
      float* uncached;
      if (!zoomInfo.InFisheye(xx, -leftOffset)) {
          uncached = 0;
      }
      else {
          int specIndex = (xx - fisheyeLeft) * nBins;
          wxASSERT(specIndex >= 0 && specIndex < (int)specCache.freq.size());
          uncached = &specCache.freq[specIndex];
      }

      // zoomInfo must be queried for each column since with fisheye enabled
      // time between columns is variable
      auto w0 = sampleCount(0.5 + rate *
                   (zoomInfo.PositionToTime(xx, -leftOffset) - tOffset));

      auto w1 = sampleCount(0.5 + rate *
                    (zoomInfo.PositionToTime(xx+1, -leftOffset) - tOffset));

      bool maybeSelected = ssel0 <= w0 && w1 < ssel1;

      for (int yy = 0; yy < hiddenMid.height; ++yy) {
         const float bin     = bins[yy];
         const float nextBin = bins[yy+1];

         // For spectral selection, determine what colour
         // set to use.  We use a darker selection if
         // in both spectral range and time range.

         AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

         // If we are in the time selected range, then we may use a different color set.
         if (maybeSelected)
            selected =
               ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                  (xx + leftOffset - hiddenLeftOffset) / DASH_LENGTH, isSpectral);

         const float value = uncached
            ? findValue(uncached, bin, nextBin, nBins, autocorrelation, gain, range)
            : clip->mSpecPxCache->values[correctedX * hiddenMid.height + yy];

         unsigned char rv, gv, bv;
         GetColorGradient(value, selected, isGrayscale, &rv, &gv, &bv);

#ifdef EXPERIMENTAL_FFT_Y_GRID
         if (fftYGrid && yGrid[yy]) {
            rv /= 1.1f;
            gv /= 1.1f;
            bv /= 1.1f;
         }
#endif //EXPERIMENTAL_FFT_Y_GRID

         int px = ((mid.height - 1 - yy) * mid.width + xx);
#ifdef EXPERIMENTAL_SPECTROGRAM_OVERLAY
         // More transparent the closer to zero intensity.
         alpha[px]= wxMin( 200, (value+0.3) * 500) ;
#endif
         px *=3;
         data[px++] = rv;
         data[px++] = gv;
         data[px] = bv;
      } // each yy
   } // each xx

   wxBitmap converted = wxBitmap(image);

   wxMemoryDC memDC;

   memDC.SelectObject(converted);

   dc.Blit(mid.x, mid.y, mid.width, mid.height, &memDC, 0, 0, wxCOPY, FALSE);
}

void DrawSpectrum( TrackPanelDrawingContext &context,
                                const WaveTrack *track,
                                const wxRect & rect )
{
   const auto artist = TrackArtist::Get( context );
   const auto &blankSelectedBrush = artist->blankSelectedBrush;
   const auto &blankBrush = artist->blankBrush;
   TrackArt::DrawBackgroundWithSelection(
      context, rect, track, blankSelectedBrush, blankBrush );

   WaveTrackCache cache(track->SharedPointer<const WaveTrack>());
   for (const auto &clip: track->GetClips())
      DrawClipSpectrum( context, cache, clip.get(), rect );
}

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
            DrawWaveform(context, wt.get(), rect, muted);
            break;
         case WaveTrackViewConstants::Spectrum:
            DrawSpectrum( context, wt.get(), rect );
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
