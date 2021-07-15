/**********************************************************************

Audacity: A Digital Audio Editor

WaveformView.cpp

Paul Licameli split from WaveTrackView.cpp

**********************************************************************/


#include "WaveformView.h"

#include "WaveformVRulerControls.h"
#include "WaveTrackView.h"
#include "WaveTrackViewConstants.h"

#include "SampleHandle.h"
#include "../../../ui/EnvelopeHandle.h"
#include "../../../ui/TimeShiftHandle.h"
#include "../../../../AColor.h"
#include "../../../../Envelope.h"
#include "../../../../EnvelopeEditor.h"
#include "../../../../ProjectSettings.h"
#include "../../../../SelectedRegion.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"
#include "../../../../prefs/WaveformSettings.h"

#include <wx/graphics.h>
#include <wx/dc.h>

static WaveTrackSubView::Type sType{
   WaveTrackViewConstants::Waveform,
   { wxT("Waveform"), XXO("Wa&veform") }
};

static WaveTrackSubViewType::RegisteredType reg{ sType };

WaveformView::~WaveformView() = default;

std::vector<UIHandlePtr> WaveformView::DetailedHitTest(
   const TrackPanelMouseState &st,
   const AudacityProject *pProject, int currentTool, bool bMultiTool )
{
   auto &view = *this;
   const auto pTrack =
      std::static_pointer_cast< WaveTrack >( view.FindTrack() );

   auto pair = WaveTrackSubView::DoDetailedHitTest(
      st, pProject, currentTool, bMultiTool, pTrack);
   auto &results = pair.second;

   if (!pair.first) {
      UIHandlePtr result;

      if (bMultiTool) {
         // Conditional hit tests
         // If Tools toolbar were eliminated, we would keep these
         // The priority of these, in case more than one might apply at one
         // point, seems arbitrary
         if (NULL != (result = EnvelopeHandle::WaveTrackHitTest(
            view.mEnvelopeHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
         if (NULL != (result = TimeShiftHandle::HitTest(
            view.mTimeShiftHandle, st.state, st.rect, pTrack )))
            // This is the hit test on the "grips" drawn left and
            // right in Multi only
            results.push_back(result);
         if (NULL != (result = SampleHandle::HitTest(
            view.mSampleHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
      }
      else {
         switch ( currentTool ) {
               // Unconditional hits appropriate to the tool
               // If tools toolbar were eliminated, we would eliminate these
            case ToolCodes::envelopeTool: {
               auto &viewInfo = ViewInfo::Get(*pProject);
               auto time =
                  viewInfo.PositionToTime(st.state.m_x, st.rect.GetX());
               auto envelope = pTrack->GetEnvelopeAtTime(time);
               result = EnvelopeHandle::HitAnywhere(
                  view.mEnvelopeHandle, envelope, false);
               break;
            }
            case ToolCodes::drawTool:
               result = SampleHandle::HitAnywhere(
                  view.mSampleHandle, st.state, pTrack );
               break;
            default:
               result = {};
               break;
         }
         if (result)
            results.push_back(result);
      }
   }

   return std::move( results );
}

void WaveformView::DoSetMinimized( bool minimized )
{
   auto wt = static_cast<WaveTrack*>( FindTrack().get() );

#ifdef EXPERIMENTAL_HALF_WAVE
   bool bHalfWave;
   gPrefs->Read(wxT("/GUI/CollapseToHalfWave"), &bHalfWave, false);
   if( bHalfWave )
   {
      if (minimized)
         // Zoom to show fractionally more than the top half of the wave.
         wt->SetDisplayBounds( -0.01f, 1.0f );
      else
         // Zoom out full
         wt->SetDisplayBounds( -1.0f, 1.0f );
   }
#endif

   TrackView::DoSetMinimized( minimized );
}

auto WaveformView::SubViewType() const -> const Type &
{
   return sType;
}

std::shared_ptr<TrackVRulerControls> WaveformView::DoGetVRulerControls()
{
   return std::make_shared<WaveformVRulerControls>( shared_from_this() );
}

namespace
{

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

   // Bug 2389 - always draw at least one pixel of selection.
   int selectedX = zoomInfo.TimeToPosition(t0, -leftOffset);

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

      sel = (t0 <= time && nextTime < t1);
      sel = sel || (xx == selectedX);
      // We don't draw selection color for sync-lock selected tracks.
      sel = sel && !bIsSyncLockSelected;

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

void DrawMinMaxRMS(
   TrackPanelDrawingContext &context, const wxRect & rect, const double env[],
   float zoomMin, float zoomMax,
   bool dB, float dBRange,
   const float *min, const float *max, const float *rms, const int *bl,
   bool muted)
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
                                   bool muted,
                                   bool selected)
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

         if (!clip->GetWaveDisplay(display,t0, pps))
            return;
      }
   }

   // TODO Add a comment to say what this loop does.
   // Possibly make it into a subroutine.
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
                     fisheyeDisplay, t0, -1.0)) // ignored
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
               useMin, useMax, useRms, useBl, muted );
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
   {
      //increase virtual view size by px to hide edges that should not be visible
      auto clipRect = ClipParameters::GetClipRect(*clip, zoomInfo, rect.Inflate(1, 0), 1);
      if (!clipRect.IsEmpty())
          TrackArt::DrawClipEdges(dc, clipRect, selected);
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

}

// Header needed only for experimental drawing below
//#include "tracks/ui/TimeShiftHandle.h"
void WaveformView::DoDraw(TrackPanelDrawingContext &context,
                               const WaveTrack *track,
                               const WaveClip* selectedClip,
                               const wxRect& rect,
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

   for (const auto& clip : track->GetClips())
   {
      DrawClipWaveform(context, track, clip.get(), rect,
         dB, muted, clip.get() == selectedClip);
   }
   DrawBoldBoundaries( context, track, rect );

   const auto drawSliders = artist->drawSliders;
   if (drawSliders) {
      DrawTimeSlider( context, rect, true, highlight && gripHit );  // directed right
      DrawTimeSlider( context, rect, false, highlight && gripHit ); // directed left
   }
}

void WaveformView::Draw(
   TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      auto &dc = context.dc;
      // Update cache for locations, e.g. cutlines and merge points
      // Bug2588: do this for both channels, even if one is not drawn, so that
      // cut-line editing (which depends on the locations cache) works properly.
      // If both channels are visible, we will duplicate this effort, but that
      // matters little.
      for( auto channel:
          TrackList::Channels(static_cast<WaveTrack*>(FindTrack().get())) )
         channel->UpdateLocationsCache();

      const auto wt = std::static_pointer_cast<const WaveTrack>(
         FindTrack()->SubstitutePendingChangedTrack());

      const auto artist = TrackArtist::Get( context );
      const auto hasSolo = artist->hasSolo;
      bool muted = (hasSolo || wt->GetMute()) &&
      !wt->GetSolo();
      
#if defined(__WXMAC__)
      wxAntialiasMode aamode = dc.GetGraphicsContext()->GetAntialiasMode();
      dc.GetGraphicsContext()->SetAntialiasMode(wxANTIALIAS_NONE);
#endif
      
      auto waveTrackView = GetWaveTrackView().lock();
      wxASSERT(waveTrackView.use_count());

      auto selectedClip = waveTrackView->GetSelectedClip().lock();
      DoDraw(context, wt.get(), selectedClip.get(), rect, muted);

#if defined(__WXMAC__)
      dc.GetGraphicsContext()->SetAntialiasMode(aamode);
#endif
   }
   WaveTrackSubView::Draw( context, rect, iPass );
}

static const WaveTrackSubViews::RegisteredFactory key{
   []( WaveTrackView &view ){
      return std::make_shared< WaveformView >( view );
   }
};

// The following attaches the wave color sub-menu to the wave track popup
// menu.  It is appropriate only to waveform view and so is kept in this
// source file with the rest of the waveform view implementation.

#include <mutex> // for std::call_once
#include "WaveTrackControls.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../ProjectAudioIO.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"

//=============================================================================
// Table class for a sub-menu
struct WaveColorMenuTable : PopupMenuTable
{
   WaveColorMenuTable() : PopupMenuTable{ "WaveColor", XO("&Wave Color") } {}
   DECLARE_POPUP_MENU(WaveColorMenuTable);

   static WaveColorMenuTable &Instance();

   void InitUserData(void *pUserData) override;

   void DestroyMenu() override
   {
      mpData = NULL;
   }

   PlayableTrackControls::InitMenuData *mpData{};

   int IdOfWaveColor(int WaveColor);
   void OnWaveColorChange(wxCommandEvent & event);

   int OnInstrument1ID, OnInstrument2ID, OnInstrument3ID, OnInstrument4ID;
};

WaveColorMenuTable &WaveColorMenuTable::Instance()
{
   static WaveColorMenuTable instance;
   return instance;
}

void WaveColorMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<PlayableTrackControls::InitMenuData*>(pUserData);
}

namespace {
using ValueFinder = std::function< int( WaveTrack& ) >;

const TranslatableString GetWaveColorStr(int colorIndex)
{
   return XXO("Instrument %i").Format( colorIndex+1 );
}
}

BEGIN_POPUP_MENU(WaveColorMenuTable)
   static const auto fn = []( PopupMenuHandler &handler, wxMenu &menu, int id ){
      auto &me = static_cast<WaveColorMenuTable&>( handler );
      auto pData = me.mpData;
      const auto &track = *static_cast<WaveTrack*>(pData->pTrack);
      auto &project = pData->project;
      bool unsafe = ProjectAudioIO::Get( project ).IsAudioActive();
      
      menu.Check( id, id == me.IdOfWaveColor( track.GetWaveColorIndex() ) );
      menu.Enable( id, !unsafe );
   };

   static std::once_flag flag;
   std::call_once( flag, [this]{
      auto &hostTable = GetWaveTrackMenuTable();
      OnInstrument1ID = hostTable.ReserveId();
      OnInstrument2ID = hostTable.ReserveId();
      OnInstrument3ID = hostTable.ReserveId();
      OnInstrument4ID = hostTable.ReserveId();
   } );

   AppendRadioItem( "Instrument1", OnInstrument1ID,
      GetWaveColorStr(0), POPUP_MENU_FN( OnWaveColorChange ), fn );
   AppendRadioItem( "Instrument2", OnInstrument2ID,
      GetWaveColorStr(1), POPUP_MENU_FN( OnWaveColorChange ), fn );
   AppendRadioItem( "Instrument3", OnInstrument3ID,
      GetWaveColorStr(2), POPUP_MENU_FN( OnWaveColorChange ), fn );
   AppendRadioItem( "Instrument4", OnInstrument4ID,
      GetWaveColorStr(3), POPUP_MENU_FN( OnWaveColorChange ), fn );

END_POPUP_MENU()

/// Converts a WaveColor enumeration to a wxWidgets menu item Id.
int WaveColorMenuTable::IdOfWaveColor(int WaveColor)
{  return OnInstrument1ID + WaveColor;}

/// Handles the selection from the WaveColor submenu of the
/// track menu.
void WaveColorMenuTable::OnWaveColorChange(wxCommandEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnInstrument1ID && id <= OnInstrument4ID);
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);

   int newWaveColor = id - OnInstrument1ID;

   AudacityProject *const project = &mpData->project;

   for (auto channel : TrackList::Channels(pTrack))
      channel->SetWaveColorIndex(newWaveColor);

   ProjectHistory::Get( *project )
      .PushState(XO("Changed '%s' to %s")
         .Format( pTrack->GetName(), GetWaveColorStr(newWaveColor) ),
      XO("WaveColor Change"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

namespace {
PopupMenuTable::AttachedItem sAttachment{
   GetWaveTrackMenuTable(),
   { "SubViews/Extra" },
   std::make_unique<PopupMenuSection>( "WaveColor",
      // Conditionally add sub-menu for wave color, if showing waveform
      PopupMenuTable::Computed< WaveTrackPopupMenuTable >(
         []( WaveTrackPopupMenuTable &table ) -> Registry::BaseItemPtr {
            const auto pTrack = &table.FindWaveTrack();
            const auto &view = WaveTrackView::Get( *pTrack );
            const auto displays = view.GetDisplays();
            bool hasWaveform = (displays.end() != std::find(
               displays.begin(), displays.end(),
               WaveTrackSubView::Type{ WaveTrackViewConstants::Waveform, {} }
            ) );
            if( hasWaveform )
               return Registry::Shared( WaveColorMenuTable::Instance()
                  .Get( table.mpData ) );
            else
               return nullptr;
         } ) )
};
}

