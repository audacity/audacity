/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.cpp

Edward Hui

**********************************************************************/


#include "BrushHandle.h"
#include "Scrubbing.h"
#include "TrackView.h"

#include "../../AColor.h"
#include "../../SpectrumAnalyst.h"
#include "../../NumberScale.h"
#include "../../Project.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectHistory.h"
#include "../../ProjectSettings.h"
#include "../../ProjectWindow.h"
#include "../../RefreshCode.h"
#include "../../SelectUtilities.h"
#include "../../SelectionState.h"
#include "../../SpectralDataManager.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelAx.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../ViewInfo.h"
#include "../../WaveClip.h"
#include "../../WaveTrack.h"
#include "../../prefs/SpectrogramSettings.h"
#include "../../../images/Cursors.h"
#include "../playabletrack/wavetrack/ui/SpectrumView.h"

#include <tgmath.h>
#include <wx/event.h>
#include <iostream>

enum {
   //This constant determines the size of the horizontal region (in pixels) around
   //the right and left selection bounds that can be used for horizontal selection adjusting
   //(or, vertical distance around top and bottom bounds in spectrograms,
   // for vertical selection adjusting)
   SELECTION_RESIZE_REGION = 3,

   // Seems 4 is too small to work at the top.  Why?
   FREQ_SNAP_DISTANCE = 10,
};

// #define SPECTRAL_EDITING_ESC_KEY

bool BrushHandle::IsClicked() const
{
   return mSelectionStateChanger.get() != NULL;
}

namespace
{
   /// Converts a frequency to screen y position.
   wxInt64 FrequencyToPosition(const WaveTrack *wt,
                               double frequency,
                               wxInt64 trackTopEdge,
                               int trackHeight)
   {
      const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
      float minFreq, maxFreq;
      wt->GetSpectrumBounds(&minFreq, &maxFreq);
      const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
      const float p = numberScale.ValueToPosition(frequency);
      return trackTopEdge + wxInt64((1.0 - p) * trackHeight);
   }

   /// Converts a position (mouse Y coordinate) to
   /// frequency, in Hz.
   double PositionToFrequency(const WaveTrack *wt,
                              bool maySnap,
                              wxInt64 mouseYCoordinate,
                              wxInt64 trackTopEdge,
                              int trackHeight)
   {
      const double rate = wt->GetRate();

      // Handle snapping
      if (maySnap &&
          mouseYCoordinate - trackTopEdge < FREQ_SNAP_DISTANCE)
         return rate;
      if (maySnap &&
          trackTopEdge + trackHeight - mouseYCoordinate < FREQ_SNAP_DISTANCE)
         return -1;

      const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
      float minFreq, maxFreq;
      wt->GetSpectrumBounds(&minFreq, &maxFreq);
      const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
      const double p = double(mouseYCoordinate - trackTopEdge) / trackHeight;
      return numberScale.PositionToValue(1.0 - p);
   }

   long long PositionToLongSample(const WaveTrack *wt,
                                  const ViewInfo &viewInfo,
                                  int trackTopEdgeX,
                                  int mousePosX)
   {
      wxInt64 posTime = viewInfo.PositionToTime(mousePosX, trackTopEdgeX);
      sampleCount sc = wt->TimeToLongSamples(posTime);
      return sc.as_long_long();
   }

   template<typename T>
   inline void SetIfNotNull(T * pValue, const T Value)
   {
      if (pValue == NULL)
         return;
      *pValue = Value;
   }

   // This returns true if we're a spectral editing track.
   inline bool isSpectralSelectionView(const TrackView *pTrackView) {
      return
            pTrackView &&
            pTrackView->IsSpectral() &&
            pTrackView->FindTrack() &&
            pTrackView->FindTrack()->TypeSwitch< bool >(
                  [&](const WaveTrack *wt) {
                     const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
                     return settings.SpectralSelectionEnabled();
                  });
   }
   wxCursor *CrosshairCursor()
   {
      static auto crosshairCursor =
            ::MakeCursor(wxCURSOR_IBEAM, CrosshairCursorXpm, 16, 16);
      return &*crosshairCursor;
   }
}

UIHandlePtr BrushHandle::HitTest
      (std::weak_ptr<BrushHandle> &holder,
       const TrackPanelMouseState &st, const AudacityProject *pProject,
       const std::shared_ptr<TrackView> &pTrackView,
       const std::shared_ptr<SpectralData> &mpData)
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto &projectSettings = ProjectSettings::Get( *pProject );
   auto result = std::make_shared<BrushHandle>(
      pTrackView, TrackList::Get( *pProject ),
      st, viewInfo, mpData, projectSettings);

   result = AssignUIHandlePtr(holder, result);

   //Make sure we are within the selected track
   // Adjusting the selection edges can be turned off in
   // the preferences...
   auto pTrack = pTrackView->FindTrack();
   if (!pTrack->GetSelected() || !viewInfo.bAdjustSelectionEdges)
   {
      return result;
   }

   return result;
}

BrushHandle::BrushHandle
      ( const std::shared_ptr<TrackView> &pTrackView,
        const TrackList &trackList,
        const TrackPanelMouseState &st, const ViewInfo &viewInfo,
        const std::shared_ptr<SpectralData> &pSpectralData,
        const ProjectSettings &pSettings)
      : mpView{ pTrackView }
      , mpSpectralData(pSpectralData)
{
   const wxMouseState &state = st.state;
   auto pTrack = pTrackView->FindTrack().get();
   auto wt = dynamic_cast<WaveTrack *>(pTrack);
   double rate = mpSpectralData->GetSR();

   mRect = st.rect;
   mBrushRadius = pSettings.GetBrushRadius();
   mFreqUpperBound = wt->GetSpectrogramSettings().maxFreq - 1;
   mFreqLowerBound = wt->GetSpectrogramSettings().minFreq + 1;
   mIsSmartSelection = pSettings.IsSmartSelection();
   mIsOvertones = pSettings.IsOvertones();
   // Borrowed from TimeToLongSample
   mSampleCountLowerBound = floor( pTrack->GetStartTime() * rate + 0.5);
   mSampleCountUpperBound = floor( pTrack->GetEndTime() * rate + 0.5);
}

BrushHandle::~BrushHandle()
{
}

namespace {
   // Is the distance between A and B less than D?
   template < class A, class B, class DIST > bool within(A a, B b, DIST d)
   {
      return (a > b - d) && (a < b + d);
   }

   inline double findMaxRatio(double center, double rate)
   {
      const double minFrequency = 1.0;
      const double maxFrequency = (rate / 2.0);
      const double frequency =
            std::min(maxFrequency,
                     std::max(minFrequency, center));
      return
            std::min(frequency / minFrequency, maxFrequency / frequency);
   }
}

void BrushHandle::Enter(bool, AudacityProject *project)
{

}

bool BrushHandle::Escape(AudacityProject *project)
{
   return false;
}

// Add or remove data accroding to the ctrl key press
void BrushHandle::HandleHopBinData(int hopNum, int freqBinNum) {
   // Ignore the mouse dragging outside valid area
   // We should also check for the available freq. range that is visible to user
   long long sampleCount = hopNum * mpSpectralData->GetHopSize();
   wxInt64 freq = freqBinNum * mpSpectralData->GetSR() / mpSpectralData->GetWindowSize();

   if(sampleCount < mSampleCountLowerBound || sampleCount > mSampleCountUpperBound ||
      freq < mFreqLowerBound || freq > mFreqUpperBound)
      return;

   if(mbCtrlDown)
      mpSpectralData->removeHopBinData(hopNum, freqBinNum);
   else
      mpSpectralData->addHopBinData(hopNum, freqBinNum);
}

UIHandle::Result BrushHandle::Click
      (const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;

   const auto pView = mpView.lock();
   if ( !pView )
      return Cancelled;

   wxMouseEvent &event = evt.event;
   const auto sTrack = TrackList::Get( *pProject ).Lock( FindTrack() );
   const auto pTrack = sTrack.get();
   const WaveTrack *const wt =
         static_cast<const WaveTrack*>(pTrack);
   auto &trackPanel = TrackPanel::Get( *pProject );
   auto &viewInfo = ViewInfo::Get( *pProject );

   return RefreshAll;
}

UIHandle::Result BrushHandle::Drag
      (const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;

   const auto pView = mpView.lock();
   if ( !pView )
      return Cancelled;

   wxMouseEvent &event = evt.event;
   const auto sTrack = TrackList::Get( *pProject ).Lock( FindTrack() );
   const auto pTrack = sTrack.get();
   WaveTrack *const wt =
         static_cast<WaveTrack*>(pTrack);
   auto &trackPanel = TrackPanel::Get( *pProject );
   auto &viewInfo = ViewInfo::Get( *pProject );

   auto posXToHopNum = [&](int x0){
      double posTime = viewInfo.PositionToTime(x0, mRect.x);
      sampleCount sc = wt->TimeToLongSamples(posTime);
      return sc.as_long_long() / mpSpectralData->GetHopSize();
   };

   auto posYToFreqBin = [&](int y0){
      int resFreq = PositionToFrequency(wt, 0, y0, mRect.y, mRect.height);
      return resFreq / (mpSpectralData->GetSR() / mpSpectralData->GetWindowSize());
   };

   // Clip the coordinates
   // TODO: Find ways to access the ClipParameters (for the mid)
   int dest_xcoord = std::clamp(event.m_x, mRect.x + 10, mRect.x + mRect.width);
   int dest_ycoord = std::clamp(event.m_y, mRect.y + 10, mRect.y + mRect.height);

   int h1 = posXToHopNum(dest_xcoord);
   int b1 = posYToFreqBin(dest_ycoord);

   mbCtrlDown = event.ControlDown();

   // Use the hop and bin number to calculate the brush stroke, instead of the mouse coordinates
   // For mouse coordinate:
   // (src_xcoord, src_ycoord) -> (dest_xcoord, dest_ycoord)
   if(!mpSpectralData->coordHistory.empty()){
      int src_xcoord = mpSpectralData->coordHistory.back().first;
      int src_ycoord = mpSpectralData->coordHistory.back().second;

      int h0 = posXToHopNum(src_xcoord);
      int b0 = posYToFreqBin(src_ycoord);
      int wd = mBrushRadius * 2;

      int dh =  abs(h1-h0), sh = h0<h1 ? 1 : -1;
      int db = -abs(b1-b0), sb = b0<b1 ? 1 : -1;
      int err = dh+db, err2;

      // Line drawing (draw points until the start coordinate reaches the end)
      while(true){
         if (h0 == h1 && b0 == b1)
            break;
         err2 = err * 2;
         if (err2 * 2 >= db) { err += db; h0 += sh; }
         if (err2 * 2 <= dh) { err += dh; b0 += sb; }

         // For each (h0, b0), draw circle
         int h2 = mBrushRadius;
         int b2 = 0;
         int hChange = 1 - (mBrushRadius << 1);
         int bChange = 0;
         int radiusError = 0;

         int bm = b0;
         if(mIsSmartSelection){
            // Correct the y coord (snap to highest energy freq. bin)
            if(auto *sView = dynamic_cast<SpectrumView*>(pView.get())){
               int resFreqBin = SpectralDataManager::FindFrequencySnappingBin(wt,
                  h0 * mpSpectralData -> GetHopSize(), mFreqSnappingRatio, bm);
               if(resFreqBin != - 1)
                  bm = resFreqBin;
            }
         }

         while (h2 >= b2) {
            for (int i = h0 - h2; i <= h0 + h2; i++)
            {
               HandleHopBinData(i, bm + b2);
               HandleHopBinData(i, bm - b2);
            }
            for (int i = h0 - b2; i <= h0 + b2; i++)
            {
               HandleHopBinData(i, bm + h2);
               HandleHopBinData(i, bm - h2);
            }

            b2++;
            radiusError += bChange;
            bChange += 2;
            if (((radiusError << 1) + hChange) > 0)
            {
               h2--;
               radiusError += hChange;
               hChange += 2;
            }
         } // End of full circle drawing
      } // End of line connecting
   }
   mpSpectralData->coordHistory.push_back(std::make_pair(dest_xcoord, dest_ycoord));
   return RefreshAll;
}

HitTestPreview BrushHandle::Preview
      (const TrackPanelMouseState &st, AudacityProject *pProject)
{
   TranslatableString tip;
   wxCursor *pCursor = CrosshairCursor();
   return { tip, pCursor };
}

UIHandle::Result BrushHandle::Release
      (const TrackPanelMouseEvent &evt, AudacityProject *pProject,
       wxWindow *)
{
   using namespace RefreshCode;
   mpSpectralData->saveAndClearBuffer();
   if(mbCtrlDown){
      ProjectHistory::Get( *pProject ).PushState(
            XO( "Erased selected area" ),
            XO( "Erased selected area" ) );
      ProjectHistory::Get( *pProject ).ModifyState(true);
   }
   else{
      ProjectHistory::Get( *pProject ).PushState(
            XO( "Selected area using Brush Tool" ),
            XO( "Brush tool selection" ) );
      ProjectHistory::Get( *pProject ).ModifyState(true);
   }

   return RefreshNone;
}

UIHandle::Result BrushHandle::Cancel(AudacityProject *pProject)
{
   mpSpectralData->dataBuffer.clear();
   mpSpectralData->coordHistory.clear();

   return RefreshCode::RefreshAll;
}

void BrushHandle::Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      auto& dc = context.dc;
      wxPoint coord;
      coord.x = mMostRecentX;
      coord.y = mMostRecentY;
      dc.SetBrush( *wxTRANSPARENT_BRUSH );
      dc.SetPen( *wxYELLOW_PEN );
      dc.DrawCircle(coord, mBrushRadius);
   }
}

std::weak_ptr<Track> BrushHandle::FindTrack()
{
   auto pView = mpView.lock();
   if (!pView)
      return {};
   else
      return pView->FindTrack();
}
