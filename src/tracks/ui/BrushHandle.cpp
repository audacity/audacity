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

   mMostRecentX = event.m_x;
   mMostRecentY = event.m_y;

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

   int x = mAutoScrolling ? mMostRecentX : event.m_x;
   int y = mAutoScrolling ? mMostRecentY : event.m_y;
   mMostRecentX = x;
   mMostRecentY = y;

   double posTime;
   wxInt64 posFreq;
   auto posToLongLong = [&](int x0){
      posTime = viewInfo.PositionToTime(x0, mRect.x);
      sampleCount sc = wt->TimeToLongSamples(posTime);
      return sc.as_long_long();
   };

   // Clip the coordinates
   // TODO: Find ways to access the ClipParameters (for the mid)
   int dest_xcoord = std::max(mRect.x + 10, std::min(x, mRect.x + mRect.width - 20));
   int dest_ycoord = std::max(mRect.y + 10, std::min(y, mRect.y + mRect.height - 10));
   int destFreq = PositionToFrequency(wt, 0, dest_ycoord, mRect.y, mRect.height);

   int x1 = posToLongLong(dest_xcoord) / mpSpectralData->GetHopSize();
   int y1 = destFreq / (mpSpectralData->GetSR() / mpSpectralData->GetWindowSize());

   mbCtrlDown = event.ControlDown();

   // Use the hop and bin number to calculate the brush stroke, instead of the mouse coordinates
   // For mouse coordinate:
   // (src_xcoord, src_ycoord) -> (dest_xcoord, dest_ycoord)
   //
   // Variables for Bresenham's line algorithm:
   // x0, x1, y0, y1... (already transformed in hops and bins space}
   if(!mpSpectralData->coordHistory.empty()){
      int src_xcoord = mpSpectralData->coordHistory.back().first;
      int src_ycoord = mpSpectralData->coordHistory.back().second;
      int srcFreq = PositionToFrequency(wt, 0, src_ycoord, mRect.y, mRect.height);

      int x0 = posToLongLong(src_xcoord) / mpSpectralData->GetHopSize();
      int y0 = srcFreq / (mpSpectralData->GetSR() / mpSpectralData->GetWindowSize());
      int wd = mBrushRadius * 2;

      int dx =  abs(x1-x0), sx = x0<x1 ? 1 : -1;
      int dy = -abs(y1-y0), sy = y0<y1 ? 1 : -1;
      int err = dx+dy, err2;

      // Line drawing (draw points until the start coordinate reaches the end)
      while(true){
         if (x0 == x1 && y0 == y1)
            break;
         err2 = err * 2;
         if (err2 * 2 >= dy) { err += dy; x0 += sx; }
         if (err2 * 2 <= dx) { err += dx; y0 += sy; }

         // For each (x0, y0), draw circle
         int x2 = mBrushRadius;
         int y2 = 0;
         int xChange = 1 - (mBrushRadius << 1);
         int yChange = 0;
         int radiusError = 0;

         int ym = y0;
         if(mIsSmartSelection){
            // Correct the y coord (snap to highest energy freq. bin)
            long long startSC = posToLongLong(x0);
            wxInt64 targetFreq = PositionToFrequency(wt, 0, ym, mRect.y, mRect.height);
            if(auto *sView = dynamic_cast<SpectrumView*>(pView.get())){
               wxInt64 resFreq = SpectralDataManager::FindFrequencySnappingBin(wt,
                                                                               startSC,
                                                                               mFreqSnappingRatio,
                                                                               targetFreq);
               if(resFreq != - 1)
                  ym = FrequencyToPosition(wt, resFreq, mRect.y, mRect.height);
            }
         }

         while (x2 >= y2) {
            for (int i = x0 - x2; i <= x0 + x2; i++)
            {
               HandleHopBinData(i, ym+y2);
               HandleHopBinData(i, ym-y2);
            }
            for (int i = x0 - y2; i <= x0 + y2; i++)
            {
               HandleHopBinData(i, ym+x2);
               HandleHopBinData(i, ym-x2);
            }

            y2++;
            radiusError += yChange;
            yChange += 2;
            if (((radiusError << 1) + xChange) > 0)
            {
               x2--;
               radiusError += xChange;
               xChange += 2;
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
   mSelectionStateChanger.reset();
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

void BrushHandle::Connect(AudacityProject *pProject)
{
   mTimerHandler = std::make_shared<TimerHandler>( this, pProject );
}

class BrushHandle::TimerHandler : public wxEvtHandler
{
public:
   TimerHandler( BrushHandle *pParent, AudacityProject *pProject )
         : mParent{ pParent }
         , mConnectedProject{ pProject }
   {
      if (mConnectedProject)
         mConnectedProject->Bind(EVT_TRACK_PANEL_TIMER,
                                 &BrushHandle::TimerHandler::OnTimer,
                                 this);
   }

   // Receives timer event notifications, to implement auto-scroll
   void OnTimer(wxCommandEvent &event);

private:
   BrushHandle *mParent;
   AudacityProject *mConnectedProject;
};

void BrushHandle::TimerHandler::OnTimer(wxCommandEvent &event)
{
   event.Skip();

   // AS: If the user is dragging the mouse and there is a track that
   //  has captured the mouse, then scroll the screen, as necessary.

   ///  We check on each timer tick to see if we need to scroll.

   // DM: If we're "autoscrolling" (which means that we're scrolling
   //  because the user dragged from inside to outside the window,
   //  not because the user clicked in the scroll bar), then
   //  the selection code needs to be handled slightly differently.
   //  We set this flag ("mAutoScrolling") to tell the selecting
   //  code that we didn't get here as a result of a mouse event,
   //  and therefore it should ignore the event,
   //  and instead use the last known mouse position.  Setting
   //  this flag also causes the Mac to redraw immediately rather
   //  than waiting for the next update event; this makes scrolling
   //  smoother on MacOS 9.

   const auto project = mConnectedProject;
   const auto &trackPanel = TrackPanel::Get( *project );
   auto &window = ProjectWindow::Get( *project );
   if (mParent->mMostRecentX >= mParent->mRect.x + mParent->mRect.width) {
      mParent->mAutoScrolling = true;
      window.TP_ScrollRight();
   }
   else if (mParent->mMostRecentX < mParent->mRect.x) {
      mParent->mAutoScrolling = true;
      window.TP_ScrollLeft();
   }
   else {
      // Bug1387:  enable autoscroll during drag, if the pointer is at either
      // extreme x coordinate of the screen, even if that is still within the
      // track area.

      int xx = mParent->mMostRecentX, yy = 0;
      trackPanel.ClientToScreen(&xx, &yy);
      if (xx == 0) {
         mParent->mAutoScrolling = true;
         window.TP_ScrollLeft();
      }
      else {
         int width, height;
         ::wxDisplaySize(&width, &height);
         if (xx == width - 1) {
            mParent->mAutoScrolling = true;
            window.TP_ScrollRight();
         }
      }
   }

   auto pTrack = mParent->FindTrack().lock(); // TrackList::Lock() ?
   if (mParent->mAutoScrolling && pTrack) {
      // AS: To keep the selection working properly as we scroll,
      //  we fake a mouse event (remember, this method is called
      //  from a timer tick).

      // AS: For some reason, GCC won't let us pass this directly.
      wxMouseEvent evt(wxEVT_MOTION);
      const auto size = trackPanel.GetSize();
      mParent->Drag(
            TrackPanelMouseEvent{
                  evt, mParent->mRect, size,
                  TrackView::Get( *pTrack ).shared_from_this() },
            project
      );
      mParent->mAutoScrolling = false;
      TrackPanel::Get( *mConnectedProject ).Refresh(false);
   }
}
