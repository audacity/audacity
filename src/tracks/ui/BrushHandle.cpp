/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.cpp

Paul Licameli split from TrackPanel.cpp

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

#include <wx/event.h>

// Only for definition of SonifyBeginModifyState:
//#include "../../NoteTrack.h"

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

   enum SelectionBoundary {
      SBNone,
      SBLeft, SBRight,
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      SBBottom, SBTop, SBCenter, SBWidth,
#endif
   };

   SelectionBoundary ChooseTimeBoundary
      (
      const double t0, const double t1,
      const ViewInfo &viewInfo,
      double selend, bool onlyWithinSnapDistance,
      wxInt64 *pPixelDist, double *pPinValue)
   {
      const wxInt64 posS = viewInfo.TimeToPosition(selend);
      const wxInt64 pos0 = viewInfo.TimeToPosition(t0);
      wxInt64 pixelDist = std::abs(posS - pos0);
      bool chooseLeft = true;

      if (t1<=t0)
         // Special case when selection is a point, and thus left
         // and right distances are the same
         chooseLeft = (selend < t0);
      else {
         const wxInt64 pos1 = viewInfo.TimeToPosition(t1);
         const wxInt64 rightDist = std::abs(posS - pos1);
         if (rightDist < pixelDist)
            chooseLeft = false, pixelDist = rightDist;
      }

      SetIfNotNull(pPixelDist, pixelDist);

      if (onlyWithinSnapDistance &&
         pixelDist >= SELECTION_RESIZE_REGION) {
         SetIfNotNull(pPinValue, -1.0);
         return SBNone;
      }
      else if (chooseLeft) {
         SetIfNotNull(pPinValue, t1);
         return SBLeft;
      }
      else {
         SetIfNotNull(pPinValue, t0);
         return SBRight;
      }
   }

   SelectionBoundary ChooseBoundary
      (const ViewInfo &viewInfo,
       wxCoord xx, wxCoord yy, const TrackView *pTrackView, const wxRect &rect,
       bool mayDragWidth, bool onlyWithinSnapDistance,
       double *pPinValue = NULL)
   {
      // Choose one of four boundaries to adjust, or the center frequency.
      // May choose frequencies only if in a spectrogram view and
      // within the time boundaries.
      // May choose no boundary if onlyWithinSnapDistance is true.
      // Otherwise choose the eligible boundary nearest the mouse click.
      const double selend = viewInfo.PositionToTime(xx, rect.x);
      wxInt64 pixelDist = 0;
      const double t0 = viewInfo.selectedRegion.t0();
      const double t1 = viewInfo.selectedRegion.t1();

      SelectionBoundary boundary =
         ChooseTimeBoundary(t0,t1,viewInfo, selend, onlyWithinSnapDistance,
         &pixelDist, pPinValue);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      //const double t0 = viewInfo.selectedRegion.t0();
      //const double t1 = viewInfo.selectedRegion.t1();
      const double f0 = viewInfo.selectedRegion.f0();
      const double f1 = viewInfo.selectedRegion.f1();
      const double fc = viewInfo.selectedRegion.fc();
      double ratio = 0;

      bool chooseTime = true;
      bool chooseBottom = true;
      bool chooseCenter = false;
      // Consider adjustment of frequencies only if mouse is
      // within the time boundaries
      if (!viewInfo.selectedRegion.isPoint() &&
         t0 <= selend && selend < t1 &&
         isSpectralSelectionView(pTrackView)) {
         // Spectral selection track is always wave
         auto pTrack = pTrackView->FindTrack();
         const WaveTrack *const wt =
           static_cast<const WaveTrack*>(pTrack.get());
         const wxInt64 bottomSel = (f0 >= 0)
            ? FrequencyToPosition(wt, f0, rect.y, rect.height)
            : rect.y + rect.height;
         const wxInt64 topSel = (f1 >= 0)
            ? FrequencyToPosition(wt, f1, rect.y, rect.height)
            : rect.y;
         wxInt64 signedBottomDist = (int)(yy - bottomSel);
         wxInt64 verticalDist = std::abs(signedBottomDist);
         if (bottomSel == topSel)
            // Top and bottom are too close to resolve on screen
            chooseBottom = (signedBottomDist >= 0);
         else {
            const wxInt64 topDist = std::abs((int)(yy - topSel));
            if (topDist < verticalDist)
               chooseBottom = false, verticalDist = topDist;
         }
         if (fc > 0
#ifdef SPECTRAL_EDITING_ESC_KEY
            && mayDragWidth
#endif
            ) {
            const wxInt64 centerSel =
               FrequencyToPosition(wt, fc, rect.y, rect.height);
            const wxInt64 centerDist = abs((int)(yy - centerSel));
            if (centerDist < verticalDist)
               chooseCenter = true, verticalDist = centerDist,
               ratio = f1 / fc;
         }
         if (verticalDist >= 0 &&
            verticalDist < pixelDist) {
            pixelDist = verticalDist;
            chooseTime = false;
         }
      }

      if (!chooseTime) {
         // PRL:  Seems I need a larger tolerance to make snapping work
         // at top of track, not sure why
         if (onlyWithinSnapDistance &&
            pixelDist >= FREQ_SNAP_DISTANCE) {
            SetIfNotNull(pPinValue, -1.0);
            return SBNone;
         }
         else if (chooseCenter) {
            SetIfNotNull(pPinValue, ratio);
            return SBCenter;
         }
         else if (mayDragWidth && fc > 0) {
            SetIfNotNull(pPinValue, fc);
            return SBWidth;
         }
         else if (chooseBottom) {
            SetIfNotNull(pPinValue, f1);
            return SBBottom;
         }
         else {
            SetIfNotNull(pPinValue, f0);
            return SBTop;
         }
      }
      else
#endif
      {
         return boundary;
      }
   }

   wxCursor *SelectCursor()
   {
      static auto selectCursor =
         ::MakeCursor(wxCURSOR_IBEAM, IBeamCursorXpm, 17, 16);
      return &*selectCursor;
   }

   wxCursor *EnvelopeCursor()
   {
      // This one doubles as the center frequency cursor for spectral selection:
      static auto envelopeCursor =
         ::MakeCursor(wxCURSOR_ARROW, EnvCursorXpm, 16, 16);
      return &*envelopeCursor;
   }

   void SetTipAndCursorForBoundary
      (SelectionBoundary boundary, bool frequencySnapping,
       TranslatableString &tip, wxCursor *&pCursor)
   {
      static wxCursor adjustLeftSelectionCursor{ wxCURSOR_POINT_LEFT };
      static wxCursor adjustRightSelectionCursor{ wxCURSOR_POINT_RIGHT };

      static auto bottomFrequencyCursor =
         ::MakeCursor(wxCURSOR_ARROW, BottomFrequencyCursorXpm, 16, 16);
      static auto topFrequencyCursor =
         ::MakeCursor(wxCURSOR_ARROW, TopFrequencyCursorXpm, 16, 16);
      static auto bandWidthCursor =
         ::MakeCursor(wxCURSOR_ARROW, BandWidthCursorXpm, 16, 16);

      switch (boundary) {
      case SBNone:
         pCursor = SelectCursor();
         break;
      case SBLeft:
         tip = XO("Click and drag to move left selection boundary.");
         pCursor = &adjustLeftSelectionCursor;
         break;
      case SBRight:
         tip = XO("Click and drag to move right selection boundary.");
         pCursor = &adjustRightSelectionCursor;
         break;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      case SBBottom:
         tip = XO("Click and drag to move bottom selection frequency.");
         pCursor = &*bottomFrequencyCursor;
         break;
      case SBTop:
         tip = XO("Click and drag to move top selection frequency.");
         pCursor = &*topFrequencyCursor;
         break;
      case SBCenter:
      {
#ifndef SPECTRAL_EDITING_ESC_KEY
         tip =
            frequencySnapping ?
            XO("Click and drag to move center selection frequency to a spectral peak.") :
            XO("Click and drag to move center selection frequency.");

#else
         shiftDown;

         tip =
            XO("Click and drag to move center selection frequency.");

#endif

         pCursor = EnvelopeCursor();
      }
      break;
      case SBWidth:
         tip = XO("Click and drag to adjust frequency bandwidth.");
         pCursor = &*bandWidthCursor;
         break;
#endif
      default:
         wxASSERT(false);
      } // switch
      // Falls through the switch if there was no boundary found.
   }
}

UIHandlePtr BrushHandle::HitTest
(std::weak_ptr<BrushHandle> &holder,
 const TrackPanelMouseState &st, const AudacityProject *pProject,
 const std::shared_ptr<TrackView> &pTrackView,
 const std::shared_ptr<SpectralData> &mpData)
{
   // This handle is a little special because there may be some state to
   // preserve during movement before the click.
   auto old = holder.lock();
   bool oldUseSnap = true;
   if (old) {
      // It should not have started listening to timer events
      if( old->mTimerHandler ) {
         wxASSERT(false);
         // Handle this eventuality anyway, don't leave a dangling back-pointer
         // in the attached event handler.
         old->mTimerHandler.reset();
      }
      oldUseSnap = old->mUseSnap;
   }

   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto result = std::make_shared<BrushHandle>(
      pTrackView, oldUseSnap, TrackList::Get( *pProject ), st, viewInfo, mpData);

   result = AssignUIHandlePtr(holder, result);

   //Make sure we are within the selected track
   // Adjusting the selection edges can be turned off in
   // the preferences...
   auto pTrack = pTrackView->FindTrack();
   if (!pTrack->GetSelected() || !viewInfo.bAdjustSelectionEdges)
   {
      return result;
   }

   {
      const wxRect &rect = st.rect;
      wxInt64 leftSel = viewInfo.TimeToPosition(viewInfo.selectedRegion.t0(), rect.x);
      wxInt64 rightSel = viewInfo.TimeToPosition(viewInfo.selectedRegion.t1(), rect.x);
      // Something is wrong if right edge comes before left edge
      wxASSERT(!(rightSel < leftSel));
      static_cast<void>(leftSel); // Suppress unused variable warnings if not in debug-mode
      static_cast<void>(rightSel);
   }

   return result;
}

UIHandle::Result BrushHandle::NeedChangeHighlight
(const BrushHandle &oldState, const BrushHandle &newState)
{
   auto useSnap = oldState.mUseSnap;
   // This is guaranteed when constructing the NEW handle:
   wxASSERT( useSnap == newState.mUseSnap );
   if (!useSnap)
      return 0;

   auto &oldSnapState = oldState.mSnapStart;
   auto &newSnapState = newState.mSnapStart;
   if ( oldSnapState.Snapped() == newSnapState.Snapped() &&
        (!oldSnapState.Snapped() ||
         oldSnapState.outCoord == newSnapState.outCoord) )
      return 0;

   return RefreshCode::RefreshAll;
}

BrushHandle::BrushHandle
( const std::shared_ptr<TrackView> &pTrackView, bool useSnap,
  const TrackList &trackList,
  const TrackPanelMouseState &st, const ViewInfo &viewInfo,
  const std::shared_ptr<SpectralData> &mpSpectralData)
   : mpView{ pTrackView }
   , mpSpectralData(mpSpectralData)
   , mSnapManager{ std::make_shared<SnapManager>(
      *trackList.GetOwner(), trackList, viewInfo) }
{
   const wxMouseState &state = st.state;
   mRect = st.rect;

   auto time = std::max(0.0, viewInfo.PositionToTime(state.m_x, mRect.x));
   auto pTrack = pTrackView->FindTrack();
   mSnapStart = mSnapManager->Snap(pTrack.get(), time, false);
   if (mSnapStart.snappedPoint)
         mSnapStart.outCoord += mRect.x;
   else
      mSnapStart.outCoord = -1;

   mUseSnap = useSnap;
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
   const WaveTrack *const wt =
           static_cast<const WaveTrack*>(pTrack);
   auto &trackPanel = TrackPanel::Get( *pProject );
   auto &viewInfo = ViewInfo::Get( *pProject );

   int x = mAutoScrolling ? mMostRecentX : event.m_x;
   int y = mAutoScrolling ? mMostRecentY : event.m_y;
   mMostRecentX = x;
   mMostRecentY = y;

   // Convert the cursor position to freq. value & time_position
   wxInt64 posFreq = PositionToFrequency(wt, 0, mMostRecentY, mRect.y, mRect.height);
   const double posTime = viewInfo.PositionToTime(mMostRecentX, mRect.x);
//   std::cout << mMostRecentX << "____" << mMostRecentY << std::endl;
//   std::cout << posFreq << "____" << posTime << std::endl;
//   wxInt64 restoredX = viewInfo.TimeToPosition(posTime, mRect.x, 0);
//   wxInt64 restoredY = FrequencyToPosition(wt, posFreq, mRect.y, mRect.height);

   mpSpectralData->addFreqTimeData(posFreq, posTime);
   mpSpectralData->coordHistory.push_back(std::make_pair(x, y));
   return RefreshAll;
}

HitTestPreview BrushHandle::Preview
        (const TrackPanelMouseState &st, AudacityProject *pProject)
{
   TranslatableString tip;
   wxCursor *pCursor = EnvelopeCursor();
   return { tip, pCursor };
}

UIHandle::Result BrushHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *)
{
   using namespace RefreshCode;
   ProjectHistory::Get( *pProject ).PushState(
           /* i18n-hint: (verb) Audacity has just done a special kind of DELETE on
              the labeled audio regions */
           XO( "Selected area using Brush Tool" ),
           /* i18n-hint: (verb) Do a special kind of DELETE on labeled audio
              regions */
           XO( "Brush tool selection" ) );
   ProjectHistory::Get( *pProject ).ModifyState(false);

   return RefreshNone;
}

UIHandle::Result BrushHandle::Cancel(AudacityProject *pProject)
{
   mSelectionStateChanger.reset();
   mpSpectralData->freqTimePtsData.clear();
   mpSpectralData->coordHistory.clear();

   return RefreshCode::RefreshAll;
}

void BrushHandle::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      auto& dc = context.dc;
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
