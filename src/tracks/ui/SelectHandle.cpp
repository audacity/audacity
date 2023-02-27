/**********************************************************************

Audacity: A Digital Audio Editor

SelectHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "SelectHandle.h"

#include "Scrubbing.h"
#include "ChannelView.h"

#include "AColor.h"
#include "../../SpectrumAnalyst.h"
#include "LabelTrack.h"
#include "NumberScale.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../ProjectSettings.h"
#include "../../ProjectWindow.h"
#include "../../RefreshCode.h"
#include "../../SelectUtilities.h"
#include "SelectionState.h"
#include "SyncLock.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelAx.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "../../prefs/SpectrogramSettings.h"
#include "../../../images/Cursors.h"

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

bool SelectHandle::IsClicked() const
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
      const auto &settings = SpectrogramSettings::Get(*wt);
      float minFreq, maxFreq;
      SpectrogramBounds::Get(*wt).GetBounds(*wt, minFreq, maxFreq);
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

      const auto &settings = SpectrogramSettings::Get(*wt);
      float minFreq, maxFreq;
      SpectrogramBounds::Get(*wt)
         .GetBounds(*wt, minFreq, maxFreq);
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
   inline bool isSpectralSelectionView(const ChannelView *pChannelView) {
      return
        pChannelView &&
        pChannelView->IsSpectral() &&
        pChannelView->FindTrack() &&
        pChannelView->FindTrack()->TypeSwitch<bool>(
           [&](const WaveTrack &wt) {
              const auto &settings = SpectrogramSettings::Get(wt);
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

   SelectionBoundary ChooseBoundary(
      const ViewInfo &viewInfo,
      wxCoord xx, wxCoord yy, const ChannelView *pChannelView,
      const wxRect &rect,
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
         isSpectralSelectionView(pChannelView)) {
         // Spectral selection track is always wave
         auto pTrack = pChannelView->FindTrack();
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

UIHandlePtr SelectHandle::HitTest
(std::weak_ptr<SelectHandle> &holder,
 const TrackPanelMouseState &st, const AudacityProject *pProject,
 const std::shared_ptr<ChannelView> &pChannelView)
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
   auto result = std::make_shared<SelectHandle>(
      pChannelView, oldUseSnap, TrackList::Get(*pProject), st, viewInfo);

   result = AssignUIHandlePtr(holder, result);

   //Make sure we are within the selected track
   // Adjusting the selection edges can be turned off in
   // the preferences...
   auto pTrack = pChannelView->FindTrack();
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

UIHandle::Result SelectHandle::NeedChangeHighlight
(const SelectHandle &oldState, const SelectHandle &newState)
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

SelectHandle::SelectHandle(
   const std::shared_ptr<ChannelView> &pChannelView, bool useSnap,
   const TrackList &trackList,
   const TrackPanelMouseState &st, const ViewInfo &viewInfo
)  : mpView{ pChannelView }
   // Selection dragging can snap to play region boundaries
   , mSnapManager{ std::make_shared<SnapManager>(
      *trackList.GetOwner(), trackList, viewInfo, SnapPointArray{
         SnapPoint{ viewInfo.playRegion.GetLastActiveStart() },
         SnapPoint{ viewInfo.playRegion.GetLastActiveEnd() },
   } ) }
{
   const wxMouseState &state = st.state;
   mRect = st.rect;

   auto time = std::max(0.0, viewInfo.PositionToTime(state.m_x, mRect.x));
   auto pTrack = pChannelView->FindTrack();
   mSnapStart = mSnapManager->Snap(pTrack.get(), time, false);
   if (mSnapStart.snappedPoint)
         mSnapStart.outCoord += mRect.x;
   else
      mSnapStart.outCoord = -1;

   mUseSnap = useSnap;
}

SelectHandle::~SelectHandle()
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

void SelectHandle::Enter(bool, AudacityProject *project)
{
   SetUseSnap(true, project);
}

void SelectHandle::SetUseSnap(bool use, AudacityProject *project)
{
   mUseSnap = use;

   bool hasSnap = HasSnap();
   if (hasSnap)
      // Repaint to turn the snap lines on or off
      mChangeHighlight = RefreshCode::RefreshAll;

   if (IsClicked()) {
      // Readjust the moving selection end
      AssignSelection(
         ViewInfo::Get( *project ),
         mUseSnap ? mSnapEnd.outTime : mSnapEnd.timeSnappedTime,
         nullptr);
   }
}

bool SelectHandle::HasSnap() const
{
   return
      (IsClicked() ? mSnapEnd : mSnapStart).snappedPoint;
}

bool SelectHandle::HasEscape(AudacityProject *) const
{
   return HasSnap() && mUseSnap;
}

bool SelectHandle::Escape(AudacityProject *project)
{
   if (SelectHandle::HasEscape(project)) {
      SetUseSnap(false, project);
      return true;
   }
   return false;
}

UIHandle::Result SelectHandle::Click(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   /// This method gets called when we're handling selection
   /// and the mouse was just clicked.

   using namespace RefreshCode;

   const auto pView = mpView.lock();
   if ( !pView )
      return Cancelled;

   wxMouseEvent &event = evt.event;
   auto &trackList = TrackList::Get(*pProject);
   const auto sTrack = trackList.Lock(FindTrack());
   const auto pTrack = sTrack.get();
   const auto pLeader = *trackList.Find(pTrack);
   auto &trackPanel = TrackPanel::Get(*pProject);
   auto &viewInfo = ViewInfo::Get(*pProject);

   mMostRecentX = event.m_x;
   mMostRecentY = event.m_y;

   bool selectChange = (
      event.LeftDown() &&
      event.ControlDown() &&
      pTrack->TypeSwitch<bool>( [&](LabelTrack &){
         // We should reach this, only in default of other hits on glyphs or
         // text boxes.
         bool bShift = event.ShiftDown();
         bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
         SelectUtilities::DoListSelection(
            *pProject, *pTrack, bShift, true, !unsafe);
         return true;
       } )
   );
   if ( selectChange )
      // Do not start a drag
      return RefreshAll | Cancelled;
   
   auto &selectionState = SelectionState::Get( *pProject );
   if (event.LeftDClick() && !event.ShiftDown()) {
      // Deselect all other tracks and select this one.
      selectionState.SelectNone(trackList);

      if (pLeader)
         selectionState.SelectTrack(*pLeader, true, true);

      // Default behavior: select whole track
      SelectionState::SelectTrackLength(
         viewInfo, *pLeader, SyncLockState::Get(*pProject).IsSyncLocked());

      // Special case: if we're over a clip in a WaveTrack,
      // select just that clip
      pTrack->TypeSwitch( [&] ( WaveTrack &wt ) {
         auto time = viewInfo.PositionToTime(event.m_x, mRect.x);
         WaveClip *const selectedClip = wt.GetClipAtTime(time);
         if (selectedClip) {
            viewInfo.selectedRegion.setTimes(
               selectedClip->GetPlayStartTime(), selectedClip->GetPlayEndTime());
         }
      } );

      ProjectHistory::Get( *pProject ).ModifyState(false);

      // Do not start a drag
      return RefreshAll | Cancelled;
   }
   else if (!event.LeftDown())
      return Cancelled;

   mInitialSelection = viewInfo.selectedRegion;

   mSelectionStateChanger =
      std::make_shared<SelectionStateChanger>(selectionState, trackList);

   mSelectionBoundary = 0;

   bool bShiftDown = event.ShiftDown();
   bool bCtrlDown = event.ControlDown();

   mSelStart = mUseSnap ? mSnapStart.outTime : mSnapStart.timeSnappedTime;
   auto xx = viewInfo.TimeToPosition(mSelStart, mRect.x);

   // I. Shift-click adjusts an existing selection
   if (bShiftDown || bCtrlDown) {
      if (bShiftDown)
         selectionState.ChangeSelectionOnShiftClick(trackList, *pTrack);
      if( bCtrlDown ){
         //Commented out bIsSelected toggles, as in Track Control Panel.
         //bool bIsSelected = pTrack->GetSelected();
         //Actual bIsSelected will always add.
         bool bIsSelected = false;
         // Don't toggle away the last selected track.
         if (!bIsSelected || trackPanel.GetSelectedTrackCount() > 1)
            if (pLeader)
               selectionState.SelectTrack(*pLeader, !bIsSelected, true);
      }

      double value;
      // Shift-click, choose closest boundary
      SelectionBoundary boundary =
         ChooseBoundary(viewInfo, xx, event.m_y,
            pView.get(), mRect, false, false, &value);
      mSelectionBoundary = boundary;
      switch (boundary) {
         case SBLeft:
         case SBRight:
         {
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
            // If drag starts, change time selection only
            // (also exit frequency snapping)
            mFreqSelMode = FREQ_SEL_INVALID;
#endif
            mSelStartValid = true;
            mSelStart = value;
            mSnapStart = SnapResults{};
            AdjustSelection(pProject, viewInfo, event.m_x, mRect.x, pTrack);
            break;
         }
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         case SBBottom:
         case SBTop:
         {
            mFreqSelTrack = pTrack->SharedPointer<const WaveTrack>();
            mFreqSelPin = value;
            mFreqSelMode =
               (boundary == SBBottom)
               ? FREQ_SEL_BOTTOM_FREE : FREQ_SEL_TOP_FREE;

            // Drag frequency only, not time:
            mSelStartValid = false;
            AdjustFreqSelection(
               static_cast<WaveTrack*>(pTrack),
               viewInfo, event.m_y, mRect.y, mRect.height);
            break;
         }
         case SBCenter:
         {
            const auto wt = static_cast<const WaveTrack*>(pTrack);
            HandleCenterFrequencyClick(viewInfo, true, wt, value);
            break;
         }
#endif
         default:
            wxASSERT(false);
      };

      // For persistence of the selection change:
      ProjectHistory::Get( *pProject ).ModifyState(false);

      // Get timer events so we can auto-scroll
      Connect(pProject);

      // Full refresh since the label area may need to indicate
      // newly selected tracks.
      return RefreshAll;
   }

   // II. Unmodified click starts a NEW selection

   //Make sure you are within the selected track
   bool startNewSelection = true;
   if (pTrack && pTrack->GetSelected()) {
      // Adjusting selection edges can be turned off in the
      // preferences now
      if (viewInfo.bAdjustSelectionEdges) {
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         if (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER &&
            isSpectralSelectionView(pView.get())) {
            // This code is no longer reachable, but it had a place in the
            // spectral selection prototype.  It used to be that you could be
            // in a center-frequency-snapping mode that was not a mouse drag
            // but responded to mouse movements.  Click exited that and dragged
            // width instead.  PRL.

            // Ignore whether we are inside the time selection.
            // Exit center-snapping, start dragging the width.
            mFreqSelMode = FREQ_SEL_PINNED_CENTER;
            mFreqSelTrack = pTrack->SharedPointer<const WaveTrack>();
            mFreqSelPin = viewInfo.selectedRegion.fc();
            // Do not adjust time boundaries
            mSelStartValid = false;
            AdjustFreqSelection(
               static_cast<WaveTrack*>(pTrack),
               viewInfo, event.m_y, mRect.y, mRect.height);
            // For persistence of the selection change:
            ProjectHistory::Get( *pProject ).ModifyState(false);
            mSelectionBoundary = SBWidth;
            return RefreshNone;
         }
         else
#endif
         {
            // Not shift-down, choose boundary only within snapping
            double value;
            SelectionBoundary boundary =
               ChooseBoundary(viewInfo, xx, event.m_y,
                  pView.get(), mRect, true, true, &value);
            mSelectionBoundary = boundary;
            switch (boundary) {
            case SBNone:
               // startNewSelection remains true
               break;
            case SBLeft:
            case SBRight:
               startNewSelection = false;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
               // Disable frequency selection
               mFreqSelMode = FREQ_SEL_INVALID;
#endif
               mSelStartValid = true;
               mSelStart = value;
               mSnapStart = SnapResults{};
               break;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
            case SBBottom:
            case SBTop:
            case SBWidth:
               startNewSelection = false;
               // Disable time selection
               mSelStartValid = false;
               mFreqSelTrack = pTrack->SharedPointer<const WaveTrack>();
               mFreqSelPin = value;
               mFreqSelMode =
                  (boundary == SBWidth) ? FREQ_SEL_PINNED_CENTER :
                  (boundary == SBBottom) ? FREQ_SEL_BOTTOM_FREE :
                  FREQ_SEL_TOP_FREE;
               break;
            case SBCenter:
            {
               const auto wt = static_cast<const WaveTrack*>(pTrack);
               HandleCenterFrequencyClick(viewInfo, false, wt, value);
               startNewSelection = false;
               break;
            }
#endif
            default:
               wxASSERT(false);
            }
         }
      } // bAdjustSelectionEdges
   }

   // III. Common case for starting a NEW selection

   if (startNewSelection) {
      // If we didn't move a selection boundary, start a NEW selection
      selectionState.SelectNone(trackList);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      StartFreqSelection (viewInfo, event.m_y, mRect.y, mRect.height,
         pView.get());
#endif
      StartSelection(pProject);
      if (pLeader)
         selectionState.SelectTrack(*pLeader, true, true);
      TrackFocus::Get(*pProject).Set(pTrack);

      Connect(pProject);
      return RefreshAll;
   }
   else {
      Connect(pProject);
      return RefreshAll;
   }
}

UIHandle::Result SelectHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;

   const auto pView = mpView.lock();
   if ( !pView )
      return Cancelled;

   auto &viewInfo = ViewInfo::Get( *pProject );
   const wxMouseEvent &event = evt.event;

   int x = mAutoScrolling ? mMostRecentX : event.m_x;
   int y = mAutoScrolling ? mMostRecentY : event.m_y;
   mMostRecentX = x;
   mMostRecentY = y;

   /// AS: If we're dragging to adjust a selection (or actually,
   ///  if the screen is scrolling while you're selecting), we
   ///  handle it here.

   // Fuhggeddaboudit if we're not dragging and not autoscrolling.
   if (!event.Dragging() && !mAutoScrolling)
      return RefreshNone;

   if (event.CmdDown()) {
      // Ctrl-drag has no meaning, fuhggeddaboudit
      // JKC YES it has meaning.
      //return RefreshNone;
   }

   // Also fuhggeddaboudit if not in a track.
   auto pTrack = TrackList::Get( *pProject ).Lock( FindTrack() );
   if (!pTrack)
      return RefreshNone;

   // JKC: Logic to prevent a selection smaller than 5 pixels to
   // prevent accidental dragging when selecting.
   // (if user really wants a tiny selection, they should zoom in).
   // Can someone make this value of '5' configurable in
   // preferences?
   enum { minimumSizedSelection = 5 }; //measured in pixels

   // Might be dragging frequency bounds only, test
   if (mSelStartValid) {
      wxInt64 SelStart = viewInfo.TimeToPosition(mSelStart, mRect.x); //cvt time to pixels.
      // Abandon this drag if selecting < 5 pixels.
      if (wxLongLong(SelStart - x).Abs() < minimumSizedSelection)
         return RefreshNone;
   }

   if (evt.pCell) {
      if ( auto clickedTrack =
          static_cast<CommonTrackPanelCell*>(evt.pCell.get())->FindTrack() ) {
         // Handle which tracks are selected
         Track *sTrack = pTrack.get();
         Track *eTrack = clickedTrack.get();
         auto &trackList = TrackList::Get( *pProject );
         if ( sTrack && eTrack && !event.ControlDown() ) {
            auto &selectionState = SelectionState::Get( *pProject );
            selectionState.SelectRangeOfTracks( trackList, *sTrack, *eTrack );
         }

   #ifdef EXPERIMENTAL_SPECTRAL_EDITING
   #ifndef SPECTRAL_EDITING_ESC_KEY
         if (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER &&
             !viewInfo.selectedRegion.isPoint())
            MoveSnappingFreqSelection
            (pProject, viewInfo, y, mRect.y, mRect.height, pView.get());
         else
   #endif
            if ( TrackList::Get( *pProject ).Lock(mFreqSelTrack) == pTrack )
               AdjustFreqSelection(
                  static_cast<WaveTrack*>(pTrack.get()),
                  viewInfo, y, mRect.y, mRect.height);
   #endif
         
         AdjustSelection(pProject, viewInfo, x, mRect.x, clickedTrack.get());
      }
   }

   return RefreshNone

      // If scrubbing does not use the helper poller thread, then
      // don't refresh at every mouse event, because it slows down seek-scrub.
      // Instead, let OnTimer do it, which is often enough.
      // And even if scrubbing does use the thread, then skipping refresh does not
      // bring that advantage, but it is probably still a good idea anyway.

      // | UpdateSelection

   ;
}

HitTestPreview SelectHandle::Preview
(const TrackPanelMouseState &st, AudacityProject *pProject)
{
   if (!HasSnap() && !mUseSnap)
      // Moved out of snapping; revert to un-escaped state
      mUseSnap = true;

   const auto pView = mpView.lock();
   if ( !pView )
      return {};

   auto pTrack = FindTrack().lock();
   if (!pTrack)
      return {};

   TranslatableString tip;
   wxCursor *pCursor = SelectCursor();
   if ( IsClicked() )
      // Use same cursor as at the click
      SetTipAndCursorForBoundary
         (SelectionBoundary(mSelectionBoundary),
          (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER),
          tip, pCursor);
   else {
      // Choose one of many cursors for mouse-over

      auto &viewInfo = ViewInfo::Get( *pProject );

      auto &state = st.state;
      auto time = mUseSnap ? mSnapStart.outTime : mSnapStart.timeSnappedTime;
      auto xx = viewInfo.TimeToPosition(time, mRect.x);

      const bool bMultiToolMode =
         (ToolCodes::multiTool == ProjectSettings::Get( *pProject ).GetTool());

      //In Multi-tool mode, give multitool prompt if no-special-hit.
      if (bMultiToolMode) {
         // Look up the current key binding for Preferences.
         // (Don't assume it's the default!)
         auto keyStr =
            CommandManager::Get( *pProject ).GetKeyFromName(wxT("Preferences"))
            .Display( true );
         if (keyStr.empty())
            // No keyboard preference defined for opening Preferences dialog
            /* i18n-hint: These are the names of a menu and a command in that menu */
            keyStr = _("Edit, Preferences...");
         
         /* i18n-hint: %s is usually replaced by "Ctrl+P" for Windows/Linux, "Command+," for Mac */
         tip = XO("Multi-Tool Mode: %s for Mouse and Keyboard Preferences.")
            .Format( keyStr );
         // Later in this function we may point to some other string instead.
         if (!pTrack->GetSelected() ||
             !viewInfo.bAdjustSelectionEdges)
            ;
         else {
            const wxRect &rect = st.rect;
            const bool bShiftDown = state.ShiftDown();
            const bool bCtrlDown = state.ControlDown();
            const bool bModifierDown = bShiftDown || bCtrlDown;

            // If not shift-down and not snapping center, then
            // choose boundaries only in snapping tolerance,
            // and may choose center.
            SelectionBoundary boundary =
            ChooseBoundary(viewInfo, xx, state.m_y,
               pView.get(), rect, !bModifierDown, !bModifierDown);

            SetTipAndCursorForBoundary(boundary, !bShiftDown, tip, pCursor);
         }
      }

#if 0
      // This is a vestige of an idea in the prototype version.
      // Center would snap without mouse button down, click would pin the center
      // and drag width.
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      if ((mFreqSelMode == FREQ_SEL_SNAPPING_CENTER) &&
         isSpectralSelectionView(pView)) {
         // Not shift-down, but center frequency snapping toggle is on
         tip = XO("Click and drag to set frequency bandwidth.");
         pCursor = &*envelopeCursor;
         return {};
      }
#endif
#endif

      if (!pTrack->GetSelected() || !viewInfo.bAdjustSelectionEdges)
         ;
      else {
         const wxRect &rect = st.rect;
         const bool bShiftDown = state.ShiftDown();
         const bool bCtrlDown = state.ControlDown();
         const bool bModifierDown = bShiftDown || bCtrlDown;
         SelectionBoundary boundary = ChooseBoundary(
            viewInfo, xx, state.m_y,
               pView.get(), rect, !bModifierDown, !bModifierDown);
         SetTipAndCursorForBoundary(boundary, !bShiftDown, tip, pCursor);
      }
   }
   if (tip.empty()) {
      tip = XO("Click and drag to select audio");
   }
   if (HasEscape(pProject) && mUseSnap) {
      tip.Join(
/* i18n-hint: "Snapping" means automatic alignment of selection edges to any nearby label or clip boundaries */
        XO("(snapping)"), wxT(" ")
      );
   }
   return { tip, pCursor };
}

UIHandle::Result SelectHandle::Release
(const TrackPanelMouseEvent &, AudacityProject *pProject,
 wxWindow *)
{
   using namespace RefreshCode;
   ProjectHistory::Get( *pProject ).ModifyState(false);
   mFrequencySnapper.reset();
   mSnapManager.reset();
   if (mSelectionStateChanger) {
      mSelectionStateChanger->Commit();
      mSelectionStateChanger.reset();
   }
   
   if (mUseSnap && (mSnapStart.outCoord != -1 || mSnapEnd.outCoord != -1))
      return RefreshAll;
   else
      return RefreshNone;
}

UIHandle::Result SelectHandle::Cancel(AudacityProject *pProject)
{
   mSelectionStateChanger.reset();
   ViewInfo::Get( *pProject ).selectedRegion = mInitialSelection;

   return RefreshCode::RefreshAll;
}

void SelectHandle::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassSnapping ) {
      auto &dc = context.dc;
      // Draw snap guidelines if we have any
      if ( mSnapManager ) {
         auto coord1 = (mUseSnap || IsClicked()) ? mSnapStart.outCoord : -1;
         auto coord2 = (!mUseSnap || !IsClicked()) ? -1 : mSnapEnd.outCoord;
         mSnapManager->Draw( &dc, coord1, coord2 );
      }
   }
}

wxRect SelectHandle::DrawingArea(
   TrackPanelDrawingContext &,
   const wxRect &rect, const wxRect &panelRect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassSnapping )
      return MaximizeHeight( rect, panelRect );
   else
      return rect;
}

std::weak_ptr<Track> SelectHandle::FindTrack()
{
   auto pView = mpView.lock();
   if (!pView)
      return {};
   else
      return pView->FindTrack();
}

void SelectHandle::Connect(AudacityProject *pProject)
{
   mTimerHandler = std::make_shared<TimerHandler>( this, pProject );
}

class SelectHandle::TimerHandler
{
public:
   TimerHandler( SelectHandle *pParent, AudacityProject *pProject )
      : mParent{ pParent }
      , mConnectedProject{ pProject }
   {
      if (mConnectedProject)
         mSubscription = ProjectWindow::Get( *mConnectedProject )
            .GetPlaybackScroller().Subscribe( *this, &SelectHandle::TimerHandler::OnTimer);
   }

   // Receives timer event notifications, to implement auto-scroll
   void OnTimer(Observer::Message);

private:
   SelectHandle *mParent;
   AudacityProject *mConnectedProject;
   Observer::Subscription mSubscription;
};

void SelectHandle::TimerHandler::OnTimer(Observer::Message)
{
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
            ChannelView::Get(*pTrack->GetChannel(0)).shared_from_this() },
         project
      );
      mParent->mAutoScrolling = false;
      TrackPanel::Get( *mConnectedProject ).Refresh(false);
   }
}

/// Reset our selection markers.
void SelectHandle::StartSelection( AudacityProject *pProject )
{
   auto &viewInfo = ViewInfo::Get( *pProject );
   mSelStartValid = true;

   viewInfo.selectedRegion.setTimes(mSelStart, mSelStart);

   // PRL:  commented out the Sonify stuff with the TrackPanel refactor.
   // It was no-op anyway.
   //SonifyBeginModifyState();
   ProjectHistory::Get( *pProject ).ModifyState(false);
   //SonifyEndModifyState();
}

/// Extend or contract the existing selection
void SelectHandle::AdjustSelection
(AudacityProject *pProject,
 ViewInfo &viewInfo, int mouseXCoordinate, int trackLeftEdge,
 Track *track)
{
   if (!mSelStartValid)
      // Must be dragging frequency bounds only.
      return;

   double selend =
      std::max(0.0, viewInfo.PositionToTime(mouseXCoordinate, trackLeftEdge));
   double origSelend = selend;

   auto pTrack = Track::SharedPointer( track );
   if (!pTrack)
      pTrack = TrackList::Get( *pProject ).Lock( FindTrack() );

   if (pTrack && mSnapManager.get()) {
      bool rightEdge = (selend > mSelStart);
      mSnapEnd = mSnapManager->Snap(pTrack.get(), selend, rightEdge);
      if (mSnapEnd.Snapped()) {
         if (mUseSnap)
            selend = mSnapEnd.outTime;
         if (mSnapEnd.snappedPoint)
            mSnapEnd.outCoord += trackLeftEdge;
      }
      if (!mSnapEnd.snappedPoint)
         mSnapEnd.outCoord = -1;

      // Check if selection endpoints are too close together to snap (unless
      // using snap-to-time -- then we always accept the snap results)
      if (mSnapStart.outCoord >= 0 &&
          mSnapEnd.outCoord >= 0 &&
          std::abs(mSnapStart.outCoord - mSnapEnd.outCoord) < 3) {
         if(!mSnapEnd.snappedTime)
            selend = origSelend;
         mSnapEnd.outCoord = -1;
      }
   }
   AssignSelection(viewInfo, selend, pTrack.get());
}

void SelectHandle::AssignSelection
(ViewInfo &viewInfo, double selend, Track *pTrack)
{
   double sel0, sel1;
   if (mSelStart < selend) {
      sel0 = mSelStart;
      sel1 = selend;
   }
   else {
      sel1 = mSelStart;
      sel0 = selend;
   }

   viewInfo.selectedRegion.setTimes(sel0, sel1);
}

void SelectHandle::StartFreqSelection(ViewInfo &viewInfo,
   int mouseYCoordinate, int trackTopEdge,
   int trackHeight, ChannelView *pChannelView)
{
   mFreqSelTrack.reset();
   mFreqSelMode = FREQ_SEL_INVALID;
   mFreqSelPin = SelectedRegion::UndefinedFrequency;

   if (isSpectralSelectionView(pChannelView)) {
      // Spectral selection track is always wave
      auto shTrack = pChannelView->FindTrack()->SharedPointer<const WaveTrack>();
      mFreqSelTrack = shTrack;
      mFreqSelMode = FREQ_SEL_FREE;
      mFreqSelPin =
         PositionToFrequency(shTrack.get(), false, mouseYCoordinate,
         trackTopEdge, trackHeight);
      viewInfo.selectedRegion.setFrequencies(mFreqSelPin, mFreqSelPin);
   }
}

void SelectHandle::AdjustFreqSelection(
   const WaveTrack *wt, ViewInfo &viewInfo,
   int mouseYCoordinate, int trackTopEdge,
   int trackHeight)
{
   if (mFreqSelMode == FREQ_SEL_INVALID ||
       mFreqSelMode == FREQ_SEL_SNAPPING_CENTER)
      return;

   // Extension happens only when dragging in the same track in which we
   // started, and that is of a spectrogram display type.

   const double rate =  wt->GetRate();
   const double frequency =
      PositionToFrequency(wt, true, mouseYCoordinate,
         trackTopEdge, trackHeight);

   // Dragging center?
   if (mFreqSelMode == FREQ_SEL_DRAG_CENTER) {
      if (frequency == rate || frequency < 1.0)
         // snapped to top or bottom
         viewInfo.selectedRegion.setFrequencies(
            SelectedRegion::UndefinedFrequency,
            SelectedRegion::UndefinedFrequency);
      else {
         // mFreqSelPin holds the ratio of top to center
         const double maxRatio = findMaxRatio(frequency, rate);
         const double ratio = std::min(maxRatio, mFreqSelPin);
         viewInfo.selectedRegion.setFrequencies(
            frequency / ratio, frequency * ratio);
      }
   }
   else if (mFreqSelMode == FREQ_SEL_PINNED_CENTER) {
      if (mFreqSelPin >= 0) {
         // Change both upper and lower edges leaving centre where it is.
         if (frequency == rate || frequency < 1.0)
            // snapped to top or bottom
            viewInfo.selectedRegion.setFrequencies(
               SelectedRegion::UndefinedFrequency,
               SelectedRegion::UndefinedFrequency);
         else {
            // Given center and mouse position, find ratio of the larger to the
            // smaller, limit that to the frequency scale bounds, and adjust
            // top and bottom accordingly.
            const double maxRatio = findMaxRatio(mFreqSelPin, rate);
            double ratio = frequency / mFreqSelPin;
            if (ratio < 1.0)
               ratio = 1.0 / ratio;
            ratio = std::min(maxRatio, ratio);
            viewInfo.selectedRegion.setFrequencies(
               mFreqSelPin / ratio, mFreqSelPin * ratio);
         }
      }
   }
   else {
      // Dragging of upper or lower.
      const bool bottomDefined =
         !(mFreqSelMode == FREQ_SEL_TOP_FREE && mFreqSelPin < 0);
      const bool topDefined =
         !(mFreqSelMode == FREQ_SEL_BOTTOM_FREE && mFreqSelPin < 0);
      if (!bottomDefined || (topDefined && mFreqSelPin < frequency)) {
         // Adjust top
         if (frequency == rate)
            // snapped high; upper frequency is undefined
            viewInfo.selectedRegion.setF1(SelectedRegion::UndefinedFrequency);
         else
            viewInfo.selectedRegion.setF1(std::max(1.0, frequency));

         viewInfo.selectedRegion.setF0(mFreqSelPin);
      }
      else {
         // Adjust bottom
         if (frequency < 1.0)
            // snapped low; lower frequency is undefined
            viewInfo.selectedRegion.setF0(SelectedRegion::UndefinedFrequency);
         else
            viewInfo.selectedRegion.setF0(std::min(rate / 2.0, frequency));

         viewInfo.selectedRegion.setF1(mFreqSelPin);
      }
   }
}

void SelectHandle::HandleCenterFrequencyClick
(const ViewInfo &viewInfo, bool shiftDown, const WaveTrack *pTrack, double value)
{
   if (shiftDown) {
      // Disable time selection
      mSelStartValid = false;
      mFreqSelTrack = pTrack->SharedPointer<const WaveTrack>();
      mFreqSelPin = value;
      mFreqSelMode = FREQ_SEL_DRAG_CENTER;
   }
   else {
#ifndef SPECTRAL_EDITING_ESC_KEY
      // Start center snapping
      // Turn center snapping on (the only way to do this)
      mFreqSelMode = FREQ_SEL_SNAPPING_CENTER;
      // Disable time selection
      mSelStartValid = false;
      mFrequencySnapper = std::make_shared<SpectrumAnalyst>();
      StartSnappingFreqSelection(*mFrequencySnapper, viewInfo, pTrack);
#endif
   }
}

void SelectHandle::StartSnappingFreqSelection
   (SpectrumAnalyst &analyst,
    const ViewInfo &viewInfo, const WaveTrack *pTrack)
{
   static const size_t minLength = 8;

   const double rate = pTrack->GetRate();

   // Grab samples, just for this track, at these times
   std::vector<float> frequencySnappingData;
   const auto start =
      pTrack->TimeToLongSamples(viewInfo.selectedRegion.t0());
   const auto end =
      pTrack->TimeToLongSamples(viewInfo.selectedRegion.t1());
   const auto length =
      std::min(frequencySnappingData.max_size(),
         limitSampleBufferSize(10485760, // as in FreqWindow.cpp
            end - start));
   const auto effectiveLength = std::max(minLength, length);
   frequencySnappingData.resize(effectiveLength, 0.0f);
   pTrack->GetFloats(
      &frequencySnappingData[0],
      start, length, FillFormat::fillZero,
      // Don't try to cope with exceptions, just read zeroes instead.
      false);

   // Use same settings as are now used for spectrogram display,
   // except, shrink the window as needed so we get some answers

   const auto &settings = SpectrogramSettings::Get(*pTrack);
   auto windowSize = settings.GetFFTLength();

   while(windowSize > effectiveLength)
      windowSize >>= 1;
   const int windowType = settings.windowType;

   analyst.Calculate(
      SpectrumAnalyst::Spectrum, windowType, windowSize, rate,
      &frequencySnappingData[0], length);

   // We can now throw away the sample data but we keep the spectrum.
}

void SelectHandle::MoveSnappingFreqSelection(
   AudacityProject *pProject, ViewInfo &viewInfo, int mouseYCoordinate,
   int trackTopEdge,
   int trackHeight, ChannelView *pChannelView)
{
   auto pTrack = pChannelView->FindTrack().get();
   if (pTrack &&
      pTrack->GetSelected() &&
      isSpectralSelectionView(pChannelView)) {
      // Spectral selection track is always wave
      WaveTrack *const wt = static_cast<WaveTrack*>(pTrack);
      // PRL:
      // What would happen if center snapping selection began in one spectrogram track,
      // then continues inside another?  We do not then recalculate
      // the spectrum (as was done in StartSnappingFreqSelection)
      // but snap according to the peaks in the old track.

      // But if we always supply the original clicked track here that doesn't matter.
      const double rate = wt->GetRate();
      const double frequency =
         PositionToFrequency(wt, false, mouseYCoordinate,
         trackTopEdge, trackHeight);
      const double snappedFrequency =
         mFrequencySnapper->FindPeak(frequency, NULL);
      const double maxRatio = findMaxRatio(snappedFrequency, rate);
      double ratio = 2.0; // An arbitrary octave on each side, at most
      {
         const double f0 = viewInfo.selectedRegion.f0();
         const double f1 = viewInfo.selectedRegion.f1();
         if (f1 >= f0 && f0 >= 0)
            // Preserve already chosen ratio instead
            ratio = sqrt(f1 / f0);
      }
      ratio = std::min(ratio, maxRatio);

      mFreqSelPin = snappedFrequency;
      viewInfo.selectedRegion.setFrequencies(
         snappedFrequency / ratio, snappedFrequency * ratio);

      // A change here would affect what AdjustFreqSelection() does
      // in the prototype version where you switch from moving center to
      // dragging width with a click.  No effect now.
      mFreqSelTrack = wt->SharedPointer<const WaveTrack>();

      // SelectNone();
      // SelectTrack(pTrack, true);
      TrackFocus::Get( *pProject ).Set(pTrack);
   }
}

void SelectHandle::SnapCenterOnce
   (SpectrumAnalyst &analyst,
    ViewInfo &viewInfo, const WaveTrack *pTrack, bool up)
{
   const auto &settings = SpectrogramSettings::Get(*pTrack);
   const auto windowSize = settings.GetFFTLength();
   const double rate = pTrack->GetRate();
   const double nyq = rate / 2.0;
   const double binFrequency = rate / windowSize;

   double f1 = viewInfo.selectedRegion.f1();
   double centerFrequency = viewInfo.selectedRegion.fc();
   if (centerFrequency <= 0) {
      centerFrequency = up ? binFrequency : nyq;
      f1 = centerFrequency * sqrt(2.0);
   }

   double ratio = f1 / centerFrequency;
   const int originalBin = floor(0.5 + centerFrequency / binFrequency);
   const int limitingBin = up ? floor(0.5 + nyq / binFrequency) : 1;

   // This is crude and wasteful, doing the FFT each time the command is called.
   // It would be better to cache the data, but then invalidation of the cache would
   // need doing in all places that change the time selection.
   StartSnappingFreqSelection(analyst, viewInfo, pTrack);
   double snappedFrequency = centerFrequency;
   int bin = originalBin;
   if (up) {
      while (snappedFrequency <= centerFrequency &&
         bin < limitingBin)
         snappedFrequency = analyst.FindPeak(++bin * binFrequency, NULL);
   }
   else {
      while (snappedFrequency >= centerFrequency &&
         bin > limitingBin)
         snappedFrequency = analyst.FindPeak(--bin * binFrequency, NULL);
   }

   // PRL:  added these two lines with the big TrackPanel refactor
   const double maxRatio = findMaxRatio(snappedFrequency, rate);
   ratio = std::min(ratio, maxRatio);

   viewInfo.selectedRegion.setFrequencies
      (snappedFrequency / ratio, snappedFrequency * ratio);
}

#if 0
// unused
void SelectHandle::ResetFreqSelectionPin
   (const ViewInfo &viewInfo, double hintFrequency, bool logF)
{
   switch (mFreqSelMode) {
   case FREQ_SEL_INVALID:
   case FREQ_SEL_SNAPPING_CENTER:
      mFreqSelPin = -1.0;
      break;

   case FREQ_SEL_PINNED_CENTER:
      mFreqSelPin = viewInfo.selectedRegion.fc();
      break;

   case FREQ_SEL_DRAG_CENTER:
   {
      // Re-pin the width
      const double f0 = viewInfo.selectedRegion.f0();
      const double f1 = viewInfo.selectedRegion.f1();
      if (f0 >= 0 && f1 >= 0)
         mFreqSelPin = sqrt(f1 / f0);
      else
         mFreqSelPin = -1.0;
   }
   break;

   case FREQ_SEL_FREE:
      // Pin which?  Farther from the hint which is the presumed
      // mouse position.
   {
      // If this function finds use again, the following should be
      // generalized using NumberScale

      const double f0 = viewInfo.selectedRegion.f0();
      const double f1 = viewInfo.selectedRegion.f1();
      if (logF) {
         if (f1 < 0)
            mFreqSelPin = f0;
         else {
            const double logf1 = log(std::max(1.0, f1));
            const double logf0 = log(std::max(1.0, f0));
            const double logHint = log(std::max(1.0, hintFrequency));
            if (std::abs(logHint - logf1) < std::abs(logHint - logf0))
               mFreqSelPin = f0;
            else
               mFreqSelPin = f1;
         }
      }
      else {
         if (f1 < 0 ||
            std::abs(hintFrequency - f1) < std::abs(hintFrequency - f0))
            mFreqSelPin = f0;
         else
            mFreqSelPin = f1;
      }
   }
   break;

   case FREQ_SEL_TOP_FREE:
      mFreqSelPin = viewInfo.selectedRegion.f0();
      break;

   case FREQ_SEL_BOTTOM_FREE:
      mFreqSelPin = viewInfo.selectedRegion.f1();
      break;

   default:
      wxASSERT(false);
   }
}
#endif
