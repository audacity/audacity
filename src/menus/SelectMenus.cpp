#include "../Audacity.h"
#include "../Experimental.h"

#include "../AdornedRulerPanel.h"
#include "../AudioIO.h"
#include "../FreqWindow.h"
#include "../Menus.h" // for PrefsListener
#include "../Prefs.h"
#include "../Project.h"
#include "../TimeDialog.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../NoteTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ControlToolBar.h"
#include "../tracks/ui/SelectHandle.h"

// private helper classes and functions
namespace {

void DoSelectTimeAndTracks
(AudacityProject &project, bool bAllTime, bool bAllTracks)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( bAllTime )
      selectedRegion.setTimes(
         tracks->GetMinOffset(), tracks->GetEndTime());

   if( bAllTracks ) {
      for (auto t : tracks->Any()){
         t->SetSelected(true);
      }

      project.ModifyState(false);
      trackPanel->Refresh(false);
   }
}

// Temporal selection (not TimeTrack selection)
// potentially for all wave or not tracks.
void DoSelectTimeAndAudioTracks
(AudacityProject &project, bool bAllTime, bool bAllTracks)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if( bAllTime )
      selectedRegion.setTimes(
         tracks->GetMinOffset(), tracks->GetEndTime());

   if( bAllTracks ) {
      for (auto t : tracks->Any()){
         auto wt = dynamic_cast<WaveTrack *>(t);
         auto nt = dynamic_cast<NoteTrack *>(t);
         if( wt || nt )
            t->SetSelected(true);
      }

      project.ModifyState(false);
      trackPanel->Refresh(false);
   }
}

void DoNextPeakFrequency(AudacityProject &project, bool up)
{
   auto tracks = project.GetTracks();
   auto &viewInfo = project.GetViewInfo();
   auto trackPanel = project.GetTrackPanel();

   // Find the first selected wave track that is in a spectrogram view.
   const WaveTrack *pTrack {};
   for ( auto wt : tracks->Selected< const WaveTrack >() ) {
      const int display = wt->GetDisplay();
      if (display == WaveTrack::Spectrum) {
         pTrack = wt;
         break;
      }
   }

   if (pTrack) {
      SpectrumAnalyst analyst;
      SelectHandle::SnapCenterOnce(analyst, viewInfo, pTrack, up);
      trackPanel->Refresh(false);
      project.ModifyState(false);
   }
}

double NearestZeroCrossing
(AudacityProject &project, double t0)
{
   auto rate = project.GetRate();
   auto tracks = project.GetTracks();

   // Window is 1/100th of a second.
   auto windowSize = size_t(std::max(1.0, rate / 100));
   Floats dist{ windowSize, true };

   int nTracks = 0;
   for (auto one : tracks->Selected< const WaveTrack >()) {
      auto oneWindowSize = size_t(std::max(1.0, one->GetRate() / 100));
      Floats oneDist{ oneWindowSize };
      auto s = one->TimeToLongSamples(t0);
      // fillTwo to ensure that missing values are treated as 2, and hence do
      // not get used as zero crossings.
      one->Get((samplePtr)oneDist.get(), floatSample,
               s - (int)oneWindowSize/2, oneWindowSize, fillTwo);


      // Looking for actual crossings.
      double prev = 2.0;
      for(size_t i=0; i<oneWindowSize; i++){
         float fDist = fabs( oneDist[i]); // score is absolute value
         if( prev * oneDist[i] > 0 ) // both same sign?  No good.
            fDist = fDist + 0.4; // No good if same sign.
         else if( prev > 0.0 )
            fDist = fDist + 0.1; // medium penalty for downward crossing.
         prev = oneDist[i];
         oneDist[i] = fDist;
      }

      // TODO: The mixed rate zero crossing code is broken,
      // if oneWindowSize > windowSize we'll miss out some
      // samples - so they will still be zero, so we'll use them.
      for(size_t i = 0; i < windowSize; i++) {
         size_t j;
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
         // Apply a small penalty for distance from the original endpoint
         // We'll always prefer an upward  
         dist[i] +=
            0.1 * (abs(int(i) - int(windowSize/2))) / float(windowSize/2);
      }
      nTracks++;
   }

   // Find minimum
   int argmin = 0;
   float min = 3.0;
   for(size_t i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

   // If we're worse than 0.2 on average, on one track, then no good.
   if(( nTracks == 1 ) && ( min > (0.2*nTracks) ))
      return t0;
   // If we're worse than 0.6 on average, on multi-track, then no good.
   if(( nTracks > 1 ) && ( min > (0.6*nTracks) ))
      return t0;

   return t0 + (argmin - (int)windowSize/2) / rate;
}

// If this returns true, then there was a key up, and nothing more to do,
// after this function has completed.
// (at most this function just does a ModifyState for the keyup)
bool OnlyHandleKeyUp( const CommandContext &context )
{
   auto &project = context.project;
   auto evt = context.pEvt;
   bool bKeyUp = (evt) && evt->GetEventType() == wxEVT_KEY_UP;

   if( project.IsAudioActive() )
      return bKeyUp;
   if( !bKeyUp )
      return false;

   project.ModifyState(false);
   return true;
}

enum CursorDirection {
   DIRECTION_LEFT = -1,
   DIRECTION_RIGHT = +1
};

enum SelectionOperation {
    SELECTION_EXTEND,
    SELECTION_CONTRACT,
    CURSOR_MOVE
};

enum TimeUnit {
    TIME_UNIT_SECONDS,
    TIME_UNIT_PIXELS
};

struct SeekInfo
{
   wxLongLong mLastSelectionAdjustment { ::wxGetUTCTimeMillis() };
   double mSeekShort{ 0.0 };
   double mSeekLong{ 0.0 };
};

void SeekWhenAudioActive(double seekStep, wxLongLong &lastSelectionAdjustment)
{
#ifdef EXPERIMENTAL_IMPROVED_SEEKING
   if (gAudioIO->GetLastPlaybackTime() < lastSelectionAdjustment) {
      // Allow time for the last seek to output a buffer before
      // discarding samples again
      // Do not advance mLastSelectionAdjustment
      return;
   }
#endif
   lastSelectionAdjustment = ::wxGetUTCTimeMillis();

   gAudioIO->SeekStream(seekStep);
}

// Handles moving a selection edge with the keyboard in snap-to-time mode;
// returns the moved value.
// Will move at least minPix pixels -- set minPix positive to move forward,
// negative to move backward.
// Helper for moving by keyboard with snap-to-grid enabled
double GridMove
(AudacityProject &project, double t, int minPix)
{
   auto rate = project.GetRate();
   auto &viewInfo = project.GetViewInfo();
   auto format = project.GetSelectionFormat();

   NumericConverter nc(NumericConverter::TIME, format, t, rate);

   // Try incrementing/decrementing the value; if we've moved far enough we're
   // done
   double result;
   minPix >= 0 ? nc.Increment() : nc.Decrement();
   result = nc.GetValue();
   if (std::abs(viewInfo.TimeToPosition(result) - viewInfo.TimeToPosition(t))
       >= abs(minPix))
       return result;

   // Otherwise, move minPix pixels, then snap to the time.
   result = viewInfo.OffsetTimeByPixels(t, minPix);
   nc.SetValue(result);
   result = nc.GetValue();
   return result;
}

double OffsetTime
(AudacityProject &project,
 double t, double offset, TimeUnit timeUnit, int snapToTime)
{
   auto &viewInfo = project.GetViewInfo();

    if (timeUnit == TIME_UNIT_SECONDS)
        return t + offset; // snapping is currently ignored for non-pixel moves

    if (snapToTime == SNAP_OFF)
        return viewInfo.OffsetTimeByPixels(t, (int)offset);

    return GridMove(project, t, (int)offset);
}

// Moving a cursor, and collapsed selection.
void MoveWhenAudioInactive
(AudacityProject &project, double seekStep, TimeUnit timeUnit)
{
   auto &viewInfo = project.GetViewInfo();
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto ruler = project.GetRulerPanel();

   // If TIME_UNIT_SECONDS, snap-to will be off.
   int snapToTime = project.GetSnapTo();
   const double t0 = viewInfo.selectedRegion.t0();
   const double end = std::max( 
      tracks->GetEndTime(),
      trackPanel->GetScreenEndTime());

   // Move the cursor
   // Already in cursor mode?
   if( viewInfo.selectedRegion.isPoint() )
   {
      double newT = OffsetTime(project,
         t0, seekStep, timeUnit, snapToTime);
      // constrain.
      newT = std::max(0.0, newT);
      newT = std::min(newT, end);
      // Move 
      viewInfo.selectedRegion.setT0(
         newT,
         false); // do not swap selection boundaries
      viewInfo.selectedRegion.collapseToT0();

      // Move the visual cursor, avoiding an unnecessary complete redraw
      trackPanel->DrawOverlays(false);
      ruler->DrawOverlays(false);

      // This updates the selection shown on the selection bar, and the play
      // region
      project.TP_DisplaySelection();
   } else
   {
      // Transition to cursor mode.
      if( seekStep < 0 )
         viewInfo.selectedRegion.collapseToT0();
      else
         viewInfo.selectedRegion.collapseToT1();
      trackPanel->Refresh(false);
   }

   // Make sure NEW position is in view
   trackPanel->ScrollIntoView(viewInfo.selectedRegion.t1());
   return;
}

void SeekWhenAudioInactive
(AudacityProject &project, double seekStep, TimeUnit timeUnit,
SelectionOperation operation)
{
   auto &viewInfo = project.GetViewInfo();
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   if( operation == CURSOR_MOVE )
   {
      MoveWhenAudioInactive( project, seekStep, timeUnit);
      return;
   }

   int snapToTime = project.GetSnapTo();
   const double t0 = viewInfo.selectedRegion.t0();
   const double t1 = viewInfo.selectedRegion.t1();
   const double end = std::max( 
      tracks->GetEndTime(),
      trackPanel->GetScreenEndTime());

   // Is it t0 or t1 moving?
   bool bMoveT0 = (operation == SELECTION_CONTRACT && seekStep > 0) ||
	   (operation == SELECTION_EXTEND && seekStep < 0);
   // newT is where we want to move to
   double newT = OffsetTime( project,
      bMoveT0 ? t0 : t1, seekStep, timeUnit, snapToTime);
   // constrain to be in the track/screen limits.
   newT = std::max( 0.0, newT );
   newT = std::min( newT, end);
   // optionally constrain to be a contraction, i.e. so t0/t1 do not cross over
   if( operation == SELECTION_CONTRACT )
      newT = bMoveT0 ? std::min( t1, newT ) : std::max( t0, newT );

   // Actually move
   if( bMoveT0 )
      viewInfo.selectedRegion.setT0( newT );
   else 
      viewInfo.selectedRegion.setT1( newT );

   // Ensure it is visible, and refresh.
   trackPanel->ScrollIntoView(newT);
   trackPanel->Refresh(false);
}

// Handle small cursor and play head movements
void SeekLeftOrRight
(AudacityProject &project, double direction, SelectionOperation operation,
 SeekInfo &info)
{
   // PRL:  What I found and preserved, strange though it be:
   // During playback:  jump depends on preferences and is independent of the
   // zoom and does not vary if the key is held
   // Else: jump depends on the zoom and gets bigger if the key is held

   if( project.IsAudioActive() )
   {
      if( operation == CURSOR_MOVE )
         SeekWhenAudioActive( info.mSeekShort * direction,
            info.mLastSelectionAdjustment );
      else if( operation == SELECTION_EXTEND )
         SeekWhenAudioActive( info.mSeekLong * direction,
            info.mLastSelectionAdjustment );
      // Note: no action for CURSOR_CONTRACT
      return;
   }

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   const wxLongLong curtime = ::wxGetUTCTimeMillis();
   enum { MIN_INTERVAL = 50 };
   const bool fast =
      (curtime - info.mLastSelectionAdjustment < MIN_INTERVAL);

   info.mLastSelectionAdjustment = curtime;

   // How much faster should the cursor move if shift is down?
   enum { LARGER_MULTIPLIER = 4 };
   const double seekStep = (fast ? LARGER_MULTIPLIER : 1.0) * direction;

   SeekWhenAudioInactive( project, seekStep, TIME_UNIT_PIXELS, operation);
}

// Move the cursor forward or backward, while paused or while playing.
void DoCursorMove(
   AudacityProject &project, double seekStep,
   wxLongLong &lastSelectionAdjustment)
{
   if (project.IsAudioActive()) {
      SeekWhenAudioActive(seekStep, lastSelectionAdjustment);
   }
   else
   {
      lastSelectionAdjustment = ::wxGetUTCTimeMillis();
      MoveWhenAudioInactive(project, seekStep, TIME_UNIT_SECONDS);
   }

   project.ModifyState(false);
}

void DoBoundaryMove(AudacityProject &project, int step, SeekInfo &info)
{
   auto &viewInfo = project.GetViewInfo();
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   // step is negative, then is moving left.  step positive, moving right.
   // Move the left/right selection boundary, to expand the selection

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetUTCTimeMillis();
   int pixels = step;
   if( curtime - info.mLastSelectionAdjustment < 50 )
   {
      pixels *= 4;
   }
   info.mLastSelectionAdjustment = curtime;

   // we used to have a parameter boundaryContract to say if expanding or
   // contracting.  it is no longer needed.
   bool bMoveT0 = (step < 0 );// ^ boundaryContract ;

   if( project.IsAudioActive() )
   {
      double indicator = gAudioIO->GetStreamTime();
      if( bMoveT0 )
         viewInfo.selectedRegion.setT0(indicator, false);
      else
         viewInfo.selectedRegion.setT1(indicator);

      project.ModifyState(false);
      trackPanel->Refresh(false);
      return;
   }

   const double t0 = viewInfo.selectedRegion.t0();
   const double t1 = viewInfo.selectedRegion.t1();
   const double end = std::max( 
      tracks->GetEndTime(),
      trackPanel->GetScreenEndTime());

   double newT = viewInfo.OffsetTimeByPixels( bMoveT0 ? t0 : t1, pixels);
   // constrain to be in the track/screen limits.
   newT = std::max( 0.0, newT );
   newT = std::min( newT, end);
   // optionally constrain to be a contraction, i.e. so t0/t1 do not cross over
   //if( boundaryContract )
   //   newT = bMoveT0 ? std::min( t1, newT ) : std::max( t0, newT );

   // Actually move
   if( bMoveT0 )
      viewInfo.selectedRegion.setT0( newT );
   else 
      viewInfo.selectedRegion.setT1( newT );

   // Ensure it is visible, and refresh.
   trackPanel->ScrollIntoView(newT);
   trackPanel->Refresh(false);

   project.ModifyState(false);
}

}

namespace SelectActions {

// exported helper functions

void DoListSelection
(AudacityProject &project, Track *t, bool shift, bool ctrl, bool modifyState)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectionState = project.GetSelectionState();
   auto &viewInfo = project.GetViewInfo();
   auto isSyncLocked = project.IsSyncLocked();

   selectionState.HandleListSelection
      ( *tracks, viewInfo, *t,
        shift, ctrl, isSyncLocked );

   if (! ctrl )
      trackPanel->SetFocusedTrack(t);
   project.Refresh(false);
   if (modifyState)
      project.ModifyState(true);
}

void DoSelectAll(AudacityProject &project)
{
   DoSelectTimeAndTracks( project, true, true );
}

void DoSelectAllAudio(AudacityProject &project)
{
   DoSelectTimeAndAudioTracks( project, true, true );
}

// This function selects all tracks if no tracks selected,
// and all time if no time selected.
// There is an argument for making it just count wave tracks,
// However you could then not select a label and cut it,
// without this function selecting all tracks.
void DoSelectSomething(AudacityProject &project)
{
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   bool bTime = selectedRegion.isPoint();
   bool bTracks = tracks->Selected().empty();

   if( bTime || bTracks )
      DoSelectTimeAndTracks( project, bTime, bTracks );
}

// Menu handler functions

struct Handler
   : CommandHandlerObject // MUST be the first base class!
   , PrefsListener
{

void OnSelectAll(const CommandContext &context)
{
   DoSelectAll( context.project );
}

void OnSelectNone(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   selectedRegion.collapseToT0();
   project.SelectNone();
   project.ModifyState(false);
}

void OnSelectAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectTimeAndTracks( project, false, true );
}

void OnSelectSyncLockSel(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();

   bool selected = false;
   for (auto t : tracks->Any()
         + &Track::IsSyncLockSelected - &Track::IsSelected) {
      t->SetSelected(true);
      selected = true;
   }

   if (selected)
      project.ModifyState(false);

   trackPanel->Refresh(false);
}

//this pops up a dialog which allows the left selection to be set.
//If playing/recording is happening, it sets the left selection at
//the current play position.
void OnSetLeftSelection(const CommandContext &context)
{
   auto &project = context.project;
   auto token = project.GetAudioIOToken();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto trackPanel = project.GetTrackPanel();

   bool bSelChanged = false;
   if ((token > 0) && gAudioIO->IsStreamActive(token))
   {
      double indicator = gAudioIO->GetStreamTime();
      selectedRegion.setT0(indicator, false);
      bSelChanged = true;
   }
   else
   {
      auto fmt = project.GetSelectionFormat();
      auto rate = project.GetRate();

      TimeDialog dlg(&project, _("Set Left Selection Boundary"),
         fmt, rate, selectedRegion.t0(), _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         selectedRegion.setT0(
            std::max(0.0, dlg.GetTimeValue()), false);
         bSelChanged = true;
      }
   }

   if (bSelChanged)
   {
      project.ModifyState(false);
      trackPanel->Refresh(false);
   }
}

void OnSetRightSelection(const CommandContext &context)
{
   auto &project = context.project;
   auto token = project.GetAudioIOToken();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto trackPanel = project.GetTrackPanel();

   bool bSelChanged = false;
   if ((token > 0) && gAudioIO->IsStreamActive(token))
   {
      double indicator = gAudioIO->GetStreamTime();
      selectedRegion.setT1(indicator, false);
      bSelChanged = true;
   }
   else
   {
      auto fmt = project.GetSelectionFormat();
      auto rate = project.GetRate();

      TimeDialog dlg(&project, _("Set Right Selection Boundary"),
         fmt, rate, selectedRegion.t1(), _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         selectedRegion.setT1(
            std::max(0.0, dlg.GetTimeValue()), false);
         bSelChanged = true;
      }
   }

   if (bSelChanged)
   {
      project.ModifyState(false);
      trackPanel->Refresh(false);
   }
}

void OnSelectStartCursor(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   double kWayOverToRight = std::numeric_limits<double>::max();

   auto range = tracks->Selected();
   if ( ! range )
      return;

   double minOffset = range.min( &Track::GetStartTime );

   if( minOffset >=
       (kWayOverToRight * (1 - std::numeric_limits<double>::epsilon()) ))
      return;

   selectedRegion.setT0(minOffset);

   project.ModifyState(false);

   trackPanel->Refresh(false);
}

void OnSelectCursorEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   double kWayOverToLeft = std::numeric_limits<double>::lowest();

   auto range = tracks->Selected();
   if ( ! range )
      return;

   double maxEndOffset = range.max( &Track::GetEndTime );

   if( maxEndOffset <=
       (kWayOverToLeft * (1 - std::numeric_limits<double>::epsilon()) ))
      return;

   selectedRegion.setT1(maxEndOffset);

   project.ModifyState(false);

   trackPanel->Refresh(false);
}

void OnSelectTrackStartToEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = project.GetViewInfo();
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();

   auto range = tracks->Selected();
   double maxEndOffset = range.max( &Track::GetEndTime );
   double minOffset = range.min( &Track::GetStartTime );

   if( maxEndOffset < minOffset)
      return;

   viewInfo.selectedRegion.setTimes( minOffset, maxEndOffset );
   project.ModifyState(false);

   trackPanel->Refresh(false);
}

// Handler state:
SelectedRegion mRegionSave{};

void OnSelectionSave(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   mRegionSave = selectedRegion;
}

void OnSelectionRestore(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto trackPanel = project.GetTrackPanel();

   if ((mRegionSave.t0() == 0.0) &&
       (mRegionSave.t1() == 0.0))
      return;

   selectedRegion = mRegionSave;

   project.ModifyState(false);

   trackPanel->Refresh(false);
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING

// Handler state:
double mLastF0{ SelectedRegion::UndefinedFrequency };
double mLastF1{ SelectedRegion::UndefinedFrequency };

void OnToggleSpectralSelection(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   const double f0 = selectedRegion.f0();
   const double f1 = selectedRegion.f1();
   const bool haveSpectralSelection =
   !(f0 == SelectedRegion::UndefinedFrequency &&
     f1 == SelectedRegion::UndefinedFrequency);
   if (haveSpectralSelection)
   {
      mLastF0 = f0;
      mLastF1 = f1;
      selectedRegion.setFrequencies
      (SelectedRegion::UndefinedFrequency, SelectedRegion::UndefinedFrequency);
   }
   else
      selectedRegion.setFrequencies(mLastF0, mLastF1);

   trackPanel->Refresh(false);
   project.ModifyState(false);
}

void OnNextHigherPeakFrequency(const CommandContext &context)
{
   auto &project = context.project;
   DoNextPeakFrequency(project, true);
}

void OnNextLowerPeakFrequency(const CommandContext &context)
{
   auto &project = context.project;
   DoNextPeakFrequency(project, false);
}
#endif

// Handler state:
bool mCursorPositionHasBeenStored{ false };
double mCursorPositionStored{ 0.0 };

void OnSelectCursorStoredCursor(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto isAudioActive = project.IsAudioActive();

   if (mCursorPositionHasBeenStored) {
      double cursorPositionCurrent = isAudioActive
         ? gAudioIO->GetStreamTime()
         : selectedRegion.t0();
      selectedRegion.setTimes(
         std::min(cursorPositionCurrent, mCursorPositionStored),
         std::max(cursorPositionCurrent, mCursorPositionStored));

      project.ModifyState(false);
      trackPanel->Refresh(false);
   }
}

void OnCursorPositionStore(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto isAudioActive = project.IsAudioActive();

   mCursorPositionStored =
      isAudioActive ? gAudioIO->GetStreamTime() : selectedRegion.t0();
   mCursorPositionHasBeenStored = true;
}

void OnZeroCrossing(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto trackPanel = project.GetTrackPanel();

   const double t0 = NearestZeroCrossing(project, selectedRegion.t0());
   if (selectedRegion.isPoint())
      selectedRegion.setTimes(t0, t0);
   else {
      const double t1 = NearestZeroCrossing(project, selectedRegion.t1());
      // Empty selection is generally not much use, so do not make it if empty.
      if( fabs( t1 - t0 ) * project.GetRate() > 1.5 )
         selectedRegion.setTimes(t0, t1);
   }

   project.ModifyState(false);

   trackPanel->Refresh(false);
}

void OnSnapToOff(const CommandContext &context)
{
   auto &project = context.project;
   project.SetSnapTo(SNAP_OFF);
}

void OnSnapToNearest(const CommandContext &context)
{
   auto &project = context.project;
   project.SetSnapTo(SNAP_NEAREST);
}

void OnSnapToPrior(const CommandContext &context)
{
   auto &project = context.project;
   project.SetSnapTo(SNAP_PRIOR);
}

void OnSelToStart(const CommandContext &context)
{
   auto &project = context.project;
   project.Rewind(true);
   project.ModifyState(false);
}

void OnSelToEnd(const CommandContext &context)
{
   auto &project = context.project;
   project.SkipEnd(true);
   project.ModifyState(false);
}

// Handler state:
SeekInfo mSeekInfo;

void OnSelExtendLeft(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_LEFT, SELECTION_EXTEND,
         mSeekInfo );
}

void OnSelExtendRight(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_RIGHT, SELECTION_EXTEND,
         mSeekInfo );
}

void OnSelSetExtendLeft(const CommandContext &context)
{
   DoBoundaryMove( context.project, DIRECTION_LEFT, mSeekInfo);
}

void OnSelSetExtendRight(const CommandContext &context)
{
   DoBoundaryMove( context.project, DIRECTION_RIGHT, mSeekInfo);
}

void OnSelContractLeft(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_RIGHT, SELECTION_CONTRACT,
         mSeekInfo );
}

void OnSelContractRight(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_LEFT, SELECTION_CONTRACT,
         mSeekInfo );
}

void OnCursorSelStart(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   selectedRegion.collapseToT0();
   project.ModifyState(false);
   trackPanel->ScrollIntoView(selectedRegion.t0());
   trackPanel->Refresh(false);
}

void OnCursorSelEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   selectedRegion.collapseToT1();
   project.ModifyState(false);
   trackPanel->ScrollIntoView(selectedRegion.t1());
   trackPanel->Refresh(false);
}

void OnCursorTrackStart(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   double kWayOverToRight = std::numeric_limits<double>::max();

   auto trackRange = tracks->Selected();
   if (trackRange.empty())
      // This should have been prevented by command manager
      return;

   // Range is surely nonempty now
   auto minOffset = std::max( 0.0, trackRange.min( &Track::GetOffset ) );

   if( minOffset >=
       (kWayOverToRight * (1 - std::numeric_limits<double>::epsilon()) ))
      return;

   selectedRegion.setTimes(minOffset, minOffset);
   project.ModifyState(false);
   trackPanel->ScrollIntoView(selectedRegion.t0());
   trackPanel->Refresh(false);
}

void OnCursorTrackEnd(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   double kWayOverToLeft = std::numeric_limits<double>::lowest();

   auto trackRange = tracks->Selected();
   if (trackRange.empty())
      // This should have been prevented by command manager
      return;

   // Range is surely nonempty now
   auto maxEndOffset = trackRange.max( &Track::GetEndTime );

   if( maxEndOffset <
       (kWayOverToLeft * (1 - std::numeric_limits<double>::epsilon()) ))
      return;

   selectedRegion.setTimes(maxEndOffset, maxEndOffset);
   project.ModifyState(false);
   trackPanel->ScrollIntoView(selectedRegion.t1());
   trackPanel->Refresh(false);
}

void OnSkipStart(const CommandContext &context)
{
   auto &project = context.project;
   wxCommandEvent evt;

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnRewind(evt);
   project.ModifyState(false);
}

void OnSkipEnd(const CommandContext &context)
{
   auto &project = context.project;
   wxCommandEvent evt;

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnFF(evt);
   project.ModifyState(false);
}

void OnCursorLeft(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_LEFT, CURSOR_MOVE,
         mSeekInfo );
}

void OnCursorRight(const CommandContext &context)
{
   if( !OnlyHandleKeyUp( context ) )
      SeekLeftOrRight( context.project, DIRECTION_RIGHT, CURSOR_MOVE,
         mSeekInfo );
}

void OnCursorShortJumpLeft(const CommandContext &context)
{
   DoCursorMove( context.project,
      -mSeekInfo.mSeekShort, mSeekInfo.mLastSelectionAdjustment );
}

void OnCursorShortJumpRight(const CommandContext &context)
{
   DoCursorMove( context.project,
      mSeekInfo.mSeekShort, mSeekInfo.mLastSelectionAdjustment );
}

void OnCursorLongJumpLeft(const CommandContext &context)
{
   DoCursorMove( context.project,
      -mSeekInfo.mSeekLong, mSeekInfo.mLastSelectionAdjustment );
}

void OnCursorLongJumpRight(const CommandContext &context)
{
   DoCursorMove( context.project,
      mSeekInfo.mSeekLong, mSeekInfo.mLastSelectionAdjustment );
}

void OnSeekLeftShort(const CommandContext &context)
{
   auto &project = context.project;
   SeekLeftOrRight( project, DIRECTION_LEFT, CURSOR_MOVE, mSeekInfo );
}

void OnSeekRightShort(const CommandContext &context)
{
   auto &project = context.project;
   SeekLeftOrRight( project, DIRECTION_RIGHT, CURSOR_MOVE, mSeekInfo );
}

void OnSeekLeftLong(const CommandContext &context)
{
   auto &project = context.project;
   SeekLeftOrRight( project, DIRECTION_LEFT, SELECTION_EXTEND, mSeekInfo );
}

void OnSeekRightLong(const CommandContext &context)
{
   auto &project = context.project;
   SeekLeftOrRight( project, DIRECTION_RIGHT, SELECTION_EXTEND, mSeekInfo );
}

#if 1
// Legacy functions, not used as of version 2.3.0
void OnSelectAllTime(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectTimeAndTracks( project, true, false );
}
#endif

void UpdatePrefs() override
{
   gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &mSeekInfo.mSeekShort, 1.0);
   gPrefs->Read(wxT("/AudioIO/SeekLongPeriod"), &mSeekInfo.mSeekLong, 15.0);
}
Handler()
{
   UpdatePrefs();
}

}; // struct Handler

} // namespace

// Handler is stateful.  Needs a factory registered with
// AudacityProject.
static const AudacityProject::RegisteredAttachedObjectFactory factory{ []{
   return std::make_unique< SelectActions::Handler >();
} };
static CommandHandlerObject &findCommandHandler(AudacityProject &project) {
   return static_cast<SelectActions::Handler&>(
      project.GetAttachedObject(factory));
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& SelectActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr ClipSelectMenu( AudacityProject& );

MenuTable::BaseItemPtr SelectMenu( AudacityProject& )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;
   
   /* i18n-hint: (verb) It's an item on a menu. */
   return Menu( _("&Select"),
      Command( wxT("SelectAll"), XXO("&All"), FN(OnSelectAll),
         TracksExistFlag,
         Options{ wxT("Ctrl+A"), _("Select All") } ),
      Command( wxT("SelectNone"), XXO("&None"), FN(OnSelectNone),
         TracksExistFlag,
         Options{ wxT("Ctrl+Shift+A"), _("Select None") } ),

      //////////////////////////////////////////////////////////////////////////

      Menu( _("&Tracks"),
         Command( wxT("SelAllTracks"), XXO("In All &Tracks"),
            FN(OnSelectAllTracks),
            TracksExistFlag,
            wxT("Ctrl+Shift+K") )

#ifdef EXPERIMENTAL_SYNC_LOCK
         ,
         Command( wxT("SelSyncLockTracks"), XXO("In All &Sync-Locked Tracks"),
            FN(OnSelectSyncLockSel),
            TracksSelectedFlag | IsSyncLockedFlag,
            Options{ wxT("Ctrl+Shift+Y"), _("Select Sync-Locked") } )
#endif
      ),

      //////////////////////////////////////////////////////////////////////////

      Menu( _("R&egion"),
         Command( wxT("SetLeftSelection"), XXO("&Left at Playback Position"),
            FN(OnSetLeftSelection), TracksExistFlag,
            Options{ wxT("["), _("Set Selection Left at Play Position") } ),
         Command( wxT("SetRightSelection"), XXO("&Right at Playback Position"),
            FN(OnSetRightSelection), TracksExistFlag,
            Options{ wxT("]"), _("Set Selection Right at Play Position") } ),
         Command( wxT("SelTrackStartToCursor"), XXO("Track &Start to Cursor"),
            FN(OnSelectStartCursor), AlwaysEnabledFlag,
            Options{ wxT("Shift+J"), _("Select Track Start to Cursor") } ),
         Command( wxT("SelCursorToTrackEnd"), XXO("Cursor to Track &End"),
            FN(OnSelectCursorEnd), AlwaysEnabledFlag,
            Options{ wxT("Shift+K"), _("Select Cursor to Track End") } ),
         Command( wxT("SelTrackStartToEnd"), XXO("Track Start to En&d"),
            FN(OnSelectTrackStartToEnd), AlwaysEnabledFlag,
            Options{}.LongName( _("Select Track Start to End") ) ),

         Separator(),

         // GA: Audacity had 'Store Re&gion' here previously. There is no
         // one-step way to restore the 'Saved Cursor Position' in Select Menu,
         // so arguably using the word 'Selection' to do duty for both saving
         // the region or the cursor is better. But it does not belong in a
         // 'Region' submenu.
         Command( wxT("SelSave"), XXO("S&tore Selection"), FN(OnSelectionSave),
            WaveTracksSelectedFlag ),
         // Audacity had 'Retrieve Regio&n' here previously.
         Command( wxT("SelRestore"), XXO("Retrieve Selectio&n"),
            FN(OnSelectionRestore), TracksExistFlag )
      ),

      //////////////////////////////////////////////////////////////////////////

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      Menu( _("S&pectral"),
         Command( wxT("ToggleSpectralSelection"),
            XXO("To&ggle Spectral Selection"), FN(OnToggleSpectralSelection),
            TracksExistFlag, wxT("Q") ),
         Command( wxT("NextHigherPeakFrequency"),
            XXO("Next &Higher Peak Frequency"), FN(OnNextHigherPeakFrequency),
            TracksExistFlag ),
         Command( wxT("NextLowerPeakFrequency"),
            XXO("Next &Lower Peak Frequency"), FN(OnNextLowerPeakFrequency),
            TracksExistFlag )
      ),
#endif

      //////////////////////////////////////////////////////////////////////////

      ClipSelectMenu,

      //////////////////////////////////////////////////////////////////////////

      Separator(),

      Command( wxT("SelCursorStoredCursor"),
         XXO("Cursor to Stored &Cursor Position"),
         FN(OnSelectCursorStoredCursor), TracksExistFlag,
         Options{}.LongName( _("Select Cursor to Stored") ) ),

      Command( wxT("StoreCursorPosition"), XXO("Store Cursor Pos&ition"),
         FN(OnCursorPositionStore),
         WaveTracksExistFlag ),
      // Save cursor position is used in some selections.
      // Maybe there should be a restore for it?

      Separator(),

      Command( wxT("ZeroCross"), XXO("At &Zero Crossings"),
         FN(OnZeroCrossing), TracksSelectedFlag,
         Options{ wxT("Z"), _("Select Zero Crossing") } )
   );
}

MenuTable::BaseItemPtr ExtraSelectionMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("&Selection"),
      Command( wxT("SnapToOff"), XXO("Snap-To &Off"), FN(OnSnapToOff),
         AlwaysEnabledFlag ),
      Command( wxT("SnapToNearest"), XXO("Snap-To &Nearest"),
         FN(OnSnapToNearest), AlwaysEnabledFlag ),
      Command( wxT("SnapToPrior"), XXO("Snap-To &Prior"), FN(OnSnapToPrior),
         AlwaysEnabledFlag ),
      Command( wxT("SelStart"), XXO("Selection to &Start"), FN(OnSelToStart),
         AlwaysEnabledFlag, wxT("Shift+Home") ),
      Command( wxT("SelEnd"), XXO("Selection to En&d"), FN(OnSelToEnd),
         AlwaysEnabledFlag, wxT("Shift+End") ),
      Command( wxT("SelExtLeft"), XXO("Selection Extend &Left"),
         FN(OnSelExtendLeft),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Shift+Left\twantKeyup\tallowDup") ),
      Command( wxT("SelExtRight"), XXO("Selection Extend &Right"),
         FN(OnSelExtendRight),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Shift+Right\twantKeyup\tallowDup") ),
      Command( wxT("SelSetExtLeft"), XXO("Set (or Extend) Le&ft Selection"),
         FN(OnSelSetExtendLeft),
         TracksExistFlag | TrackPanelHasFocus ),
      Command( wxT("SelSetExtRight"), XXO("Set (or Extend) Rig&ht Selection"),
         FN(OnSelSetExtendRight),
         TracksExistFlag | TrackPanelHasFocus ),
      Command( wxT("SelCntrLeft"), XXO("Selection Contract L&eft"),
         FN(OnSelContractLeft),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Ctrl+Shift+Right\twantKeyup") ),
      Command( wxT("SelCntrRight"), XXO("Selection Contract R&ight"),
         FN(OnSelContractRight),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Ctrl+Shift+Left\twantKeyup") )
   );
}

MenuTable::BaseItemPtr ClipCursorItems( AudacityProject & );

MenuTable::BaseItemPtr CursorMenu( AudacityProject & )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;
   constexpr auto CanStopFlags = AudioIONotBusyFlag | CanStopAudioStreamFlag;

   // JKC: ANSWER-ME: How is 'cursor to' different to 'Skip To' and how is it
   // useful?
   // GA: 'Skip to' moves the viewpoint to center of the track and preserves the
   // selection. 'Cursor to' does neither. 'Center at' might describe it better
   // than 'Skip'.
   return Menu( _("&Cursor to"),
      Command( wxT("CursSelStart"), XXO("Selection Star&t"),
         FN(OnCursorSelStart),
         TimeSelectedFlag,
         Options{}.LongName( _("Cursor to Selection Start") ) ),
      Command( wxT("CursSelEnd"), XXO("Selection En&d"),
         FN(OnCursorSelEnd),
         TimeSelectedFlag,
         Options{}.LongName( _("Cursor to Selection End") ) ),

      Command( wxT("CursTrackStart"), XXO("Track &Start"),
         FN(OnCursorTrackStart),
         TracksSelectedFlag,
         Options{ wxT("J"), _("Cursor to Track Start") } ),
      Command( wxT("CursTrackEnd"), XXO("Track &End"),
         FN(OnCursorTrackEnd),
         TracksSelectedFlag,
         Options{ wxT("K"), _("Cursor to Track End") } ),

      ClipCursorItems,

      Command( wxT("CursProjectStart"), XXO("&Project Start"),
         FN(OnSkipStart),
         CanStopFlags,
         Options{ wxT("Home"), _("Cursor to Project Start") } ),
      Command( wxT("CursProjectEnd"), XXO("Project E&nd"), FN(OnSkipEnd),
         CanStopFlags,
         Options{ wxT("End"), _("Cursor to Project End") } )
   );
}

MenuTable::BaseItemPtr ExtraClipCursorItems( AudacityProject & );

MenuTable::BaseItemPtr ExtraCursorMenu( AudacityProject & )
{
   using namespace MenuTable;

   return Menu( _("&Cursor"),
      Command( wxT("CursorLeft"), XXO("Cursor &Left"), FN(OnCursorLeft),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Left\twantKeyup\tallowDup") ),
      Command( wxT("CursorRight"), XXO("Cursor &Right"), FN(OnCursorRight),
         TracksExistFlag | TrackPanelHasFocus,
         wxT("Right\twantKeyup\tallowDup") ),
      Command( wxT("CursorShortJumpLeft"), XXO("Cursor Sh&ort Jump Left"),
         FN(OnCursorShortJumpLeft),
         TracksExistFlag | TrackPanelHasFocus, wxT(",") ),
      Command( wxT("CursorShortJumpRight"), XXO("Cursor Shor&t Jump Right"),
         FN(OnCursorShortJumpRight),
         TracksExistFlag | TrackPanelHasFocus, wxT(".") ),
      Command( wxT("CursorLongJumpLeft"), XXO("Cursor Long J&ump Left"),
         FN(OnCursorLongJumpLeft),
         TracksExistFlag | TrackPanelHasFocus, wxT("Shift+,") ),
      Command( wxT("CursorLongJumpRight"), XXO("Cursor Long Ju&mp Right"),
         FN(OnCursorLongJumpRight),
         TracksExistFlag | TrackPanelHasFocus, wxT("Shift+.") ),

      ExtraClipCursorItems
   );
}

MenuTable::BaseItemPtr ExtraSeekMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("See&k"),
      Command( wxT("SeekLeftShort"), XXO("Short Seek &Left During Playback"),
         FN(OnSeekLeftShort), AudioIOBusyFlag, wxT("Left\tallowDup") ),
      Command( wxT("SeekRightShort"),
         XXO("Short Seek &Right During Playback"), FN(OnSeekRightShort),
         AudioIOBusyFlag, wxT("Right\tallowDup") ),
      Command( wxT("SeekLeftLong"), XXO("Long Seek Le&ft During Playback"),
         FN(OnSeekLeftLong), AudioIOBusyFlag, wxT("Shift+Left\tallowDup") ),
      Command( wxT("SeekRightLong"), XXO("Long Seek Rig&ht During Playback"),
         FN(OnSeekRightLong), AudioIOBusyFlag, wxT("Shift+Right\tallowDup") )
   );
}

#undef XXO
#undef FN
