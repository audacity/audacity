#include "../Audacity.h"
#include "../Experimental.h"

#include "../AdornedRulerPanel.h"
#include "../AudioIO.h"
#include "../DeviceManager.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../SoundActivatedRecord.h"
#include "../TimerRecordDialog.h"
#include "../TrackPanel.h"
#include "../UndoManager.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../prefs/TracksPrefs.h"
#include "../toolbars/ControlToolBar.h"
#include "../toolbars/TranscriptionToolBar.h"
#include "../tracks/ui/Scrubbing.h"

#include <float.h>

// private helper classes and functions
namespace {

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool MakeReadyToPlay(AudacityProject &project,
   bool loop = false, bool cutpreview = false)
{
   ControlToolBar *toolbar = project.GetControlToolBar();
   wxCommandEvent evt;

   // If this project is playing, stop playing
   if (gAudioIO->IsStreamActive(project.GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);

      ::wxMilliSleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   ControlToolBar::PlayAppearance appearance =
      cutpreview ? ControlToolBar::PlayAppearance::CutPreview
      : loop ? ControlToolBar::PlayAppearance::Looped
      : ControlToolBar::PlayAppearance::Straight;
   toolbar->SetPlay(true, appearance);
   toolbar->SetStop(false);

   return true;
}

// Post Timer Recording Actions
// Ensure this matches the enum in TimerRecordDialog.cpp
enum {
   POST_TIMER_RECORD_STOPPED = -3,
   POST_TIMER_RECORD_CANCEL_WAIT,
   POST_TIMER_RECORD_CANCEL,
   POST_TIMER_RECORD_NOTHING,
   POST_TIMER_RECORD_CLOSE,
   POST_TIMER_RECORD_RESTART,
   POST_TIMER_RECORD_SHUTDOWN
};

void DoPlayStop(const CommandContext &context)
{
   auto &project = context.project;
   auto toolbar = project.GetControlToolBar();
   auto token = project.GetAudioIOToken();

   //If this project is playing, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(token)) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->StopPlaying();
   }
   else if (gAudioIO->IsStreamActive()) {
      // If this project isn't playing, but another one is, stop playing the
      // old and start the NEW.

      //find out which project we need;
      AudacityProject* otherProject = NULL;
      for(unsigned i=0; i<gAudacityProjects.size(); i++) {
         if(gAudioIO->IsStreamActive(gAudacityProjects[i]->GetAudioIOToken())) {
            otherProject=gAudacityProjects[i].get();
            break;
         }
      }

      //stop playing the other project
      if(otherProject) {
         ControlToolBar *otherToolbar = otherProject->GetControlToolBar();
         otherToolbar->SetPlay(false);        //Pops
         otherToolbar->SetStop(true);         //Pushes stop down
         otherToolbar->StopPlaying();
      }

      //play the front project
      if (!gAudioIO->IsBusy()) {
         //update the playing area
         project.TP_DisplaySelection();
         //Otherwise, start playing (assuming audio I/O isn't busy)
         //toolbar->SetPlay(true); // Not needed as done in PlayPlayRegion.
         toolbar->SetStop(false);

         // Will automatically set mLastPlayMode
         toolbar->PlayCurrentRegion(false);
      }
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      //toolbar->SetPlay(true); // Not needed as done in PlayPlayRegion.
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void DoMoveToLabel(AudacityProject &project, bool next)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();

   // Find the number of label tracks, and ptr to last track found
   auto trackRange = tracks->Any<LabelTrack>();
   auto lt = *trackRange.rbegin();
   auto nLabelTrack = trackRange.size();

   if (nLabelTrack == 0 ) {
      trackPanel->MessageForScreenReader(_("no label track"));
   }
   else if (nLabelTrack > 1) {
      // find first label track, if any, starting at the focused track
      lt =
         *tracks->Find(trackPanel->GetFocusedTrack()).Filter<LabelTrack>();
      if (!lt)
         trackPanel->MessageForScreenReader(
            _("no label track at or below focused track"));
   }

   // If there is a single label track, or there is a label track at or below
   // the focused track
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   if (lt) {
      int i;
      if (next)
         i = lt->FindNextLabel(selectedRegion);
      else
         i = lt->FindPrevLabel(selectedRegion);

      if (i >= 0) {
         const LabelStruct* label = lt->GetLabel(i);
         if (project.IsAudioActive()) {
            DoPlayStop(project);     // stop
            selectedRegion = label->selectedRegion;
            project.RedrawProject();
            DoPlayStop(project);     // play
         }
         else {
            selectedRegion = label->selectedRegion;
            trackPanel->ScrollIntoView(selectedRegion.t0());
            project.RedrawProject();
         }

         wxString message;
         message.Printf(
            wxT("%s %d of %d"), label->title, i + 1, lt->GetNumLabels() );
         trackPanel->MessageForScreenReader(message);
      }
      else {
         trackPanel->MessageForScreenReader(_("no labels in label track"));
      }
   }
}

}

namespace TransportActions {

// exported helper functions

bool DoPlayStopSelect
(AudacityProject &project, bool click, bool shift)
{
   auto toolbar = project.GetControlToolBar();
   auto &scrubber = project.GetScrubber();
   auto token = project.GetAudioIOToken();
   auto &viewInfo = project.GetViewInfo();
   auto &selection = viewInfo.selectedRegion;

   //If busy, stop playing, make sure everything is unpaused.
   if (scrubber.HasMark() ||
       gAudioIO->IsStreamActive(token)) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down

      // change the selection
      auto time = gAudioIO->GetStreamTime();
      // Test WasSpeedPlaying(), not IsSpeedPlaying()
      // as we could be stopped now.
      if (click && scrubber.WasSpeedPlaying())
      {
         ;// don't change the selection.
      }
      else if (shift && click) {
         // Change the region selection, as if by shift-click at the play head
         auto t0 = selection.t0(), t1 = selection.t1();
         if (time < t0)
            // Grow selection
            t0 = time;
         else if (time > t1)
            // Grow selection
            t1 = time;
         else {
            // Shrink selection, changing the nearer boundary
            if (fabs(t0 - time) < fabs(t1 - time))
               t0 = time;
            else
               t1 = time;
         }
         selection.setTimes(t0, t1);
      }
      else if (click){
         // avoid a point at negative time.
         time = wxMax( time, 0 );
         // Set a point selection, as if by a click at the play head
         selection.setTimes(time, time);
      } else
         // How stop and set cursor always worked
         // -- change t0, collapsing to point only if t1 was greater
         selection.setT0(time, false);

      project.ModifyState(false);           // without bWantsAutoSave
      return true;
   }
   return false;
}

// The code for "OnPlayStopSelect" is simply the code of "OnPlayStop" and
// "OnStopSelect" merged.
void DoPlayStopSelect(AudacityProject &project)
{
   auto toolbar = project.GetControlToolBar();
   wxCommandEvent evt;
   if (DoPlayStopSelect(project, false, false))
      toolbar->OnStop(evt);
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      //toolbar->SetPlay(true); // Not needed as set in PlayPlayRegion()
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void DoPause( AudacityProject &project )
{
   wxCommandEvent evt;

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnPause(evt);
}

void DoRecord( AudacityProject &project )
{
   wxCommandEvent evt;
   evt.SetInt(2); // 0 is default, use 1 to set shift on, 2 to clear it

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnRecord(evt);
}

void DoLockPlayRegion( AudacityProject &project )
{
   auto tracks = project.GetTracks();
   auto ruler = project.GetRulerPanel();

   double start, end;
   project.GetPlayRegion(&start, &end);
   if (start >= tracks->GetEndTime()) {
       AudacityMessageBox(_("Cannot lock region beyond\nend of project."),
                    _("Error"));
   }
   else {
      project.SetPlayRegionLocked( true );
      ruler->Refresh(false);
   }
}

void DoUnlockPlayRegion( AudacityProject &project )
{
   auto ruler = project.GetRulerPanel();

   project.SetPlayRegionLocked( false );
   ruler->Refresh(false);
}

void DoTogglePinnedHead( AudacityProject &project )
{
   bool value = !TracksPrefs::GetPinnedHeadPreference();
   TracksPrefs::SetPinnedHeadPreference(value, true);
   MenuManager::ModifyAllProjectToolbarMenus();

   // Change what happens in case transport is in progress right now
   auto ctb = GetActiveProject()->GetControlToolBar();
   if (ctb)
      ctb->StartScrollingIfPreferred();

   auto ruler = project.GetRulerPanel();
   if (ruler)
      // Update button image
      ruler->UpdateButtonStates();

   auto &scrubber = project.GetScrubber();
   if (scrubber.HasMark())
      scrubber.SetScrollScrubbing(value);
}

void DoStop( AudacityProject &project )
{
   wxCommandEvent evt;

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnStop(evt);
}

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnPlayStop(const CommandContext &context)
{
   DoPlayStop( context.project );
}

void OnPlayStopSelect(const CommandContext &context)
{
   DoPlayStopSelect( context.project );
}

void OnPlayLooped(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project, true) )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->PlayCurrentRegion(true);
}

void OnPause(const CommandContext &context)
{
   DoPause( context.project );
}

void OnRecord(const CommandContext &context)
{
   DoRecord( context.project );
}

// If first choice is record same track 2nd choice is record NEW track
// and vice versa.
void OnRecord2ndChoice(const CommandContext &context)
{
   auto &project = context.project;
   wxCommandEvent evt;
   evt.SetInt(1); // 0 is default, use 1 to set shift on, 2 to clear it

   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->OnRecord(evt);
}

void OnTimerRecord(const CommandContext &context)
{
   auto &project = context.project;
   auto &undoManager = *project.GetUndoManager();

   // MY: Due to improvements in how Timer Recording saves and/or exports
   // it is now safer to disable Timer Recording when there is more than
   // one open project.
   if (AudacityProject::GetOpenProjectCount() > 1) {
      AudacityMessageBox(_("Timer Recording cannot be used with more than one open project.\n\nPlease close any additional projects and try again."),
                   _("Timer Recording"),
                   wxICON_INFORMATION | wxOK);
      return;
   }

   // MY: If the project has unsaved changes then we no longer allow access
   // to Timer Recording.  This decision has been taken as the safest approach
   // preventing issues surrounding "dirty" projects when Automatic Save/Export
   // is used in Timer Recording.
   if ((undoManager.UnsavedChanges()) &&
       (project.GetTracks()->Any() || project.EmptyCanBeDirty())) {
      AudacityMessageBox(_("Timer Recording cannot be used while you have unsaved changes.\n\nPlease save or close this project and try again."),
                   _("Timer Recording"),
                   wxICON_INFORMATION | wxOK);
      return;
   }
   // We use this variable to display "Current Project" in the Timer Recording
   // save project field
   bool bProjectSaved = project.IsProjectSaved();

   //we break the prompting and waiting dialogs into two sections
   //because they both give the user a chance to click cancel
   //and therefore remove the newly inserted track.

   TimerRecordDialog dialog(
      &project, bProjectSaved); /* parent, project saved? */
   int modalResult = dialog.ShowModal();
   if (modalResult == wxID_CANCEL)
   {
      // Cancelled before recording - don't need to do anyting.
   }
   else
   {
      // Timer Record should not record into a selection.
      bool bPreferNewTrack;
      gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
      if (bPreferNewTrack) {
         project.Rewind(false);
      } else {
         project.SkipEnd(false);
      }

      int iTimerRecordingOutcome = dialog.RunWaitDialog();
      switch (iTimerRecordingOutcome) {
      case POST_TIMER_RECORD_CANCEL_WAIT:
         // Canceled on the wait dialog
         project.RollbackState();
         break;
      case POST_TIMER_RECORD_CANCEL:
         // RunWaitDialog() shows the "wait for start" as well as "recording"
         // dialog if it returned POST_TIMER_RECORD_CANCEL it means the user
         // cancelled while the recording, so throw out the fresh track.
         // However, we can't undo it here because the PushState() is called in TrackPanel::OnTimer(),
         // which is blocked by this function.
         // so instead we mark a flag to undo it there.
         project.SetTimerRecordCancelled();
         break;
      case POST_TIMER_RECORD_NOTHING:
         // No action required
         break;
      case POST_TIMER_RECORD_CLOSE:
         wxTheApp->CallAfter( []{ QuitAudacity(); } );
         break;
      case POST_TIMER_RECORD_RESTART:
         // Restart System
#ifdef __WINDOWS__
         system("shutdown /r /f /t 30");
#endif
         break;
      case POST_TIMER_RECORD_SHUTDOWN:
         // Shutdown System
#ifdef __WINDOWS__
         system("shutdown /s /f /t 30");
#endif
         break;
      }
   }
}

#ifdef EXPERIMENTAL_PUNCH_AND_ROLL
void OnPunchAndRoll(const CommandContext &context)
{
   AudacityProject &project = context.project;
   auto &viewInfo = project.GetViewInfo();

   static const auto url =
      wxT("Punch_and_Roll_Record#Using_Punch_and_Roll_Record");

   if (gAudioIO->IsBusy())
      return;

   // Ignore all but left edge of the selection.
   viewInfo.selectedRegion.collapseToT0();
   double t1 = std::max(0.0, viewInfo.selectedRegion.t1());

   // Decide which tracks to record in.
   auto pBar = project.GetControlToolBar();
   auto tracks = pBar->ChooseExistingRecordingTracks(project, true);
   if (tracks.empty()) {
      int recordingChannels =
         std::max(0L, gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2));
      auto message =
         (recordingChannels == 1)
         ? _("Please select in a mono track.")
         : (recordingChannels == 2)
         ? _("Please select in a stereo track.")
         : wxString::Format(
            _("Please select at least %d channels."), recordingChannels);
      ShowErrorDialog(&project, _("Error"), message, url);
      return;
   }

   // Delete the portion of the target tracks right of the selection, but first,
   // remember a part of the deletion for crossfading with the new recording.
   // We may also adjust the starting point leftward if it is too close to the
   // end of the track, so that at least some nonzero crossfade data can be
   // taken.
   PRCrossfadeData crossfadeData;
   const double crossFadeDuration = std::max(0.0,
      gPrefs->Read(AUDIO_ROLL_CROSSFADE_KEY, DEFAULT_ROLL_CROSSFADE_MS)
         / 1000.0
   );

   // The test for t1 == 0.0 stops punch and roll deleting everything where the
   // selection is at zero.  There wouldn't be any cued audio to play in
   // that case, so a normal record, not a punch and roll, is called for.
   bool error = (t1 == 0.0);

   double newt1 = t1;
   for (const auto &wt : tracks) {
      sampleCount testSample(floor(t1 * wt->GetRate()));
      auto clip = wt->GetClipAtSample(testSample);
      if (!clip)
         // Bug 1890 (an enhancement request)
         // Try again, a little to the left.
         // Subtract 10 to allow a selection exactly at or slightly after the
         // end time
         clip = wt->GetClipAtSample(testSample - 10);
      if (!clip)
         error = true;
      else {
         // May adjust t1 left
         // Let's ignore the possibilty of a clip even shorter than the
         // crossfade duration!
         newt1 = std::min(newt1, clip->GetEndTime() - crossFadeDuration);
      }
   }

   if (error) {
      auto message = _("Please select a time within a clip.");
      ShowErrorDialog(&project, _("Error"), message, url);
      return;
   }

   t1 = newt1;
   for (const auto &wt : tracks) {
      const auto endTime = wt->GetEndTime();
      const auto duration =
         std::max(0.0, std::min(crossFadeDuration, endTime - t1));
      const size_t getLen = floor(duration * wt->GetRate());
      std::vector<float> data(getLen);
      if (getLen > 0) {
         float *const samples = data.data();
         const sampleCount pos = wt->TimeToLongSamples(t1);
         wt->Get((samplePtr)samples, floatSample, pos, getLen);
      }
      crossfadeData.push_back(std::move(data));
   }

   // Change tracks only after passing the error checks above
   for (const auto &wt : tracks) {
      wt->Clear(t1, wt->GetEndTime());
   }

   // Choose the tracks for playback.
   TransportTracks transportTracks;
   const auto duplex = ControlToolBar::UseDuplex();
   if (duplex)
      // play all
      transportTracks = GetAllPlaybackTracks(*project.GetTracks(), false, true);
   else
      // play recording tracks only
      std::copy(tracks.begin(), tracks.end(),
         std::back_inserter(transportTracks.playbackTracks));
      
   // Unlike with the usual recording, a track may be chosen both for playback
   // and recording.
   transportTracks.captureTracks = std::move(tracks);

   // Try to start recording
   AudioIOStartStreamOptions options(project.GetDefaultPlayOptions());
   options.preRoll = std::max(0L,
      gPrefs->Read(AUDIO_PRE_ROLL_KEY, DEFAULT_PRE_ROLL_SECONDS));
   options.pCrossfadeData = &crossfadeData;
   bool success = project.GetControlToolBar()->DoRecord(project,
      transportTracks,
      t1, DBL_MAX,
      false, // altAppearance
      options);

   if (success)
      // Undo state will get pushed elsewhere, when record finishes
      ;
   else
      // Roll back the deletions
      project.RollbackState();
}
#endif

void OnLockPlayRegion(const CommandContext &context)
{
   DoLockPlayRegion( context.project );
}

void OnUnlockPlayRegion(const CommandContext &context)
{
   DoUnlockPlayRegion( context.project );
}

void OnRescanDevices(const CommandContext &WXUNUSED(context) )
{
   DeviceManager::Instance()->Rescan();
}

void OnSoundActivated(const CommandContext &context)
{
   AudacityProject &project = context.project;

   SoundActivatedRecord dialog(&project /* parent */ );
   dialog.ShowModal();
}

void OnToggleSoundActivated(const CommandContext &WXUNUSED(context) )
{
   bool pause;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &pause, false);
   gPrefs->Write(wxT("/AudioIO/SoundActivatedRecord"), !pause);
   gPrefs->Flush();
   MenuManager::ModifyAllProjectToolbarMenus();
}

void OnTogglePinnedHead(const CommandContext &context)
{
   DoTogglePinnedHead( context.project );
}

void OnTogglePlayRecording(const CommandContext &WXUNUSED(context) )
{
   bool Duplex;
#ifdef EXPERIMENTAL_DA
   gPrefs->Read(wxT("/AudioIO/Duplex"), &Duplex, false);
#else
   gPrefs->Read(wxT("/AudioIO/Duplex"), &Duplex, true);
#endif
   gPrefs->Write(wxT("/AudioIO/Duplex"), !Duplex);
   gPrefs->Flush();
   MenuManager::ModifyAllProjectToolbarMenus();
}

void OnToggleSWPlaythrough(const CommandContext &WXUNUSED(context) )
{
   bool SWPlaythrough;
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &SWPlaythrough, false);
   gPrefs->Write(wxT("/AudioIO/SWPlaythrough"), !SWPlaythrough);
   gPrefs->Flush();
   MenuManager::ModifyAllProjectToolbarMenus();
}

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void OnToggleAutomatedInputLevelAdjustment(
   const CommandContext &WXUNUSED(context) )
{
   bool AVEnabled;
   gPrefs->Read(
      wxT("/AudioIO/AutomatedInputLevelAdjustment"), &AVEnabled, false);
   gPrefs->Write(wxT("/AudioIO/AutomatedInputLevelAdjustment"), !AVEnabled);
   gPrefs->Flush();
   MenuManager::ModifyAllProjectToolbarMenus();
}
#endif

void OnStop(const CommandContext &context)
{
   DoStop( context.project );
}

void OnPlayOneSecond(const CommandContext &context)
{
   auto &project = context.project;
   if( !MakeReadyToPlay(project) )
      return;

   auto trackPanel = project.GetTrackPanel();
   auto controlToolBar = project.GetControlToolBar();
   auto options = project.GetDefaultPlayOptions();

   double pos = trackPanel->GetMostRecentXPos();
   controlToolBar->PlayPlayRegion
      (SelectedRegion(pos - 0.5, pos + 0.5), options,
       PlayMode::oneSecondPlay);
}

/// The idea for this function (and first implementation)
/// was from Juhana Sadeharju.  The function plays the
/// sound between the current mouse position and the
/// nearest selection boundary.  This gives four possible
/// play regions depending on where the current mouse
/// position is relative to the left and right boundaries
/// of the selection region.
void OnPlayToSelection(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto trackPanel = project.GetTrackPanel();
   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double pos = trackPanel->GetMostRecentXPos();

   double t0,t1;
   // check region between pointer and the nearest selection edge
   if (fabs(pos - selectedRegion.t0()) <
       fabs(pos - selectedRegion.t1())) {
      t0 = t1 = selectedRegion.t0();
   } else {
      t0 = t1 = selectedRegion.t1();
   }
   if( pos < t1)
      t0=pos;
   else
      t1=pos;

   // JKC: oneSecondPlay mode disables auto scrolling
   // On balance I think we should always do this in this function
   // since you are typically interested in the sound EXACTLY
   // where the cursor is.
   // TODO: have 'playing attributes' such as 'with_autoscroll'
   // rather than modes, since that's how we're now using the modes.

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   controlToolBar->PlayPlayRegion
      (SelectedRegion(t0, t1), playOptions, PlayMode::oneSecondPlay);
}

// The next 4 functions provide a limited version of the
// functionality of OnPlayToSelection() for keyboard users

void OnPlayBeforeSelectionStart(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   controlToolBar->PlayPlayRegion(
      SelectedRegion(t0 - beforeLen, t0), playOptions, PlayMode::oneSecondPlay);
}

void OnPlayAfterSelectionStart(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0, t1), playOptions, PlayMode::oneSecondPlay);
   else
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0, t0 + afterLen), playOptions,
         PlayMode::oneSecondPlay);
}

void OnPlayBeforeSelectionEnd(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0, t1), playOptions, PlayMode::oneSecondPlay);
   else
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t1 - beforeLen, t1), playOptions,
         PlayMode::oneSecondPlay);
}


void OnPlayAfterSelectionEnd(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t1 = selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   controlToolBar->PlayPlayRegion(
      SelectedRegion(t1, t1 + afterLen), playOptions, PlayMode::oneSecondPlay);
}

void OnPlayBeforeAndAfterSelectionStart
(const CommandContext &context)
{
   auto &project = context.project;

   if (!MakeReadyToPlay(project))
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0 - beforeLen, t1), playOptions,
         PlayMode::oneSecondPlay);
   else
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0 - beforeLen, t0 + afterLen), playOptions,
         PlayMode::oneSecondPlay);
}

void OnPlayBeforeAndAfterSelectionEnd
(const CommandContext &context)
{
   auto &project = context.project;

   if (!MakeReadyToPlay(project))
      return;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto controlToolBar = project.GetControlToolBar();
   auto playOptions = project.GetDefaultPlayOptions();

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t0, t1 + afterLen), playOptions,
         PlayMode::oneSecondPlay);
   else
      controlToolBar->PlayPlayRegion(
         SelectedRegion(t1 - beforeLen, t1 + afterLen), playOptions,
         PlayMode::oneSecondPlay);
}


void OnPlayCutPreview(const CommandContext &context)
{
   auto &project = context.project;

   if ( !MakeReadyToPlay(project, false, true) )
      return;

   // Play with cut preview
   auto controlToolBar = project.GetControlToolBar();
   controlToolBar->PlayCurrentRegion(false, true);
}

void OnPlayAtSpeed(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->PlayAtSpeed(false, false);
   }
}

void OnPlayAtSpeedLooped(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->PlayAtSpeed(true, false);
   }
}

void OnPlayAtSpeedCutPreview(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->PlayAtSpeed(false, true);
   }
}

void OnSetPlaySpeed(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->ShowPlaySpeedDialog();
   }
}

void OnPlaySpeedInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->AdjustPlaySpeed(0.1f);
   }
}

void OnPlaySpeedDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetTranscriptionToolBar();

   if (tb) {
      tb->AdjustPlaySpeed(-0.1f);
   }
}

void OnMoveToPrevLabel(const CommandContext &context)
{
   auto &project = context.project;
   DoMoveToLabel(project, false);
}

void OnMoveToNextLabel(const CommandContext &context)
{
   auto &project = context.project;
   DoMoveToLabel(project, true);
}

#if 0
// Legacy handlers, not used as of version 2.3.0
void OnStopSelect(const CommandContext &context)
{
   auto &project = context.project;
   auto &viewInfo = project.GetViewInfo();
   auto &selectedRegion = viewInfo.selectedRegion;
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive()) {
      auto controlToolBar = project.GetControlToolBar();
      selectedRegion.setT0(gAudioIO->GetStreamTime(), false);
      controlToolBar->OnStop(evt);
      project.ModifyState(false);           // without bWantsAutoSave
   }
}
#endif

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static TransportActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& TransportActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr CursorMenu( AudacityProject& );

MenuTable::BaseItemPtr TransportMenu( AudacityProject &project )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   static const auto checkOff = Options{}.CheckState( false );
   static const auto checkOn = Options{}.CheckState( true );

   constexpr auto CanStopFlags = AudioIONotBusyFlag | CanStopAudioStreamFlag;

   /* i18n-hint: 'Transport' is the name given to the set of controls that
      play, record, pause etc. */
   return Menu( _("Tra&nsport"),
      Menu( _("Pl&aying"),
         /* i18n-hint: (verb) Start or Stop audio playback*/
         Command( wxT("PlayStop"), XXO("Pl&ay/Stop"), FN(OnPlayStop),
            CanStopAudioStreamFlag, wxT("Space") ),
         Command( wxT("PlayStopSelect"), XXO("Play/Stop and &Set Cursor"),
            FN(OnPlayStopSelect), CanStopAudioStreamFlag, wxT("X") ),
         Command( wxT("PlayLooped"), XXO("&Loop Play"), FN(OnPlayLooped),
            CanStopAudioStreamFlag, wxT("Shift+Space") ),
         Command( wxT("Pause"), XXO("&Pause"), FN(OnPause),
            CanStopAudioStreamFlag, wxT("P") )
      ),

      Menu( _("&Recording"),
         /* i18n-hint: (verb)*/
         Command( wxT("Record1stChoice"), XXO("&Record"), FN(OnRecord),
            CanStopFlags, wxT("R") ),
         // The OnRecord2ndChoice function is: if normal record records beside,
         // it records below, if normal record records below, it records beside.
         // TODO: Do 'the right thing' with other options like TimerRecord.
         Command( wxT("Record2ndChoice"),
            // Our first choice is bound to R (by default)
            // and gets the prime position.
            // We supply the name for the 'other one' here.
            // It should be bound to Shift+R
            (gPrefs->ReadBool("/GUI/PreferNewTrackRecord", false)
             ? _("&Append Record") : _("Record &New Track")),
            false, FN(OnRecord2ndChoice), CanStopFlags,
            wxT("Shift+R")
         ),

         Command( wxT("TimerRecord"), XXO("&Timer Record..."),
            FN(OnTimerRecord), CanStopFlags, wxT("Shift+T") ),

#ifdef EXPERIMENTAL_PUNCH_AND_ROLL
         Command( wxT("PunchAndRoll"), XXO("Punch and Rol&l Record"),
            FN(OnPunchAndRoll),
            WaveTracksExistFlag | AudioIONotBusyFlag, wxT("Shift+D") ),
#endif

         // JKC: I decided to duplicate this between play and record,
         // rather than put it at the top level.
         // CommandManger::AddItem can now cope with simple duplicated items.
         // PRL:  caution, this is a duplicated command name!
         Command( wxT("Pause"), XXO("&Pause"), FN(OnPause),
            CanStopAudioStreamFlag, wxT("P") )
      ),

      // Scrubbing sub-menu
      project.GetScrubber().Menu(),

      CursorMenu,

      Separator(),

      //////////////////////////////////////////////////////////////////////////

      Menu( _("Pla&y Region"),
         Command( wxT("LockPlayRegion"), XXO("&Lock"), FN(OnLockPlayRegion),
            PlayRegionNotLockedFlag ),
         Command( wxT("UnlockPlayRegion"), XXO("&Unlock"),
            FN(OnUnlockPlayRegion), PlayRegionLockedFlag )
      ),

      Separator(),

      Command( wxT("RescanDevices"), XXO("R&escan Audio Devices"),
         FN(OnRescanDevices), AudioIONotBusyFlag | CanStopAudioStreamFlag ),

      Menu( _("Transport &Options"),
         // Sound Activated recording options
         Command( wxT("SoundActivationLevel"),
            XXO("Sound Activation Le&vel..."), FN(OnSoundActivated),
            AudioIONotBusyFlag | CanStopAudioStreamFlag ),
         Command( wxT("SoundActivation"),
            XXO("Sound A&ctivated Recording (on/off)"),
            FN(OnToggleSoundActivated),
            AudioIONotBusyFlag | CanStopAudioStreamFlag, checkOff ),
         Separator(),

         Command( wxT("PinnedHead"), XXO("Pinned Play/Record &Head (on/off)"),
            FN(OnTogglePinnedHead),
            // Switching of scrolling on and off is permitted
            // even during transport
            AlwaysEnabledFlag, checkOff ),

         Command( wxT("Overdub"), XXO("&Overdub (on/off)"),
            FN(OnTogglePlayRecording),
            AudioIONotBusyFlag | CanStopAudioStreamFlag, checkOn ),
         Command( wxT("SWPlaythrough"), XXO("So&ftware Playthrough (on/off)"),
            FN(OnToggleSWPlaythrough),
            AudioIONotBusyFlag | CanStopAudioStreamFlag, checkOff )


#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         ,
         Command( wxT("AutomatedInputLevelAdjustmentOnOff"),
            XXO("A&utomated Recording Level Adjustment (on/off)"),
            FN(OnToggleAutomatedInputLevelAdjustment),
            AudioIONotBusyFlag | CanStopAudioStreamFlag, checkOff )
#endif
      )
   );
}

MenuTable::BaseItemPtr ExtraTransportMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("T&ransport"),
      // PlayStop is already in the menus.
      /* i18n-hint: (verb) Start playing audio*/
      Command( wxT("Play"), XXO("Pl&ay"), FN(OnPlayStop),
         WaveTracksExistFlag | AudioIONotBusyFlag ),
      /* i18n-hint: (verb) Stop playing audio*/
      Command( wxT("Stop"), XXO("Sto&p"), FN(OnStop),
         AudioIOBusyFlag | CanStopAudioStreamFlag ),
      Command( wxT("PlayOneSec"), XXO("Play &One Second"), FN(OnPlayOneSecond),
         CaptureNotBusyFlag, wxT("1") ),
      Command( wxT("PlayToSelection"), XXO("Play to &Selection"),
         FN(OnPlayToSelection),
         CaptureNotBusyFlag, wxT("B") ),
      Command( wxT("PlayBeforeSelectionStart"),
         XXO("Play &Before Selection Start"), FN(OnPlayBeforeSelectionStart),
         CaptureNotBusyFlag, wxT("Shift+F5") ),
      Command( wxT("PlayAfterSelectionStart"),
         XXO("Play Af&ter Selection Start"), FN(OnPlayAfterSelectionStart),
         CaptureNotBusyFlag, wxT("Shift+F6") ),
      Command( wxT("PlayBeforeSelectionEnd"),
         XXO("Play Be&fore Selection End"), FN(OnPlayBeforeSelectionEnd),
         CaptureNotBusyFlag, wxT("Shift+F7") ),
      Command( wxT("PlayAfterSelectionEnd"),
         XXO("Play Aft&er Selection End"), FN(OnPlayAfterSelectionEnd),
         CaptureNotBusyFlag, wxT("Shift+F8") ),
      Command( wxT("PlayBeforeAndAfterSelectionStart"),
         XXO("Play Before a&nd After Selection Start"),
         FN(OnPlayBeforeAndAfterSelectionStart), CaptureNotBusyFlag,
         wxT("Ctrl+Shift+F5") ),
      Command( wxT("PlayBeforeAndAfterSelectionEnd"),
         XXO("Play Before an&d After Selection End"),
         FN(OnPlayBeforeAndAfterSelectionEnd), CaptureNotBusyFlag,
         wxT("Ctrl+Shift+F7") ),
      Command( wxT("PlayCutPreview"), XXO("Play C&ut Preview"),
         FN(OnPlayCutPreview),
         CaptureNotBusyFlag, wxT("C") )
   );
}

MenuTable::BaseItemPtr ExtraPlayAtSpeedMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("&Play-at-Speed"),
      /* i18n-hint: 'Normal Play-at-Speed' doesn't loop or cut preview. */
      Command( wxT("PlayAtSpeed"), XXO("Normal Pl&ay-at-Speed"),
         FN(OnPlayAtSpeed), CaptureNotBusyFlag ),
      Command( wxT("PlayAtSpeedLooped"), XXO("&Loop Play-at-Speed"),
         FN(OnPlayAtSpeedLooped), CaptureNotBusyFlag ),
      Command( wxT("PlayAtSpeedCutPreview"), XXO("Play C&ut Preview-at-Speed"),
         FN(OnPlayAtSpeedCutPreview), CaptureNotBusyFlag ),
      Command( wxT("SetPlaySpeed"), XXO("Ad&just Playback Speed..."),
         FN(OnSetPlaySpeed), CaptureNotBusyFlag ),
      Command( wxT("PlaySpeedInc"), XXO("&Increase Playback Speed"),
         FN(OnPlaySpeedInc), CaptureNotBusyFlag ),
      Command( wxT("PlaySpeedDec"), XXO("&Decrease Playback Speed"),
         FN(OnPlaySpeedDec), CaptureNotBusyFlag ),

      // These were on the original transcription toolbar.
      // But they are not on the
      // shortened one.
      Command( wxT("MoveToPrevLabel"), XXO("Move to &Previous Label"),
         FN(OnMoveToPrevLabel),
         CaptureNotBusyFlag | TrackPanelHasFocus, wxT("Alt+Left") ),
      Command( wxT("MoveToNextLabel"), XXO("Move to &Next Label"),
         FN(OnMoveToNextLabel),
         CaptureNotBusyFlag | TrackPanelHasFocus, wxT("Alt+Right") )
   );
}

#undef XXO
#undef FN
