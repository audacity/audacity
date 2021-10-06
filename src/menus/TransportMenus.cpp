

#include "../AdornedRulerPanel.h"
#include "../AudioIO.h"
#include "../CommonCommandFlags.h"
#include "DeviceManager.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectAudioIO.h"
#include "../ProjectAudioManager.h"
#include "../ProjectFileIO.h"
#include "../ProjectHistory.h"
#include "ProjectRate.h"
#include "../ProjectSettings.h"
#include "../ProjectWindows.h"
#include "../ProjectWindow.h"
#include "../ProjectManager.h"
#include "../SelectUtilities.h"
#include "../SoundActivatedRecord.h"
#include "../TimerRecordDialog.h"
#include "../TrackPanelAx.h"
#include "../TrackPanel.h"
#include "../UndoManager.h"
#include "../WaveClip.h"
#include "../prefs/RecordingPrefs.h"
#include "../prefs/TracksPrefs.h"
#include "../WaveTrack.h"
#include "ViewInfo.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ControlToolBar.h"
#include "../toolbars/TranscriptionToolBar.h"
#include "../widgets/AudacityMessageBox.h"
#include "BasicUI.h"
#include "../widgets/ProgressDialog.h"

#include <float.h>
#include <wx/app.h>

// private helper classes and functions
namespace {

void PlayCurrentRegionAndWait(const CommandContext &context,
                              bool looped = false,
                              bool cutpreview = false)
{
   auto &project = context.project;
   auto &projectAudioManager = ProjectAudioManager::Get(project);

   const auto &playRegion = ViewInfo::Get(project).playRegion;
   double t0 = playRegion.GetStart();
   double t1 = playRegion.GetEnd();

   projectAudioManager.PlayCurrentRegion(looped, cutpreview);

   if (project.mBatchMode > 0 && t0 != t1 && !looped) {
      wxYieldIfNeeded();

      /* i18n-hint: This title appears on a dialog that indicates the progress
         in doing something.*/
      ProgressDialog progress(XO("Progress"), XO("Playing"), pdlgHideCancelButton);
      auto gAudioIO = AudioIO::Get();

      while (projectAudioManager.Playing()) {
         ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
         if (result != ProgressResult::Success) {
            projectAudioManager.Stop();
            if (result != ProgressResult::Stopped) {
               context.Error(wxT("Playing interrupted"));
            }
            break;
         }

         wxMilliSleep(100);
         wxYieldIfNeeded();
      }

      projectAudioManager.Stop();
      wxYieldIfNeeded();
   }
}

void PlayPlayRegionAndWait(const CommandContext &context,
                           const SelectedRegion &selectedRegion,
                           const AudioIOStartStreamOptions &options,
                           PlayMode mode)
{
   auto &project = context.project;
   auto &projectAudioManager = ProjectAudioManager::Get(project);

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();

   projectAudioManager.PlayPlayRegion(selectedRegion, options, mode);

   if (project.mBatchMode > 0) {
      wxYieldIfNeeded();

      /* i18n-hint: This title appears on a dialog that indicates the progress
         in doing something.*/
      ProgressDialog progress(XO("Progress"), XO("Playing"), pdlgHideCancelButton);
      auto gAudioIO = AudioIO::Get();

      while (projectAudioManager.Playing()) {
         ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
         if (result != ProgressResult::Success) {
            projectAudioManager.Stop();
            if (result != ProgressResult::Stopped) {
               context.Error(wxT("Playing interrupted"));
            }
            break;
         }

         wxMilliSleep(100);
         wxYieldIfNeeded();
      }

      projectAudioManager.Stop();
      wxYieldIfNeeded();
   }
}

void RecordAndWait(const CommandContext &context, bool altAppearance)
{
   auto &project = context.project;
   auto &projectAudioManager = ProjectAudioManager::Get(project);

   const auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();

   projectAudioManager.OnRecord(altAppearance);

   if (project.mBatchMode > 0 && t1 != t0) {
      wxYieldIfNeeded();

      /* i18n-hint: This title appears on a dialog that indicates the progress
         in doing something.*/
      ProgressDialog progress(XO("Progress"), XO("Recording"), pdlgHideCancelButton);
      auto gAudioIO = AudioIO::Get();

      while (projectAudioManager.Recording()) {
         ProgressResult result = progress.Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
         if (result != ProgressResult::Success) {
            projectAudioManager.Stop();
            if (result != ProgressResult::Stopped) {
               context.Error(wxT("Recording interrupted"));
            }
            break;
         }

         wxMilliSleep(100);
         wxYieldIfNeeded();
      }

      projectAudioManager.Stop();
      wxYieldIfNeeded();
   }
}

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool MakeReadyToPlay(AudacityProject &project)
{
   auto &toolbar = ControlToolBar::Get( project );
   wxCommandEvent evt;

   // If this project is playing, stop playing
   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO->IsStreamActive(
      ProjectAudioIO::Get( project ).GetAudioIOToken()
   )) {
      // Make momentary changes of button appearances
      toolbar.SetPlay(false);        //Pops
      toolbar.SetStop();         //Pushes stop down
      toolbar.OnStop(evt);

      ::wxMilliSleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   return true;
}

// Returns true if this project was stopped, otherwise false.
// (it may though have stopped another project playing)
bool DoStopPlaying(const CommandContext &context)
{
   auto &project = context.project;
   auto &projectAudioManager = ProjectAudioManager::Get(project);
   auto gAudioIO = AudioIOBase::Get();
   auto &toolbar = ControlToolBar::Get(project);
   auto token = ProjectAudioIO::Get(project).GetAudioIOToken();

   //If this project is playing, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(token)) {
      toolbar.SetStop();         //Pushes stop down
      projectAudioManager.Stop();
      // Playing project was stopped.  All done.
      return true;
   }

   // This project isn't playing.
   // If some other project is playing, stop playing it
   if (gAudioIO->IsStreamActive()) {

      //find out which project we need;
      auto start = AllProjects{}.begin(), finish = AllProjects{}.end(),
         iter = std::find_if(start, finish,
            [&](const AllProjects::value_type &ptr) {
         return gAudioIO->IsStreamActive(
            ProjectAudioIO::Get(*ptr).GetAudioIOToken()); });

      //stop playing the other project
      if (iter != finish) {
         auto otherProject = *iter;
         auto &otherToolbar = ControlToolBar::Get(*otherProject);
         auto &otherProjectAudioManager =
            ProjectAudioManager::Get(*otherProject);
         otherToolbar.SetStop();         //Pushes stop down
         otherProjectAudioManager.Stop();
      }
   }
   return false;
}

void DoStartPlaying(const CommandContext &context, bool looping = false)
{
   auto &project = context.project;
   auto &projectAudioManager = ProjectAudioManager::Get(project);
   auto gAudioIO = AudioIOBase::Get();
   //play the front project
   if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)

      // Will automatically set mLastPlayMode
      PlayCurrentRegionAndWait(context, looping);
   }
}

void DoMoveToLabel(AudacityProject &project, bool next)
{
   auto &tracks = TrackList::Get( project );
   auto &trackFocus = TrackFocus::Get( project );
   auto &window = ProjectWindow::Get( project );
   auto &projectAudioManager = ProjectAudioManager::Get(project);

   // Find the number of label tracks, and ptr to last track found
   auto trackRange = tracks.Any<LabelTrack>();
   auto lt = *trackRange.rbegin();
   auto nLabelTrack = trackRange.size();

   if (nLabelTrack == 0 ) {
      trackFocus.MessageForScreenReader(XO("no label track"));
   }
   else if (nLabelTrack > 1) {
      // find first label track, if any, starting at the focused track
      lt =
         *tracks.Find(trackFocus.Get()).Filter<LabelTrack>();
      if (!lt)
         trackFocus.MessageForScreenReader(
            XO("no label track at or below focused track"));
   }

   // If there is a single label track, or there is a label track at or below
   // the focused track
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   if (lt) {
      int i;
      if (next)
         i = lt->FindNextLabel(selectedRegion);
      else
         i = lt->FindPrevLabel(selectedRegion);

      if (i >= 0) {
         const LabelStruct* label = lt->GetLabel(i);
         bool looping = projectAudioManager.Looping();
         if (ProjectAudioIO::Get( project ).IsAudioActive()) {
            DoStopPlaying(project);
            selectedRegion = label->selectedRegion;
            window.RedrawProject();
            DoStartPlaying(project, looping);
         }
         else {
            selectedRegion = label->selectedRegion;
            window.ScrollIntoView(selectedRegion.t0());
            window.RedrawProject();
         }
         /* i18n-hint:
            String is replaced by the name of a label,
            first number gives the position of that label in a sequence
            of labels,
            and the last number is the total number of labels in the sequence.
         */
         auto message = XO("%s %d of %d")
            .Format( label->title, i + 1, lt->GetNumLabels() );
         trackFocus.MessageForScreenReader(message);
      }
      else {
         trackFocus.MessageForScreenReader(XO("no labels in label track"));
      }
   }
}

}

// Menu handler functions

namespace TransportActions {

struct Handler : CommandHandlerObject {

// This Plays OR Stops audio.  It's a toggle.
// It is usually bound to the SPACE key.
void OnPlayStop(const CommandContext &context)
{
   if (DoStopPlaying(context.project))
      return;
   DoStartPlaying(context.project);
}

void OnPlayStopSelect(const CommandContext &context)
{
   ProjectAudioManager::Get( context.project ).DoPlayStopSelect();
}

void OnPlayLooped(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   PlayCurrentRegionAndWait(context, true);
}

void OnPause(const CommandContext &context)
{
   ProjectAudioManager::Get( context.project ).OnPause();
}

void OnRecord(const CommandContext &context)
{
   RecordAndWait(context, false);
}

// If first choice is record same track 2nd choice is record NEW track
// and vice versa.
void OnRecord2ndChoice(const CommandContext &context)
{
   RecordAndWait(context, true);
}

void OnTimerRecord(const CommandContext &context)
{
   auto &project = context.project;
   const auto &settings = ProjectSettings::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   // MY: Due to improvements in how Timer Recording saves and/or exports
   // it is now safer to disable Timer Recording when there is more than
   // one open project.
   if (AllProjects{}.size() > 1) {
      AudacityMessageBox(
         XO(
"Timer Recording cannot be used with more than one open project.\n\nPlease close any additional projects and try again."),
         XO("Timer Recording"),
         wxICON_INFORMATION | wxOK);
      return;
   }

   // MY: If the project has unsaved changes then we no longer allow access
   // to Timer Recording.  This decision has been taken as the safest approach
   // preventing issues surrounding "dirty" projects when Automatic Save/Export
   // is used in Timer Recording.
   if ((undoManager.UnsavedChanges()) &&
       (TrackList::Get( project ).Any() || settings.EmptyCanBeDirty())) {
      AudacityMessageBox(
         XO(
"Timer Recording cannot be used while you have unsaved changes.\n\nPlease save or close this project and try again."),
         XO("Timer Recording"),
         wxICON_INFORMATION | wxOK);
      return;
   }

   // We check the selected tracks to see if there is enough of them to accommodate
   // all input channels and all of them have the same sampling rate.
   // Those checks will be later performed by recording function anyway,
   // but we want to warn the user about potential problems from the very start.
   const auto selectedTracks{ GetPropertiesOfSelected(project) };
   const int rateOfSelected{ selectedTracks.rateOfSelected };
   const int numberOfSelected{ selectedTracks.numberOfSelected };
   const bool allSameRate{ selectedTracks.allSameRate };

   if (!allSameRate) {
      AudacityMessageBox(XO("The tracks selected "
         "for recording must all have the same sampling rate"),
         XO("Mismatched Sampling Rates"),
         wxICON_ERROR | wxCENTRE);

      return;
   }

   const auto existingTracks{ ProjectAudioManager::ChooseExistingRecordingTracks(project, true, rateOfSelected) };
   if (existingTracks.empty()) {
      if (numberOfSelected > 0 && rateOfSelected !=
          ProjectRate::Get(project).GetRate()) {
         AudacityMessageBox(XO(
            "Too few tracks are selected for recording at this sample rate.\n"
            "(Audacity requires two channels at the same sample rate for\n"
            "each stereo track)"),
            XO("Too Few Compatible Tracks Selected"),
            wxICON_ERROR | wxCENTRE);

         return;
      }
   }
   
   // We use this variable to display "Current Project" in the Timer Recording
   // save project field
   bool bProjectSaved = !ProjectFileIO::Get( project ).IsModified();

   //we break the prompting and waiting dialogs into two sections
   //because they both give the user a chance to click cancel
   //and therefore remove the newly inserted track.

   TimerRecordDialog dialog(
      &window, project, bProjectSaved); /* parent, project, project saved? */
   int modalResult = dialog.ShowModal();
   if (modalResult == wxID_CANCEL)
   {
      // Cancelled before recording - don't need to do anything.
   }
   else
   {
      // Bug #2382
      // Allow recording to start at current cursor position.
      #if 0
      // Timer Record should not record into a selection.
      bool bPreferNewTrack;
      gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
      if (bPreferNewTrack) {
         window.Rewind(false);
      } else {
         window.SkipEnd(false);
      }
      #endif

      int iTimerRecordingOutcome = dialog.RunWaitDialog();
      switch (iTimerRecordingOutcome) {
      case POST_TIMER_RECORD_CANCEL_WAIT:
         // Canceled on the wait dialog
         ProjectHistory::Get( project ).RollbackState();
         break;
      case POST_TIMER_RECORD_CANCEL:
         // RunWaitDialog() shows the "wait for start" as well as "recording"
         // dialog if it returned POST_TIMER_RECORD_CANCEL it means the user
         // cancelled while the recording, so throw out the fresh track.
         // However, we can't undo it here because the PushState() is called in TrackPanel::OnTimer(),
         // which is blocked by this function.
         // so instead we mark a flag to undo it there.
         ProjectAudioManager::Get( project ).SetTimerRecordCancelled();
         break;
      case POST_TIMER_RECORD_NOTHING:
         // No action required
         break;
      case POST_TIMER_RECORD_CLOSE:
         wxTheApp->CallAfter( []{
            // Simulate the application Exit menu item
            wxCommandEvent evt{ wxEVT_MENU, wxID_EXIT };
            wxTheApp->AddPendingEvent( evt );
         } );
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         break;

#ifdef __WINDOWS__
      case POST_TIMER_RECORD_RESTART:
         // Restart System
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         system("shutdown /r /f /t 30");
         break;
      case POST_TIMER_RECORD_SHUTDOWN:
         // Shutdown System
         ProjectManager::Get(project).SetSkipSavePrompt(true);
         system("shutdown /s /f /t 30");
         break;
#endif
      }
   }
}

#ifdef EXPERIMENTAL_PUNCH_AND_ROLL
void OnPunchAndRoll(const CommandContext &context)
{
   AudacityProject &project = context.project;
   auto &viewInfo = ViewInfo::Get( project );

   static const auto url =
      wxT("Punch_and_Roll_Record#Using_Punch_and_Roll_Record");

   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsBusy())
      return;

   // Ignore all but left edge of the selection.
   viewInfo.selectedRegion.collapseToT0();
   double t1 = std::max(0.0, viewInfo.selectedRegion.t1());

   // Checking the selected tracks: making sure they all have the same rate
   const auto selectedTracks{ GetPropertiesOfSelected(project) };
   const int rateOfSelected{ selectedTracks.rateOfSelected };
   const bool allSameRate{ selectedTracks.allSameRate };

   if (!allSameRate) {
      AudacityMessageBox(XO("The tracks selected "
         "for recording must all have the same sampling rate"),
         XO("Mismatched Sampling Rates"),
         wxICON_ERROR | wxCENTRE);

      return;
   }

   // Decide which tracks to record in.
   auto tracks =
      ProjectAudioManager::ChooseExistingRecordingTracks(project, true, rateOfSelected);
   if (tracks.empty()) {
      auto recordingChannels =
         std::max(0, AudioIORecordChannels.Read());
      auto message =
         (recordingChannels == 1)
         ? XO("Please select in a mono track.")
         : (recordingChannels == 2)
         ? XO("Please select in a stereo track or two mono tracks.")
         : XO("Please select at least %d channels.").Format( recordingChannels );
      BasicUI::ShowErrorDialog( *ProjectFramePlacement(&project),
         XO("Error"), message, url);
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
         // Let's ignore the possibility of a clip even shorter than the
         // crossfade duration!
         newt1 = std::min(newt1, clip->GetPlayEndTime() - crossFadeDuration);
      }
   }

   if (error) {
      auto message = XO("Please select a time within a clip.");
      BasicUI::ShowErrorDialog(
         *ProjectFramePlacement(&project), XO("Error"), message, url);
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
         wt->GetFloats(samples, pos, getLen);
      }
      crossfadeData.push_back(std::move(data));
   }

   // Change tracks only after passing the error checks above
   for (const auto &wt : tracks) {
      wt->Clear(t1, wt->GetEndTime());
   }

   // Choose the tracks for playback.
   TransportTracks transportTracks;
   const auto duplex = ProjectAudioManager::UseDuplex();
   if (duplex)
      // play all
      transportTracks =
         ProjectAudioManager::GetAllPlaybackTracks(
            TrackList::Get( project ), false, true);
   else
      // play recording tracks only
      std::copy(tracks.begin(), tracks.end(),
         std::back_inserter(transportTracks.playbackTracks));
      
   // Unlike with the usual recording, a track may be chosen both for playback
   // and recording.
   transportTracks.captureTracks = std::move(tracks);

   // Try to start recording
   auto options = DefaultPlayOptions( project );
   options.rate = rateOfSelected;
   options.preRoll = std::max(0L,
      gPrefs->Read(AUDIO_PRE_ROLL_KEY, DEFAULT_PRE_ROLL_SECONDS));
   options.pCrossfadeData = &crossfadeData;
   bool success = ProjectAudioManager::Get( project ).DoRecord(project,
      transportTracks,
      t1, DBL_MAX,
      false, // altAppearance
      options);

   if (success)
      // Undo state will get pushed elsewhere, when record finishes
      ;
   else
      // Roll back the deletions
      ProjectHistory::Get( project ).RollbackState();
}
#endif

void OnLockPlayRegion(const CommandContext &context)
{
   SelectUtilities::LockPlayRegion(context.project);
}

void OnUnlockPlayRegion(const CommandContext &context)
{
   SelectUtilities::UnlockPlayRegion(context.project);
}

void OnRescanDevices(const CommandContext &WXUNUSED(context) )
{
   DeviceManager::Instance()->Rescan();
}

void OnSoundActivated(const CommandContext &context)
{
   AudacityProject &project = context.project;

   SoundActivatedRecordDialog dialog( &GetProjectFrame( project ) /* parent */ );
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
   AdornedRulerPanel::Get( context.project ).TogglePinnedHead();
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
   ProjectAudioManager::Get( context.project ).Stop();
}

void OnPlayOneSecond(const CommandContext &context)
{
   auto &project = context.project;
   if( !MakeReadyToPlay(project) )
      return;

   auto &trackPanel = TrackPanel::Get( project );
   auto options = DefaultPlayOptions( project );

   double pos = trackPanel.GetMostRecentXPos();
   PlayPlayRegionAndWait(context, SelectedRegion(pos - 0.5, pos + 0.5),
      options, PlayMode::oneSecondPlay);
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

   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double pos = trackPanel.GetMostRecentXPos();

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

   auto playOptions = DefaultPlayOptions( project );

   PlayPlayRegionAndWait(context, SelectedRegion(t0, t1),
      playOptions, PlayMode::oneSecondPlay);
}

// The next 4 functions provide a limited version of the
// functionality of OnPlayToSelection() for keyboard users

void OnPlayBeforeSelectionStart(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   auto playOptions = DefaultPlayOptions( project );

   PlayPlayRegionAndWait(context, SelectedRegion(t0 - beforeLen, t0),
      playOptions, PlayMode::oneSecondPlay);
}

void OnPlayAfterSelectionStart(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto playOptions = DefaultPlayOptions( project );

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      PlayPlayRegionAndWait(context, SelectedRegion(t0, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      PlayPlayRegionAndWait(context, SelectedRegion(t0, t0 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
}

void OnPlayBeforeSelectionEnd(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   auto playOptions = DefaultPlayOptions( project );

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      PlayPlayRegionAndWait(context, SelectedRegion(t0, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      PlayPlayRegionAndWait(context, SelectedRegion(t1 - beforeLen, t1),
         playOptions, PlayMode::oneSecondPlay);
}

void OnPlayAfterSelectionEnd(const CommandContext &context)
{
   auto &project = context.project;

   if( !MakeReadyToPlay(project) )
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t1 = selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto playOptions = DefaultPlayOptions( project );

   PlayPlayRegionAndWait(context, SelectedRegion(t1, t1 + afterLen),
      playOptions, PlayMode::oneSecondPlay);
}

void OnPlayBeforeAndAfterSelectionStart
(const CommandContext &context)
{
   auto &project = context.project;

   if (!MakeReadyToPlay(project))
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto playOptions = DefaultPlayOptions( project );

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      PlayPlayRegionAndWait(context, SelectedRegion(t0 - beforeLen, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      PlayPlayRegionAndWait(context, SelectedRegion(t0 - beforeLen, t0 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
}

void OnPlayBeforeAndAfterSelectionEnd
(const CommandContext &context)
{
   auto &project = context.project;

   if (!MakeReadyToPlay(project))
      return;

   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   auto playOptions = DefaultPlayOptions( project );

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      PlayPlayRegionAndWait(context, SelectedRegion(t0, t1 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
   else
      PlayPlayRegionAndWait(context, SelectedRegion(t1 - beforeLen, t1 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
}

void OnPlayCutPreview(const CommandContext &context)
{
   auto &project = context.project;

   if ( !MakeReadyToPlay(project) )
      return;

   // Play with cut preview
   PlayCurrentRegionAndWait(context, false, true);
}

void OnPlayAtSpeed(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

   if (tb) {
      tb->PlayAtSpeed(false, false);
   }
}

void OnPlayAtSpeedLooped(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

   if (tb) {
      tb->PlayAtSpeed(true, false);
   }
}

void OnPlayAtSpeedCutPreview(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

   if (tb) {
      tb->PlayAtSpeed(false, true);
   }
}

void OnSetPlaySpeed(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

   if (tb) {
      tb->ShowPlaySpeedDialog();
   }
}

void OnPlaySpeedInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

   if (tb) {
      tb->AdjustPlaySpeed(0.1f);
   }
}

void OnPlaySpeedDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &TranscriptionToolBar::Get( project );

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
   auto &history = ProjectHistory::Get( project );
   auto &viewInfo = project.GetViewInfo();
   auto &selectedRegion = viewInfo.selectedRegion;

   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO->IsStreamActive()) {
      selectedRegion.setT0(gAudioIO->GetStreamTime(), false);
      ProjectAudioManager::Get( project ).Stop();
      history.ModifyState(false);           // without bWantsAutoSave
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

#define FN(X) (& TransportActions::Handler :: X)

// Under /MenuBar
namespace {
using namespace MenuTable;
BaseItemSharedPtr TransportMenu()
{
   using Options = CommandManager::Options;

   static const auto CanStopFlags = AudioIONotBusyFlag() | CanStopAudioStreamFlag();

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   /* i18n-hint: 'Transport' is the name given to the set of controls that
      play, record, pause etc. */
   Menu( wxT("Transport"), XXO("Tra&nsport"),
      Section( "Basic",
         Menu( wxT("Play"), XXO("Pl&aying"),
            /* i18n-hint: (verb) Start or Stop audio playback*/
            Command( wxT("PlayStop"), XXO("Pl&ay/Stop"), FN(OnPlayStop),
               CanStopAudioStreamFlag(), wxT("Space") ),
            Command( wxT("PlayStopSelect"), XXO("Play/Stop and &Set Cursor"),
               FN(OnPlayStopSelect), CanStopAudioStreamFlag(), wxT("X") ),
            Command( wxT("PlayLooped"), XXO("&Loop Play"), FN(OnPlayLooped),
               CanStopAudioStreamFlag(), wxT("Shift+Space") ),
            Command( wxT("Pause"), XXO("&Pause"), FN(OnPause),
               CanStopAudioStreamFlag(), wxT("P") )
         ),

         Menu( wxT("Record"), XXO("&Recording"),
            /* i18n-hint: (verb)*/
            Command( wxT("Record1stChoice"), XXO("&Record"), FN(OnRecord),
               CanStopFlags, wxT("R") ),

            // The OnRecord2ndChoice function is: if normal record records beside,
            // it records below, if normal record records below, it records beside.
            // TODO: Do 'the right thing' with other options like TimerRecord.
            // Delayed evaluation in case gPrefs is not yet defined
            [](const AudacityProject&)
            { return Command( wxT("Record2ndChoice"),
               // Our first choice is bound to R (by default)
               // and gets the prime position.
               // We supply the name for the 'other one' here.
               // It should be bound to Shift+R
               (gPrefs->ReadBool("/GUI/PreferNewTrackRecord", false)
                ? XXO("&Append Record") : XXO("Record &New Track")),
               FN(OnRecord2ndChoice), CanStopFlags,
               wxT("Shift+R"),
               findCommandHandler
            ); },

            Command( wxT("TimerRecord"), XXO("&Timer Record..."),
               FN(OnTimerRecord), CanStopFlags, wxT("Shift+T") ),

   #ifdef EXPERIMENTAL_PUNCH_AND_ROLL
            Command( wxT("PunchAndRoll"), XXO("Punch and Rol&l Record"),
               FN(OnPunchAndRoll),
               WaveTracksExistFlag() | AudioIONotBusyFlag(), wxT("Shift+D") ),
   #endif

            // JKC: I decided to duplicate this between play and record,
            // rather than put it at the top level.
            // CommandManger::AddItem can now cope with simple duplicated items.
            // PRL:  caution, this is a duplicated command name!
            Command( wxT("Pause"), XXO("&Pause"), FN(OnPause),
               CanStopAudioStreamFlag(), wxT("P") )
         )
      ),

      Section( "Other",
         Section( "",
            Menu( wxT("PlayRegion"), XXO("Pla&y Region"),
               Command( wxT("LockPlayRegion"), XXO("&Lock"), FN(OnLockPlayRegion),
                  PlayRegionNotLockedFlag() ),
               Command( wxT("UnlockPlayRegion"), XXO("&Unlock"),
                  FN(OnUnlockPlayRegion), PlayRegionLockedFlag() )
            )
         ),

         Command( wxT("RescanDevices"), XXO("R&escan Audio Devices"),
            FN(OnRescanDevices), AudioIONotBusyFlag() | CanStopAudioStreamFlag() ),

         Menu( wxT("Options"), XXO("Transport &Options"),
            Section( "",
               // Sound Activated recording options
               Command( wxT("SoundActivationLevel"),
                  XXO("Sound Activation Le&vel..."), FN(OnSoundActivated),
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag() ),
               Command( wxT("SoundActivation"),
                  XXO("Sound A&ctivated Recording (on/off)"),
                  FN(OnToggleSoundActivated),
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest(wxT("/AudioIO/SoundActivatedRecord"), false) )
            ),

            Section( "",
               Command( wxT("PinnedHead"), XXO("Pinned Play/Record &Head (on/off)"),
                  FN(OnTogglePinnedHead),
                  // Switching of scrolling on and off is permitted
                  // even during transport
                  AlwaysEnabledFlag,
                  Options{}.CheckTest([](const AudacityProject&){
                     return TracksPrefs::GetPinnedHeadPreference(); } ) ),

               Command( wxT("Overdub"), XXO("&Overdub (on/off)"),
                  FN(OnTogglePlayRecording),
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest( wxT("/AudioIO/Duplex"),
#ifdef EXPERIMENTAL_DA
                     false
#else
                     true
#endif
                  ) ),
               Command( wxT("SWPlaythrough"), XXO("So&ftware Playthrough (on/off)"),
                  FN(OnToggleSWPlaythrough),
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest( wxT("/AudioIO/SWPlaythrough"), false ) )


      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
               ,
               Command( wxT("AutomatedInputLevelAdjustmentOnOff"),
                  XXO("A&utomated Recording Level Adjustment (on/off)"),
                  FN(OnToggleAutomatedInputLevelAdjustment),
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest(
                     wxT("/AudioIO/AutomatedInputLevelAdjustment"), false ) )
      #endif
            )
         )
      )
   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( TransportMenu() )
};

BaseItemSharedPtr ExtraTransportMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Transport"), XXO("T&ransport"),
      // PlayStop is already in the menus.
      /* i18n-hint: (verb) Start playing audio*/
      Command( wxT("Play"), XXO("Pl&ay"), FN(OnPlayStop),
         WaveTracksExistFlag() | AudioIONotBusyFlag() ),
      /* i18n-hint: (verb) Stop playing audio*/
      Command( wxT("Stop"), XXO("Sto&p"), FN(OnStop),
         AudioIOBusyFlag() | CanStopAudioStreamFlag() ),
      Command( wxT("PlayOneSec"), XXO("Play &One Second"), FN(OnPlayOneSecond),
         CaptureNotBusyFlag(), wxT("1") ),
      Command( wxT("PlayToSelection"), XXO("Play to &Selection"),
         FN(OnPlayToSelection),
         CaptureNotBusyFlag(), wxT("B") ),
      Command( wxT("PlayBeforeSelectionStart"),
         XXO("Play &Before Selection Start"), FN(OnPlayBeforeSelectionStart),
         CaptureNotBusyFlag(), wxT("Shift+F5") ),
      Command( wxT("PlayAfterSelectionStart"),
         XXO("Play Af&ter Selection Start"), FN(OnPlayAfterSelectionStart),
         CaptureNotBusyFlag(), wxT("Shift+F6") ),
      Command( wxT("PlayBeforeSelectionEnd"),
         XXO("Play Be&fore Selection End"), FN(OnPlayBeforeSelectionEnd),
         CaptureNotBusyFlag(), wxT("Shift+F7") ),
      Command( wxT("PlayAfterSelectionEnd"),
         XXO("Play Aft&er Selection End"), FN(OnPlayAfterSelectionEnd),
         CaptureNotBusyFlag(), wxT("Shift+F8") ),
      Command( wxT("PlayBeforeAndAfterSelectionStart"),
         XXO("Play Before a&nd After Selection Start"),
         FN(OnPlayBeforeAndAfterSelectionStart), CaptureNotBusyFlag(),
         wxT("Ctrl+Shift+F5") ),
      Command( wxT("PlayBeforeAndAfterSelectionEnd"),
         XXO("Play Before an&d After Selection End"),
         FN(OnPlayBeforeAndAfterSelectionEnd), CaptureNotBusyFlag(),
         wxT("Ctrl+Shift+F7") ),
      Command( wxT("PlayCutPreview"), XXO("Play C&ut Preview"),
         FN(OnPlayCutPreview),
         CaptureNotBusyFlag(), wxT("C") )
   ) ) };
   return menu;
}

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraTransportMenu() )
};

BaseItemSharedPtr ExtraPlayAtSpeedMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("PlayAtSpeed"), XXO("&Play-at-Speed"),
      /* i18n-hint: 'Normal Play-at-Speed' doesn't loop or cut preview. */
      Command( wxT("PlayAtSpeed"), XXO("Normal Pl&ay-at-Speed"),
         FN(OnPlayAtSpeed), CaptureNotBusyFlag() ),
      Command( wxT("PlayAtSpeedLooped"), XXO("&Loop Play-at-Speed"),
         FN(OnPlayAtSpeedLooped), CaptureNotBusyFlag() ),
      Command( wxT("PlayAtSpeedCutPreview"), XXO("Play C&ut Preview-at-Speed"),
         FN(OnPlayAtSpeedCutPreview), CaptureNotBusyFlag() ),
      Command( wxT("SetPlaySpeed"), XXO("Ad&just Playback Speed..."),
         FN(OnSetPlaySpeed), CaptureNotBusyFlag() ),
      Command( wxT("PlaySpeedInc"), XXO("&Increase Playback Speed"),
         FN(OnPlaySpeedInc), CaptureNotBusyFlag() ),
      Command( wxT("PlaySpeedDec"), XXO("&Decrease Playback Speed"),
         FN(OnPlaySpeedDec), CaptureNotBusyFlag() )
   ) ) };
   return menu;
}

AttachedItem sAttachment3{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraPlayAtSpeedMenu() )
};

BaseItemSharedPtr ExtraSelectionItems()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr items{
   (FinderScope{ findCommandHandler },
   Items(wxT("MoveToLabel"),
      Command(wxT("MoveToPrevLabel"), XXO("Move to Pre&vious Label"),
         FN(OnMoveToPrevLabel),
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Left")),
      Command(wxT("MoveToNextLabel"), XXO("Move to Ne&xt Label"),
         FN(OnMoveToNextLabel),
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Right"))
   )) };
   return items;
}

AttachedItem sAttachment4{
  { wxT("Optional/Extra/Part1/Select"), { OrderingHint::End, {} } },
  Shared(ExtraSelectionItems())
};

}

#undef FN
