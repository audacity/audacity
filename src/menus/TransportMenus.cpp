#include "AudioIO.h"
#include "../CommonCommandFlags.h"
#include "DeviceManager.h"
#include "LabelTrack.h"
#include "../Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "../ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../ProjectWindows.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../SoundActivatedRecord.h"
#include "../TrackPanelAx.h"
#include "../TrackPanel.h"
#include "../TransportUtilities.h"
#include "UndoManager.h"
#include "../prefs/RecordingPrefs.h"
#include "../prefs/TracksPrefs.h"
#include "WaveTrack.h"
#include "ViewInfo.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ControlToolBar.h"
#include "../toolbars/ToolManager.h"
#include "AudacityMessageBox.h"
#include "BasicUI.h"
#include "ProgressDialog.h"

#include <thread>
#include <float.h>
#include <wx/app.h>

// private helper classes and functions
namespace {

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

      using namespace std::chrono;
      std::this_thread::sleep_for(100ms);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   return true;
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
         bool newDefault = projectAudioManager.Looping();
         if (ProjectAudioIO::Get( project ).IsAudioActive()) {
            TransportUtilities::DoStopPlaying(project);
            selectedRegion = label->selectedRegion;
            window.RedrawProject();
            TransportUtilities::DoStartPlaying(project, newDefault);
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

bool IsLoopingEnabled(const AudacityProject& project)
{
   auto &playRegion = ViewInfo::Get(project).playRegion;
    return playRegion.Active();
}

}

// Strings for menu items and also for dialog titles
/* i18n-hint Sets a starting point for looping play */
static const auto SetLoopInTitle = XXO("Set Loop &In");
/* i18n-hint Sets an ending point for looping play */
static const auto SetLoopOutTitle = XXO("Set Loop &Out");

// Menu handler functions

namespace {

// This plays (once, with fixed bounds) OR Stops audio.  It's a toggle.
// The default binding for Shift+SPACE.
void OnPlayOnceOrStop(const CommandContext &context)
{
   if (TransportUtilities::DoStopPlaying(context.project))
      return;
   TransportUtilities::DoStartPlaying(context.project);
}

void OnPlayStopSelect(const CommandContext &context)
{
   ProjectAudioManager::Get( context.project ).DoPlayStopSelect();
}

// This plays (looping, maybe adjusting the loop) OR Stops audio.  It's a toggle.
// The default binding for SPACE
void OnPlayDefaultOrStop(const CommandContext &context)
{
   auto &project = context.project;
   if (TransportUtilities::DoStopPlaying(project))
      return;

   if( !MakeReadyToPlay(project) )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   TransportUtilities::PlayCurrentRegionAndWait(context, true);
}

void OnPause(const CommandContext &context)
{
   ProjectAudioManager::Get( context.project ).OnPause();
}

void OnRecord(const CommandContext &context)
{
   TransportUtilities::RecordAndWait(context, false);
}

// If first choice is record same track 2nd choice is record NEW track
// and vice versa.
void OnRecord2ndChoice(const CommandContext &context)
{
   TransportUtilities::RecordAndWait(context, true);
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
   using Iterator =
      ChannelGroup::IntervalIterator<const WideChannelGroupInterval>;
   for (const auto &wt : tracks) {
      auto rate = wt->GetRate();
      sampleCount testSample(floor(t1 * rate));
      const auto intervals = as_const(*wt).Intervals();
      auto pred = [rate](sampleCount testSample){ return
         [rate, testSample](const auto &pInterval){
            auto start = floor(pInterval->Start() * rate + 0.5);
            auto end = floor(pInterval->End() * rate + 0.5);
            auto ts = testSample.as_double();
            return ts >= start && ts < end;
         };
      };
      auto begin = intervals.begin(), end = intervals.end(),
         iter = std::find_if(begin, end, pred(testSample));
      if (iter == end)
         // Bug 1890 (an enhancement request)
         // Try again, a little to the left.
         // Subtract 10 to allow a selection exactly at or slightly after the
         // end time
         iter = std::find_if(begin, end, pred(testSample - 10));
      if (iter == end)
         error = true;
      else {
         // May adjust t1 left
         // Let's ignore the possibility of a clip even shorter than the
         // crossfade duration!
         newt1 = std::min(newt1, (*iter).get()->End() - crossFadeDuration);
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
      if (getLen > 0) {
         // TODO more-than-two-channels
         const auto nChannels = std::min<size_t>(2, wt->NChannels());
         crossfadeData.resize(nChannels);
         float *buffers[2]{};
         for (size_t ii = 0; ii < nChannels; ++ii) {
            auto &data = crossfadeData[ii];
            data.resize(getLen);
            buffers[ii] = data.data();
         }
         const sampleCount pos = wt->TimeToLongSamples(t1);
         if (!wt->GetFloats(0, nChannels, buffers, pos, getLen))
            // TODO error message
            return;
      }
   }

   // Change tracks only after passing the error checks above
   for (const auto &wt : tracks)
      wt->Clear(t1, wt->GetEndTime());

   // Choose the tracks for playback.
   TransportSequences transportTracks;
   const auto duplex = ProjectAudioManager::UseDuplex();
   if (duplex)
      // play all
      transportTracks = MakeTransportTracks(
         TrackList::Get( project ), false, true);
   else
      // play recording tracks only
      for (auto &pTrack : tracks)
         transportTracks.playbackSequences.push_back(pTrack);
      
   // Unlike with the usual recording, a track may be chosen both for playback
   // and recording.
   std::copy(tracks.begin(), tracks.end(),
      back_inserter(transportTracks.captureSequences));

   // Try to start recording
   auto options = ProjectAudioIO::GetDefaultOptions(project);
   options.rate = rateOfSelected;
   options.preRoll = std::max(0.0,
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

void OnTogglePlayRegion(const CommandContext &context)
{
   SelectUtilities::TogglePlayRegion(context.project);
}

void OnClearPlayRegion(const CommandContext &context)
{
   SelectUtilities::ClearPlayRegion(context.project);
}

void OnSetPlayRegionIn(const CommandContext &context)
{
   auto &project = context.project;
   auto &playRegion = ViewInfo::Get(project).playRegion;
   if (!playRegion.Active())
      SelectUtilities::ActivatePlayRegion(project);
   SelectUtilities::OnSetRegion(project,
      true, false, SetLoopInTitle.Stripped());
}


void OnSetPlayRegionOut(const CommandContext &context)
{
   auto &project = context.project;
   auto &playRegion = ViewInfo::Get(project).playRegion;
   if (!playRegion.Active())
      SelectUtilities::ActivatePlayRegion(project);
   SelectUtilities::OnSetRegion(project,
      false, false, SetLoopOutTitle.Stripped());
}

void OnSetPlayRegionToSelection(const CommandContext &context)
{
   SelectUtilities::SetPlayRegionToSelection(context.project);
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
   SoundActivatedRecord.Toggle();
   gPrefs->Flush();
   ToolManager::ModifyAllProjectToolbarMenus();
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
   ToolManager::ModifyAllProjectToolbarMenus();
}

void OnToggleSWPlaythrough(const CommandContext &WXUNUSED(context) )
{
   bool SWPlaythrough;
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &SWPlaythrough, false);
   gPrefs->Write(wxT("/AudioIO/SWPlaythrough"), !SWPlaythrough);
   gPrefs->Flush();
   ToolManager::ModifyAllProjectToolbarMenus();
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
   ToolManager::ModifyAllProjectToolbarMenus();
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
   auto options = ProjectAudioIO::GetDefaultOptions(project);

   double pos = trackPanel.GetMostRecentXPos();
   TransportUtilities::PlayPlayRegionAndWait(
      context, SelectedRegion(pos - 0.5, pos + 0.5),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   TransportUtilities::PlayPlayRegionAndWait(
      context, SelectedRegion(t0, t1),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   TransportUtilities::PlayPlayRegionAndWait(
      context, SelectedRegion(t0 - beforeLen, t0),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0, t0 + afterLen),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t1 - beforeLen, t1),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   TransportUtilities::PlayPlayRegionAndWait(
      context, SelectedRegion(t1, t1 + afterLen),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0 - beforeLen, t1),
         playOptions, PlayMode::oneSecondPlay);
   else
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0 - beforeLen, t0 + afterLen),
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

   auto playOptions = ProjectAudioIO::GetDefaultOptions(project);

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t0, t1 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
   else
      TransportUtilities::PlayPlayRegionAndWait(
         context, SelectedRegion(t1 - beforeLen, t1 + afterLen),
         playOptions, PlayMode::oneSecondPlay);
}

void OnPlayCutPreview(const CommandContext &context)
{
   auto &project = context.project;

   if ( !MakeReadyToPlay(project) )
      return;

   // Play with cut preview
   TransportUtilities::PlayCurrentRegionAndWait(context, false, true);
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

// Menu definitions

// Under /MenuBar
using namespace MenuTable;
auto TransportMenu()
{
   using Options = CommandManager::Options;
   static const auto CanStopFlags =
      AudioIONotBusyFlag() | CanStopAudioStreamFlag();
   static auto menu = std::shared_ptr{
   /* i18n-hint: 'Transport' is the name given to the set of controls that
      play, record, pause etc. */
   Menu( wxT("Transport"), XXO("Tra&nsport"),
      Section( "Basic",
         Menu( wxT("Play"), XXO("Pl&aying"),
            /* i18n-hint: (verb) Start or Stop audio playback*/
            Command( wxT("DefaultPlayStop"), XXO("Pl&ay/Stop"), OnPlayDefaultOrStop,
               CanStopAudioStreamFlag(), wxT("Space") ),
            Command( wxT("PlayStopSelect"), XXO("Play/Stop and &Set Cursor"),
               OnPlayStopSelect, CanStopAudioStreamFlag(), wxT("X") ),
            Command( wxT("OncePlayStop"), XXO("Play &Once/Stop"), OnPlayOnceOrStop,
               CanStopAudioStreamFlag(), wxT("Shift+Space") ),
            Command( wxT("Pause"), XXO("&Pause"), OnPause,
               CanStopAudioStreamFlag(), wxT("P") )
         ),

         Menu( wxT("Record"), XXO("&Recording"),
            /* i18n-hint: (verb)*/
            Command( wxT("Record1stChoice"), XXO("&Record"), OnRecord,
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
               OnRecord2ndChoice, CanStopFlags,
               wxT("Shift+R")
            ); },

   #ifdef EXPERIMENTAL_PUNCH_AND_ROLL
            Command( wxT("PunchAndRoll"), XXO("Punch and Rol&l Record"),
               OnPunchAndRoll,
               WaveTracksExistFlag() | AudioIONotBusyFlag(), wxT("Shift+D") ),
   #endif

            // JKC: I decided to duplicate this between play and record,
            // rather than put it at the top level.
            // CommandManger::AddItem can now cope with simple duplicated items.
            // PRL:  caution, this is a duplicated command name!
            Command( wxT("Pause"), XXO("&Pause"), OnPause,
               CanStopAudioStreamFlag(), wxT("P") )
         )
      ),

      Section( "Other",
         Section( "",
            Menu( wxT("PlayRegion"), XXO("&Looping"),
               Command( wxT("TogglePlayRegion"), LoopToggleText,
                  OnTogglePlayRegion, AlwaysEnabledFlag,
                     Options(L"L").CheckTest([](const AudacityProject& project){
                         return IsLoopingEnabled(project);
                     } )),
               Command( wxT("ClearPlayRegion"), XXO("&Clear Loop"),
                  OnClearPlayRegion, AlwaysEnabledFlag, L"Shift+Alt+L" ),
               Command( wxT("SetPlayRegionToSelection"),
                  XXO("&Set Loop to Selection"),
                  OnSetPlayRegionToSelection, AlwaysEnabledFlag,
                     L"Shift+L" ),
               Command( wxT("SetPlayRegionIn"),
                  SetLoopInTitle,
                  OnSetPlayRegionIn, AlwaysEnabledFlag ),
               Command( wxT("SetPlayRegionOut"),
                  SetLoopOutTitle,
                  OnSetPlayRegionOut, AlwaysEnabledFlag )
            )
         ),

         Command( wxT("RescanDevices"), XXO("R&escan Audio Devices"),
            OnRescanDevices, AudioIONotBusyFlag() | CanStopAudioStreamFlag() ),

         Menu( wxT("Options"), XXO("Transport &Options"),
            Section( "Part1",
               // Sound Activated recording options
               Command( wxT("SoundActivationLevel"),
                  XXO("Set sound activation le&vel..."), OnSoundActivated,
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag() ),
               Command( wxT("SoundActivation"),
                  XXO("Enable sound a&ctivated recording"),
                  OnToggleSoundActivated,
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest(SoundActivatedRecord) )
            ),

            Section( "Part2",
               Command( wxT("Overdub"), XXO("Hear &other tracks during recording"),
                  OnTogglePlayRecording,
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest( wxT("/AudioIO/Duplex"),
#ifdef EXPERIMENTAL_DA
                     false
#else
                     true
#endif
                  ) ),
               Command( wxT("SWPlaythrough"), XXO("Enable audible input &monitoring"),
                  OnToggleSWPlaythrough,
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest( wxT("/AudioIO/SWPlaythrough"), false ) )


      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
               ,
               Command( wxT("AutomatedInputLevelAdjustmentOnOff"),
                  XXO("A&utomated Recording Level Adjustment (on/off)"),
                  OnToggleAutomatedInputLevelAdjustment,
                  AudioIONotBusyFlag() | CanStopAudioStreamFlag(),
                  Options{}.CheckTest(
                     wxT("/AudioIO/AutomatedInputLevelAdjustment"), false ) )
      #endif
            )
         )
      )
   ) };
   return menu;
}

AttachedItem sAttachment1{ Indirect(TransportMenu()) };

auto ExtraTransportMenu()
{
   static auto menu = std::shared_ptr{
   Menu( wxT("Transport"), XXO("T&ransport"),
      // PlayStop is already in the menus.
      /* i18n-hint: (verb) Start playing audio*/
      Command( wxT("Play"), XXO("Pl&ay Once"), OnPlayOnceOrStop,
         WaveTracksExistFlag() | AudioIONotBusyFlag() ),
      /* i18n-hint: (verb) Stop playing audio*/
      Command( wxT("Stop"), XXO("Sto&p"), OnStop,
         AudioIOBusyFlag() | CanStopAudioStreamFlag() ),
      Command( wxT("PlayOneSec"), XXO("Play &One Second"), OnPlayOneSecond,
         CaptureNotBusyFlag(), wxT("1") ),
      Command( wxT("PlayToSelection"), XXO("Play to &Selection"),
         OnPlayToSelection,
         CaptureNotBusyFlag(), wxT("B") ),
      Command( wxT("PlayBeforeSelectionStart"),
         XXO("Play &Before Selection Start"), OnPlayBeforeSelectionStart,
         CaptureNotBusyFlag(), wxT("Shift+F5") ),
      Command( wxT("PlayAfterSelectionStart"),
         XXO("Play Af&ter Selection Start"), OnPlayAfterSelectionStart,
         CaptureNotBusyFlag(), wxT("Shift+F6") ),
      Command( wxT("PlayBeforeSelectionEnd"),
         XXO("Play Be&fore Selection End"), OnPlayBeforeSelectionEnd,
         CaptureNotBusyFlag(), wxT("Shift+F7") ),
      Command( wxT("PlayAfterSelectionEnd"),
         XXO("Play Aft&er Selection End"), OnPlayAfterSelectionEnd,
         CaptureNotBusyFlag(), wxT("Shift+F8") ),
      Command( wxT("PlayBeforeAndAfterSelectionStart"),
         XXO("Play Before a&nd After Selection Start"),
         OnPlayBeforeAndAfterSelectionStart, CaptureNotBusyFlag(),
         wxT("Ctrl+Shift+F5") ),
      Command( wxT("PlayBeforeAndAfterSelectionEnd"),
         XXO("Play Before an&d After Selection End"),
         OnPlayBeforeAndAfterSelectionEnd, CaptureNotBusyFlag(),
         wxT("Ctrl+Shift+F7") ),
      Command( wxT("PlayCutPreview"), XXO("Play C&ut Preview"),
         OnPlayCutPreview,
         CaptureNotBusyFlag(), wxT("C") )
   ) };
   return menu;
}

AttachedItem sAttachment2{ Indirect(ExtraTransportMenu()),
   wxT("Optional/Extra/Part1")
};

auto ExtraSelectionItems()
{
   using Options = CommandManager::Options;
   static auto items = std::shared_ptr{
   Items(wxT("MoveToLabel"),
      Command(wxT("MoveToPrevLabel"), XXO("Move to Pre&vious Label"),
         OnMoveToPrevLabel,
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Left")),
      Command(wxT("MoveToNextLabel"), XXO("Move to Ne&xt Label"),
         OnMoveToNextLabel,
         CaptureNotBusyFlag() | TrackPanelHasFocus(), wxT("Alt+Right"))
   ) };
   return items;
}

AttachedItem sAttachment4{ Indirect(ExtraSelectionItems()),
  { wxT("Optional/Extra/Part1/Select"), { OrderingHint::End, {} } }
};

}
