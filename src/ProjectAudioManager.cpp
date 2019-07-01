/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#include "ProjectAudioManager.h"

#include <wx/frame.h>
#include <wx/statusbr.h>

#include "AudioIO.h"
#include "AutoRecovery.h"
#include "DirManager.h"
#include "LabelTrack.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "TimeTrack.h"
#include "UndoManager.h"
#include "toolbars/ControlToolBar.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Warning.h"

static AudacityProject::AttachedObjects::RegisteredFactory
sProjectAudioManagerKey {
   []( AudacityProject &project ) {
      return std::make_shared< ProjectAudioManager >( project );
   }
};

ProjectAudioManager &ProjectAudioManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectAudioManager >(
      sProjectAudioManagerKey );
}

const ProjectAudioManager &ProjectAudioManager::Get(
   const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectAudioManager::~ProjectAudioManager() = default;

void ProjectAudioManager::OnAudioIORate(int rate)
{
   auto &project = mProject;

   // Be careful to null-check the window.  We might get to this function
   // during shut-down, but a timer hasn't been told to stop sending its
   // messages yet.
   auto pWindow = ProjectWindow::Find( &project );
   if ( !pWindow )
      return;
   auto &window = *pWindow;

   wxString display;
   if (rate > 0) {
      display = wxString::Format(_("Actual Rate: %d"), rate);
   }
   else
      // clear the status field
      ;

   int x, y;
   auto statusBar = window.GetStatusBar();
   statusBar->GetTextExtent(display, &x, &y);
   int widths[] = {
      0,
      ControlToolBar::Get( project ).WidthForStatusBar(statusBar),
      -1,
      x+50
   };
   statusBar->SetStatusWidths(4, widths);
   statusBar->SetStatusText(display, rateStatusBarField);
}

void ProjectAudioManager::OnAudioIOStartRecording()
{
   auto &projectFileIO = ProjectFileIO::Get( mProject );
   // Before recording is started, auto-save the file. The file will have
   // empty tracks at the bottom where the recording will be put into
   projectFileIO.AutoSave();
}

// This is called after recording has stopped and all tracks have flushed.
void ProjectAudioManager::OnAudioIOStopRecording()
{
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = ProjectWindow::Get( project );

   // Only push state if we were capturing and not monitoring
   if (projectAudioIO.GetAudioIOToken() > 0)
   {
      auto &tracks = TrackList::Get( project );
      auto gAudioIO = AudioIO::Get();
      auto &intervals = gAudioIO->LostCaptureIntervals();
      if (intervals.size()) {
         // Make a track with labels for recording errors
         auto uTrack = TrackFactory::Get( project ).NewLabelTrack();
         auto pTrack = uTrack.get();
         tracks.Add( uTrack );
         /* i18n-hint:  A name given to a track, appearing as its menu button.
          The translation should be short or else it will not display well.
          At most, about 11 Latin characters.
          Dropout is a loss of a short sequence of audio sample data from the
          recording */
         pTrack->SetName(_("Dropouts"));
         long counter = 1;
         for (auto &interval : intervals)
            pTrack->AddLabel(
               SelectedRegion{ interval.first,
                  interval.first + interval.second },
               wxString::Format(wxT("%ld"), counter++));
         ShowWarningDialog(&window, wxT("DropoutDetected"), _("\
Recorded audio was lost at the labeled locations. Possible causes:\n\
\n\
Other applications are competing with Audacity for processor time\n\
\n\
You are saving directly to a slow external storage device\n\
"
         ),
         false,
         _("Turn off dropout detection"));
      }

      auto &history = ProjectHistory::Get( project );

      if (IsTimerRecordCancelled()) {
         // discard recording
         history.RollbackState();
         // Reset timer record
         ResetTimerRecordCancelled();
      }
      else
         // Add to history
         history.PushState(_("Recorded Audio"), _("Record"));

      // Refresh the project window
      window.FixScrollbars();
      window.RedrawProject();
   }

   // Write all cached files to disk, if any
   dirManager.WriteCacheToDisk();

   // Now we auto-save again to get the project to a "normal" state again.
   projectFileIO.AutoSave();
}

void ProjectAudioManager::OnAudioIONewBlockFiles(
   const AutoSaveFile & blockFileLog)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   // New blockfiles have been created, so add them to the auto-save file
   const auto &autoSaveFileName = projectFileIO.GetAutoSaveFileName();
   if ( !autoSaveFileName.empty() )
   {
      wxFFile f{ autoSaveFileName, wxT("ab") };
      if (!f.IsOpened())
         return; // Keep recording going, there's not much we can do here
      blockFileLog.Append(f);
      f.Close();
   }
}

void ProjectAudioManager::OnCommitRecording()
{
   const auto project = &mProject;
   TrackList::Get( *project ).ApplyPendingTracks();
}

void ProjectAudioManager::OnSoundActivationThreshold()
{
   auto &project = mProject;
   auto gAudioIO = AudioIO::Get();
   if ( gAudioIO && &project == gAudioIO->GetOwningProject() ) {
      auto &bar = ControlToolBar::Get( project );
      bar.CallAfter(&ControlToolBar::Pause);
   }
}

bool ProjectAudioManager::Playing() const
{
   auto gAudioIO = AudioIO::Get();
   return
      gAudioIO->IsBusy() &&
      ControlToolBar::Get( mProject ).CanStopAudioStream() &&
      // ... and not merely monitoring
      !gAudioIO->IsMonitoring() &&
      // ... and not punch-and-roll recording
      gAudioIO->GetNumCaptureChannels() == 0;
}

bool ProjectAudioManager::Recording() const
{
   auto gAudioIO = AudioIO::Get();
   return
      gAudioIO->IsBusy() &&
      ControlToolBar::Get( mProject).CanStopAudioStream() &&
      gAudioIO->GetNumCaptureChannels() > 0;
}

AudioIOStartStreamOptions
DefaultPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   AudioIOStartStreamOptions options { &project,
      ProjectSettings::Get( project ).GetRate() };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   auto timeTrack = *TrackList::Get( project ).Any<TimeTrack>().begin();
   options.envelope = timeTrack ? timeTrack->GetEnvelope() : nullptr;
   options.listener = ProjectAudioManager::Get( project ).shared_from_this();
   return options;
}

AudioIOStartStreamOptions
DefaultSpeedPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto gAudioIO = AudioIO::Get();
   auto PlayAtSpeedRate = gAudioIO->GetBestRate(
      false,     //not capturing
      true,      //is playing
      ProjectSettings::Get( project ).GetRate()  //suggested rate
   );
   AudioIOStartStreamOptions options{ &project, PlayAtSpeedRate };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   auto timeTrack = *TrackList::Get( project ).Any<TimeTrack>().begin();
   options.envelope = timeTrack ? timeTrack->GetEnvelope() : nullptr;
   options.listener = ProjectAudioManager::Get( project ).shared_from_this();
   return options;
}

#include "AdornedRulerPanel.h"
#include "Menus.h"
#include "ViewInfo.h"
#include "prefs/TracksPrefs.h"
#include "tracks/ui/Scrubbing.h"
#include "widgets/AudacityMessageBox.h"

namespace TransportActions {

// exported helper functions

// Stop playing or recording, if paused.
void StopIfPaused( AudacityProject &project )
{
   if( AudioIOBase::Get()->IsPaused() )
      DoStop( project );
}

bool DoPlayStopSelect
(AudacityProject &project, bool click, bool shift)
{
   auto &toolbar = ControlToolBar::Get( project );
   auto &scrubber = Scrubber::Get( project );
   auto token = ProjectAudioIO::Get( project ).GetAudioIOToken();
   auto &viewInfo = ViewInfo::Get( project );
   auto &selection = viewInfo.selectedRegion;
   auto gAudioIO = AudioIOBase::Get();

   //If busy, stop playing, make sure everything is unpaused.
   if (scrubber.HasMark() ||
       gAudioIO->IsStreamActive(token)) {
      toolbar.SetStop();         //Pushes stop down

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

      ProjectHistory::Get( project ).ModifyState(false);           // without bWantsAutoSave
      return true;
   }
   return false;
}

// The code for "OnPlayStopSelect" is simply the code of "OnPlayStop" and
// "OnStopSelect" merged.
void DoPlayStopSelect(AudacityProject &project)
{
   auto &toolbar = ControlToolBar::Get( project );
   wxCommandEvent evt;
   auto gAudioIO = AudioIO::Get();
   if (DoPlayStopSelect(project, false, false))
      toolbar.OnStop(evt);
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)

      // Will automatically set mLastPlayMode
      toolbar.PlayCurrentRegion(false);
   }
}

void DoPause( AudacityProject &project )
{
   wxCommandEvent evt;

   auto &controlToolBar = ControlToolBar::Get( project );
   controlToolBar.OnPause(evt);
}

void DoRecord( AudacityProject &project )
{
   auto &controlToolBar = ControlToolBar::Get( project );
   controlToolBar.OnRecord(false);
}

void DoLockPlayRegion( AudacityProject &project )
{
   auto &tracks = TrackList::Get( project );
   auto &ruler = AdornedRulerPanel::Get( project );

   auto &viewInfo = ViewInfo::Get( project );
   auto &playRegion = viewInfo.playRegion;
   if (playRegion.GetStart() >= tracks.GetEndTime()) {
       AudacityMessageBox(_("Cannot lock region beyond\nend of project."),
                    _("Error"));
   }
   else {
      playRegion.SetLocked( true );
      ruler.Refresh(false);
   }
}

void DoUnlockPlayRegion( AudacityProject &project )
{
   auto &ruler = AdornedRulerPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetLocked( false );
   ruler.Refresh(false);
}

void DoTogglePinnedHead( AudacityProject &project )
{
   bool value = !TracksPrefs::GetPinnedHeadPreference();
   TracksPrefs::SetPinnedHeadPreference(value, true);
   MenuManager::ModifyAllProjectToolbarMenus();

   // Change what happens in case transport is in progress right now
   auto ctb = ControlToolBar::Find( *GetActiveProject() );
   if (ctb)
      ctb->StartScrollingIfPreferred();

   auto &ruler = AdornedRulerPanel::Get( project );
   // Update button image
   ruler.UpdateButtonStates();

   auto &scrubber = Scrubber::Get( project );
   if (scrubber.HasMark())
      scrubber.SetScrollScrubbing(value);
}

void DoStop( AudacityProject &project )
{
   wxCommandEvent evt;

   auto &controlToolBar = ControlToolBar::Get( project );
   controlToolBar.OnStop(evt);
}

}

#include "CommonCommandFlags.h"

static RegisteredMenuItemEnabler stopIfPaused{{
   PausedFlag,
   AudioIONotBusyFlag,
   []( const AudacityProject &project ){
      return MenuManager::Get( project ).mStopIfWasPaused; },
   []( AudacityProject &project, CommandFlag ){
      if ( MenuManager::Get( project ).mStopIfWasPaused )
         TransportActions::StopIfPaused( project );
   }
}};
