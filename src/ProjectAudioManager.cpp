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
#include "Menus.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "TimeTrack.h"
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
   auto &window = GetProjectFrame( project );
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
               wxString::Format(wxT("%ld"), counter++),
               -2 );
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

      // Add to history
      auto &history = ProjectHistory::Get( project );
      history.PushState(_("Recorded Audio"), _("Record"));

      // Reset timer record 
      if (IsTimerRecordCancelled())
      {
         EditActions::DoUndo( project );
         ResetTimerRecordCancelled();
      }

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

AudioIOStartStreamOptions
DefaultPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   AudioIOStartStreamOptions options { &project,
      ProjectSettings::Get( project ).GetRate() };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   auto timeTrack = TrackList::Get( project ).GetTimeTrack();
   options.envelope = timeTrack ? timeTrack->GetEnvelope() : nullptr;
   options.listener = &ProjectAudioManager::Get( project );
   return options;
}

AudioIOStartStreamOptions
DefaultSpeedPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto PlayAtSpeedRate = gAudioIO->GetBestRate(
      false,     //not capturing
      true,      //is playing
      ProjectSettings::Get( project ).GetRate()  //suggested rate
   );
   AudioIOStartStreamOptions options{ &project, PlayAtSpeedRate };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   auto timeTrack = TrackList::Get( project ).GetTimeTrack();
   options.envelope = timeTrack ? timeTrack->GetEnvelope() : nullptr;
   options.listener = &ProjectAudioManager::Get( project );
   return options;
}
