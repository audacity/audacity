/**********************************************************************

  Audacity: A Digital Audio Editor

  TransportState.cpp

  Paul Licameli
  split from ControlToolBar.cpp

***********************************************************************/

#include "Audacity.h"
#include "TransportState.h"

#include <cfloat>

#include "AdornedRulerPanel.h"
#include "AudioIO.h"
#include "Menus.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectSettings.h"
#include "TrackPanel.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ToolManager.h"
#include "tracks/ui/Scrubbing.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Meter.h"

AudacityProject *TransportState::mBusyProject = NULL;
std::shared_ptr<TrackList> TransportState::mCutPreviewTracks;
PlayMode TransportState::sLastPlayMode{ PlayMode::normalPlay };

int TransportState::PlayPlayRegion(const SelectedRegion &selectedRegion,
                                   const AudioIOStartStreamOptions &options,
                                   PlayMode mode,
                                   bool backwards, /* = false */
                                   bool playWhiteSpace /* = false */)
// STRONG-GUARANTEE (for state of mCutPreviewTracks)
{
   if (!CanStopAudioStream())
      return -1;

   bool useMidi = true;

   // Remove these lines to experiment with scrubbing/seeking of note tracks
   if (options.pScrubbingOptions)
      useMidi = false;

   // Uncomment this for laughs!
   // backwards = true;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   // SelectedRegion guarantees t0 <= t1, so we need another boolean argument
   // to indicate backwards play.
   const bool looped = options.playLooped;

   if (backwards)
      std::swap(t0, t1);

#if 0
   {
      PlayAppearance appearance;
      switch( mode ) {
         case PlayMode::cutPreviewPlay:
            appearance = PlayAppearance::CutPreview; break;
         case PlayMode::loopedPlay:
            appearance = PlayAppearance::Looped; break;
         default:
            appearance = PlayAppearance::Straight; break;
      }
      SetPlay(true, appearance);
   }
#endif

   bool success = false;
   auto cleanup = finally( [&] {
      if (!success) {
//         SetPlay(false);
//         SetStop(false);
//         SetRecord(false);
      }
   } );

   if (gAudioIO->IsBusy())
      return -1;

   const bool cutpreview = mode == PlayMode::cutPreviewPlay;
   if (cutpreview && t0==t1)
      return -1; /* msmeyer: makes no sense */

   AudacityProject *p = GetActiveProject();
   if (!p)
      return -1;  // Should never happen, but...

   auto &tracks = TrackList::Get( *p );

   sLastPlayMode = mode;

   bool hasaudio;
   if (useMidi)
      hasaudio = ! tracks.Any<PlayableTrack>().empty();
   else
      hasaudio = ! tracks.Any<WaveTrack>().empty();

   double latestEnd = (playWhiteSpace)? t1 : tracks.GetEndTime();

   if (!hasaudio)
      return -1;  // No need to continue without audio tracks

#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
   double init_seek = 0.0;
#endif

   if (t1 == t0) {
      if (looped) {
         // play selection if there is one, otherwise
         // set start of play region to project start, 
         // and loop the project from current play position.

         auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;
         if ((t0 > selectedRegion.t0()) && (t0 < selectedRegion.t1())) {
            t0 = selectedRegion.t0();
            t1 = selectedRegion.t1();
         }
         else {
            // loop the entire project
            t0 = tracks.GetStartTime();
            t1 = tracks.GetEndTime();
         }
      } else {
         // move t0 to valid range
         if (t0 < 0) {
            t0 = tracks.GetStartTime();
         }
         else if (t0 > tracks.GetEndTime()) {
            t0 = tracks.GetEndTime();
         }
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
         else {
            init_seek = t0;         //AC: init_seek is where playback will 'start'
            t0 = tracks.GetStartTime();
         }
#endif
      }
      t1 = tracks.GetEndTime();
   }
   else {
      // maybe t1 < t0, with backwards scrubbing for instance
      if (backwards)
         std::swap(t0, t1);

      t0 = std::max(0.0, std::min(t0, latestEnd));
      t1 = std::max(0.0, std::min(t1, latestEnd));

      if (backwards)
         std::swap(t0, t1);
   }

   int token = -1;

   if (t1 != t0) {
      if (cutpreview) {
         const double tless = std::min(t0, t1);
         const double tgreater = std::max(t0, t1);
         double beforeLen, afterLen;
         gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
         gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
         double tcp0 = tless-beforeLen;
         double diff = tgreater - tless;
         double tcp1 = (tgreater+afterLen) - diff;
         SetupCutPreviewTracks(tcp0, tless, tgreater, tcp1);
         if (backwards)
            std::swap(tcp0, tcp1);
         if (mCutPreviewTracks)
         {
            AudioIOStartStreamOptions myOptions = options;
            myOptions.cutPreviewGapStart = t0;
            myOptions.cutPreviewGapLen = t1 - t0;
            token = gAudioIO->StartStream(
               GetAllPlaybackTracks(*mCutPreviewTracks, false, useMidi),
               tcp0, tcp1, myOptions);
         }
         else
            // Cannot create cut preview tracks, clean up and exit
            return -1;
      }
      else {
         // Lifted the following into AudacityProject::GetDefaultPlayOptions()
         /*
         if (!timetrack) {
            timetrack = t->GetTimeTrack();
         }
         */
         token = gAudioIO->StartStream(
            GetAllPlaybackTracks(tracks, false, useMidi),
            t0, t1, options);
      }
      if (token != 0) {
         success = true;
         ProjectAudioIO::Get( *p ).SetAudioIOToken(token);
         mBusyProject = p;
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
         //AC: If init_seek was set, now's the time to make it happen.
         gAudioIO->SeekStream(init_seek);
#endif
      }
      else {
         // Bug1627 (part of it):
         // infinite error spew when trying to start scrub:
         // Problem was that the error dialog yields to events,
         // causing recursion to this function in the scrub timer
         // handler!  Easy fix, just delay the user alert instead.
         wxTheApp->CallAfter( []{
         // Show error message if stream could not be opened
            auto &window = ProjectWindow::Get( *::GetActiveProject() );
            ShowErrorDialog(&window,
                         _("Error"),
                         _("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
                         wxT("Error_opening_sound_device"));
         });
      }
   }

   if (!success)
      return -1;

   StartScrollingIfPreferred();

   // Let other UI update appearance
   if (p)
      AdornedRulerPanel::Get( *p ).DrawBothOverlays();

   return token;
}

void TransportState::PlayCurrentRegion(bool looped /* = false */,
                                       bool cutpreview /* = false */)
{
   if (!CanStopAudioStream())
      return;

   AudacityProject *p = GetActiveProject();

   if (p)
   {
      auto playRegion = ViewInfo::Get( *p ).playRegion;

      auto options = DefaultPlayOptions( *p );
      options.playLooped = looped;
      if (cutpreview)
         options.timeTrack = NULL;
      auto mode =
         cutpreview ? PlayMode::cutPreviewPlay
         : options.playLooped ? PlayMode::loopedPlay
         : PlayMode::normalPlay;
      PlayPlayRegion(SelectedRegion(playRegion.GetStart(), playRegion.GetEnd()),
                     options, mode);
   }
}

bool TransportState::CanStopAudioStream()
{
   return (!gAudioIO->IsStreamActive() ||
           gAudioIO->IsMonitoring() ||
           gAudioIO->GetOwningProject() == GetActiveProject());
}

void TransportState::StopPlaying(bool stopStream /* = true*/)
{
   StopScrolling();

   AudacityProject *project = GetActiveProject();

   if(project)
      // Let scrubbing code do some appearance change
      Scrubber::Get( *project ).StopScrubbing();

   if (!CanStopAudioStream())
      return;

//   mStop->PushDown();

//   SetStop(false);
   if(stopStream)
      gAudioIO->StopStream();
//   SetPlay(false);
//   SetRecord(false);

   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      gAudioIO->AILADisable();
   #endif

//   mPause->PopUp();
//   mPaused=false;
   //Make sure you tell gAudioIO to unpause
//   gAudioIO->SetPaused(mPaused);

   ClearCutPreviewTracks();

   mBusyProject = NULL;
   // So that we continue monitoring after playing or recording.
   // also clean the MeterQueues
   if( project ) {
      project->MayStartMonitoring();

      auto &projectAudioIO = ProjectAudioIO::Get( *project );
      MeterPanel *meter = projectAudioIO.GetPlaybackMeter();
      if( meter ) {
         meter->Clear();
      }

      meter = projectAudioIO.GetCaptureMeter();
      if( meter ) {
         meter->Clear();
      }
   }

   const auto toolbar =
      ToolManager::Get( *project ).GetToolBar( ScrubbingBarID );
   toolbar->EnableDisableButtons();
}

void TransportState::Pause()
{
   if (!CanStopAudioStream())
      gAudioIO->SetPaused(!gAudioIO->IsPaused());
   else {
      wxCommandEvent dummy;
//      OnPause(dummy);
   }
}

WaveTrackArray TransportState::ChooseExistingRecordingTracks(
   AudacityProject &proj, bool selectedOnly)
{
   auto p = &proj;
   size_t recordingChannels =
      std::max(0L, gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2));
   bool strictRules = (recordingChannels <= 2);

   // Iterate over all wave tracks, or over selected wave tracks only.
   //
   // In the usual cases of one or two recording channels, seek a first-fit
   // unbroken sub-sequence for which the total number of channels matches the
   // required number exactly.  Never drop inputs or fill only some channels
   // of a track.
   //
   // In case of more than two recording channels, choose tracks only among the
   // selected.  Simply take the earliest wave tracks, until the number of
   // channels is enough.  If there are fewer channels than inputs, but at least
   // one channel, then some of the input channels will be dropped.
   //
   // Resulting tracks may be non-consecutive within the list of all tracks
   // (there may be non-wave tracks between, or non-selected tracks when
   // considering selected tracks only.)

   if (!strictRules && !selectedOnly)
      return {};

   auto &trackList = TrackList::Get( *p );
   std::vector<unsigned> channelCounts;
   WaveTrackArray candidates;
   const auto range = trackList.Leaders<WaveTrack>();
   for ( auto candidate : selectedOnly ? range + &Track::IsSelected : range ) {
      // count channels in this track
      const auto channels = TrackList::Channels( candidate );
      unsigned nChannels = channels.size();

      if (strictRules && nChannels > recordingChannels) {
         // The recording would under-fill this track's channels
         // Can't use any partial accumulated results
         // either.  Keep looking.
         candidates.clear();
         channelCounts.clear();
         continue;
      }
      else {
         // Might use this but may have to discard some of the accumulated
         while(strictRules &&
               nChannels + candidates.size() > recordingChannels) {
            auto nOldChannels = channelCounts[0];
            wxASSERT(nOldChannels > 0);
            channelCounts.erase(channelCounts.begin());
            candidates.erase(candidates.begin(),
                             candidates.begin() + nOldChannels);
         }
         channelCounts.push_back(nChannels);
         for ( auto channel : channels ) {
            candidates.push_back(channel->SharedPointer<WaveTrack>());
            if(candidates.size() == recordingChannels)
               // Done!
               return candidates;
         }
      }
   }

   if (!strictRules && !candidates.empty())
      // good enough
      return candidates;

   // If the loop didn't exit early, we could not find enough channels
   return {};
}

bool TransportState::DoRecord(AudacityProject &project,
   const TransportTracks &tracks,
   double t0, double t1,
   bool altAppearance,
   const AudioIOStartStreamOptions &options)
{
   CommandFlag flags = AlwaysEnabledFlag; // 0 means recalc flags.

   // NB: The call may have the side effect of changing flags.
   bool allowed = MenuManager::Get(project).TryToMakeActionAllowed(
      project,
      flags,
      AudioIONotBusyFlag | CanStopAudioStreamFlag,
      AudioIONotBusyFlag | CanStopAudioStreamFlag);

   if (!allowed)
      return false;
   // ...end of code from CommandHandler.

   if (gAudioIO->IsBusy()) {
      if (!TransportState::CanStopAudioStream() ||
          0 == gAudioIO->GetNumCaptureChannels())
//         mRecord->PopUp()
         ;
      else
//         mRecord->PushDown()
         ;
      return false;
   }

//   SetRecord(true, altAppearance);

   bool success = false;
   auto cleanup = finally([&] {
      if (!success) {
//         SetPlay(false);
  //       SetStop(false);
    //     SetRecord(false);
      }

      // Success or not:
//      UpdateStatusBar(GetActiveProject());
   });

   auto transportTracks = tracks;

   // Will replace any given capture tracks with temporaries
   transportTracks.captureTracks.clear();

   const auto p = &project;

   bool appendRecord = !tracks.captureTracks.empty();

   {
      if (appendRecord) {
         // Append recording:
         // Pad selected/all wave tracks to make them all the same length
         auto &trackList = TrackList::Get( *p );
         for (const auto &wt : tracks.captureTracks)
         {
            auto endTime = wt->GetEndTime();

            // If the track was chosen for recording and playback both,
            // remember the original in preroll tracks, before making the
            // pending replacement.
            bool prerollTrack = make_iterator_range(transportTracks.playbackTracks).contains(wt);
            if (prerollTrack)
                  transportTracks.prerollTracks.push_back(wt);

            // A function that copies all the non-sample data between
            // wave tracks; in case the track recorded to changes scale
            // type (for instance), during the recording.
            auto updater = [](Track &d, const Track &s){
               auto &dst = static_cast<WaveTrack&>(d);
               auto &src = static_cast<const WaveTrack&>(s);
               dst.Reinit(src);
            };

            // Get a copy of the track to be appended, to be pushed into
            // undo history only later.
            auto pending = std::static_pointer_cast<WaveTrack>(
               trackList.RegisterPendingChangedTrack(
                  updater, wt.get() ) );

            // End of current track is before or at recording start time.
            // Less than or equal, not just less than, to ensure a clip boundary.
            // when append recording.
            if (endTime <= t0) {

               // Pad the recording track with silence, up to the
               // maximum time.
               auto newTrack = TrackFactory::Get( *p ).NewWaveTrack();
               newTrack->InsertSilence(0.0, t0 - endTime);
               newTrack->Flush();
               pending->Clear(endTime, t0);
               pending->Paste(endTime, newTrack.get());
            }
            transportTracks.captureTracks.push_back(pending);
         }
         trackList.UpdatePendingTracks();
      }

      if( transportTracks.captureTracks.empty() )
      {   // recording to NEW track(s).
         bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
         wxString defaultTrackName, defaultRecordingTrackName;

         // Count the tracks.
         auto &trackList = TrackList::Get( *p );
         auto numTracks = trackList.Leaders<const WaveTrack>().size();

         auto recordingChannels = std::max(1L, gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2));

         gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &recordingNameCustom, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TrackNumber"), &useTrackNumber, false);
         gPrefs->Read(wxT("/GUI/TrackNames/DateStamp"), &useDateStamp, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TimeStamp"), &useTimeStamp, false);
         defaultTrackName = TracksPrefs::GetDefaultAudioTrackNamePreference();
         gPrefs->Read(wxT("/GUI/TrackNames/RecodingTrackName"), &defaultRecordingTrackName, defaultTrackName);

         wxString baseTrackName = recordingNameCustom? defaultRecordingTrackName : defaultTrackName;

         Track *first {};
         for (int c = 0; c < recordingChannels; c++) {
            auto newTrack = TrackFactory::Get( *p ).NewWaveTrack();
            if (!first)
               first = newTrack.get();

            // Quantize bounds to the rate of the new track.
            if (c == 0) {
               if (t0 < DBL_MAX)
                  t0 = newTrack->LongSamplesToTime(newTrack->TimeToLongSamples(t0));
               if (t1 < DBL_MAX)
                  t1 = newTrack->LongSamplesToTime(newTrack->TimeToLongSamples(t1));
            }

            newTrack->SetOffset(t0);

            wxString nameSuffix = wxString(wxT(""));

            if (useTrackNumber) {
               nameSuffix += wxString::Format(wxT("%d"), 1 + numTracks + c);
            }

            if (useDateStamp) {
               if (!nameSuffix.empty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISODate();
            }

            if (useTimeStamp) {
               if (!nameSuffix.empty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISOTime();
            }

            // ISO standard would be nice, but ":" is unsafe for file name.
            nameSuffix.Replace(wxT(":"), wxT("-"));

           if (baseTrackName.empty()) {
               newTrack->SetName(nameSuffix);
            }
            else if (nameSuffix.empty()) {
               newTrack->SetName(baseTrackName);
            }
            else {
               newTrack->SetName(baseTrackName + wxT("_") + nameSuffix);
            }

            if ((recordingChannels > 2) &&
               !(ProjectSettings::Get(*p).GetTracksFitVerticallyZoomed())) {
               newTrack->SetMinimized(true);
            }

            TrackList::Get( *p ).RegisterPendingNewTrack( newTrack );
            transportTracks.captureTracks.push_back(newTrack);
            // Bug 1548.  New track needs the focus.
            TrackPanel::Get( *p ).SetFocusedTrack( newTrack.get() );
         }
         TrackList::Get( *p ).GroupChannels(*first, recordingChannels);
      }

      //Automated Input Level Adjustment Initialization
      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         gAudioIO->AILAInitialize();
      #endif

      int token = gAudioIO->StartStream(transportTracks, t0, t1, options);

      success = (token != 0);

      if (success) {
         ProjectAudioIO::Get( *p ).SetAudioIOToken(token);
         mBusyProject = p;

         StartScrollingIfPreferred();
      }
      else {
         CancelRecording();

         // Show error message if stream could not be opened
         wxString msg = wxString::Format(_("Error opening recording device.\nError code: %s"), gAudioIO->LastPaErrorString());
         auto &window = ProjectWindow::Get( *::GetActiveProject() );
         ShowErrorDialog( &window, _("Error"), msg, wxT("Error_opening_sound_device") );
      }
   }

   return success;
}

void TransportState::SetupCutPreviewTracks(double WXUNUSED(playStart), double cutStart,
                                           double cutEnd, double  WXUNUSED(playEnd))

// STRONG-GUARANTEE (for state of mCutPreviewTracks)
{
   ClearCutPreviewTracks();
   AudacityProject *p = GetActiveProject();
   if (p) {
      auto trackRange = TrackList::Get( *p ).Selected<const PlayableTrack>();
      if( !trackRange.empty() ) {
         auto cutPreviewTracks = TrackList::Create();
         for (const auto track1 : trackRange) {
            // Duplicate and change tracks
            // Clear has a very small chance of throwing

            auto newTrack = track1->Duplicate();
            newTrack->Clear(cutStart, cutEnd);
            cutPreviewTracks->Add( newTrack );
         }
         // use NOTHROW-GUARANTEE:
         mCutPreviewTracks = cutPreviewTracks;
      }
   }
}

void TransportState::ClearCutPreviewTracks()
{
   if (mCutPreviewTracks)
      mCutPreviewTracks->Clear();
   mCutPreviewTracks.reset();
}

bool TransportState::IsTransportingPinned()
{
   if (!TracksPrefs::GetPinnedHeadPreference())
      return false;
   const auto &scrubber = Scrubber::Get( *::GetActiveProject() );
   return
     !(scrubber.HasMark() &&
       !scrubber.WasSpeedPlaying() &&
       !Scrubber::ShouldScrubPinned());
}

void TransportState::StartScrollingIfPreferred()
{
   if (IsTransportingPinned())
      StartScrolling();
#ifdef __WXMAC__
   else if (Scrubber::Get ( *::GetActiveProject() ).HasMark()) {
      // PRL:  cause many "unnecessary" refreshes.  For reasons I don't understand,
      // doing this causes wheel rotation events (mapped from the double finger vertical
      // swipe) to be delivered more uniformly to the application, so that speed control
      // works better.
      auto &window = ProjectWindow::Get( *::GetActiveProject() );
      window.GetPlaybackScroller().Activate
         (ProjectWindow::PlaybackScroller::Mode::Refresh);
   }
#endif
   else
      StopScrolling();
}

void TransportState::StartScrolling()
{
   using Mode = ProjectWindow::PlaybackScroller::Mode;
   const auto project = GetActiveProject();
   if (project) {
      auto mode = Mode::Pinned;

#if 0
      // Enable these lines to pin the playhead right instead of center,
      // when recording but not overdubbing.
      if (gAudioIO->GetNumCaptureChannels() > 0) {
         // recording

         // Display a fixed recording head while scrolling the waves continuously.
         // If you overdub, you may want to anticipate some context in existing tracks,
         // so center the head.  If not, put it rightmost to display as much wave as we can.
         bool duplex;
#ifdef EXPERIMENTAL_DA
         gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, false);
#else
         gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, true);
#endif
         if (duplex) {
            // See if there is really anything being overdubbed
            if (gAudioIO->GetNumPlaybackChannels() == 0)
               // No.
               duplex = false;
         }

         if (!duplex)
            mode = Mode::Right;
      }
#endif

      auto &window = ProjectWindow::Get( *project );
      window.GetPlaybackScroller().Activate(mode);
   }
}

void TransportState::StopScrolling()
{
   const auto project = GetActiveProject();
   if(project) {
      auto &window = ProjectWindow::Get( *project );
      window.GetPlaybackScroller().Activate
         (ProjectWindow::PlaybackScroller::Mode::Off);
      }
}

void TransportState::CommitRecording()
{
   const auto project = GetActiveProject();
   TrackList::Get( *project ).ApplyPendingTracks();
}

void TransportState::CancelRecording()
{
   const auto project = GetActiveProject();
   TrackList::Get( *project ).ClearPendingTracks();
}
