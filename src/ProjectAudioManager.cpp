/**********************************************************************

Audacity: A Digital Audio Editor

ProjectAudioManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/


#include "ProjectAudioManager.h"

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/statusbr.h>
#include <algorithm>

#include "AudioIO.h"
#include "BasicUI.h"
#include "CommonCommandFlags.h"
#include "LabelTrack.h"
#include "Menus.h"
#include "Meter.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectWindows.h"
#include "ScrubState.h"
#include "TimeTrack.h"
#include "TrackPanelAx.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "toolbars/ToolManager.h"
#include "prefs/TracksPrefs.h"
#include "tracks/ui/Scrubbing.h"
#include "tracks/ui/TrackView.h"
#include "widgets/MeterPanelBase.h"
#include "widgets/Warning.h"
#include "widgets/AudacityMessageBox.h"


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

ProjectAudioManager::ProjectAudioManager( AudacityProject &project )
   : mProject{ project }
{
   static ProjectStatus::RegisteredStatusWidthFunction
      registerStatusWidthFunction{ StatusWidthFunction };
   project.Bind( EVT_CHECKPOINT_FAILURE,
      &ProjectAudioManager::OnCheckpointFailure, this );
}

ProjectAudioManager::~ProjectAudioManager() = default;

static TranslatableString FormatRate( int rate )
{
   if (rate > 0) {
      return XO("Actual Rate: %d").Format( rate );
   }
   else
      // clear the status field
      return {};
}

auto ProjectAudioManager::StatusWidthFunction(
   const AudacityProject &project, StatusBarField field )
   -> ProjectStatus::StatusWidthResult
{
   if ( field == rateStatusBarField ) {
      auto &audioManager = ProjectAudioManager::Get( project );
      int rate = audioManager.mDisplayedRate;
      return {
         { { FormatRate( rate ) } },
         50
      };
   }
   return {};
}

namespace {
// The implementation is general enough to allow backwards play too
class CutPreviewPlaybackPolicy final : public PlaybackPolicy {
public:
   CutPreviewPlaybackPolicy(
      double gapLeft, //!< Lower bound track time of of elision
      double gapLength //!< Non-negative track duration
   );
   ~CutPreviewPlaybackPolicy() override;

   void Initialize(PlaybackSchedule &schedule, double rate) override;

   bool Done(PlaybackSchedule &schedule, unsigned long) override;

   double OffsetTrackTime( PlaybackSchedule &schedule, double offset ) override;

   PlaybackSlice GetPlaybackSlice(
      PlaybackSchedule &schedule, size_t available) override;

   std::pair<double, double> AdvancedTrackTime( PlaybackSchedule &schedule,
      double trackTime, size_t nSamples) override;

   bool RepositionPlayback(
      PlaybackSchedule &schedule, const Mixers &playbackMixers,
      size_t frames, size_t available) override;

private:
   double GapStart() const
   { return mReversed ? mGapLeft + mGapLength : mGapLeft; }
   double GapEnd() const
   { return mReversed ? mGapLeft : mGapLeft + mGapLength; }
   bool AtOrBefore(double trackTime1, double trackTime2) const
   { return mReversed ? trackTime1 >= trackTime2 : trackTime1 <= trackTime2; }

   //! Fixed at construction time; these are a track time and duration
   const double mGapLeft, mGapLength;

   //! Starting and ending track times set in Initialize()
   double mStart = 0, mEnd = 0;

   // Non-negative real time durations
   double mDuration1 = 0, mDuration2 = 0;
   double mInitDuration1 = 0, mInitDuration2 = 0;

   bool mDiscontinuity{ false };
   bool mReversed{ false };
};

CutPreviewPlaybackPolicy::CutPreviewPlaybackPolicy(
   double gapLeft, double gapLength)
: mGapLeft{ gapLeft }, mGapLength{ gapLength }
{
   wxASSERT(gapLength >= 0.0);
}

CutPreviewPlaybackPolicy::~CutPreviewPlaybackPolicy() = default;

void CutPreviewPlaybackPolicy::Initialize(
   PlaybackSchedule &schedule, double rate)
{
   PlaybackPolicy::Initialize(schedule, rate);

   // Examine mT0 and mT1 in the schedule only now; ignore changes during play
   double left = mStart = schedule.mT0;
   double right = mEnd = schedule.mT1;
   mReversed = left > right;
   if (mReversed)
      std::swap(left, right);

   if (left < mGapLeft)
      mDuration1 = schedule.ComputeWarpedLength(left, mGapLeft);
   const auto gapEnd = mGapLeft + mGapLength;
   if (gapEnd < right)
      mDuration2 = schedule.ComputeWarpedLength(gapEnd, right);
   if (mReversed)
      std::swap(mDuration1, mDuration2);
   if (sampleCount(mDuration2 * rate) == 0)
      mDuration2 = mDuration1, mDuration1 = 0;
   mInitDuration1 = mDuration1;
   mInitDuration2 = mDuration2;
}

bool CutPreviewPlaybackPolicy::Done(PlaybackSchedule &schedule, unsigned long)
{
   //! Called in the PortAudio thread
   auto diff = schedule.GetTrackTime() - mEnd;
   if (mReversed)
      diff *= -1;
   return sampleCount(diff * mRate) >= 0;
}

double CutPreviewPlaybackPolicy::OffsetTrackTime(
   PlaybackSchedule &schedule, double offset )
{
   // Compute new time by applying the offset, jumping over the gap
   auto time = schedule.GetTrackTime();
   if (offset >= 0) {
      auto space = std::clamp(mGapLeft - time, 0.0, offset);
      time += space;
      offset -= space;
      if (offset > 0)
         time = std::max(time, mGapLeft + mGapLength) + offset;
   }
   else {
      auto space = std::clamp(mGapLeft + mGapLength - time, offset, 0.0);
      time += space;
      offset -= space;
      if (offset < 0)
         time = std::min(time, mGapLeft) + offset;
   }
   time = std::clamp(time, std::min(mStart, mEnd), std::max(mStart, mEnd));

   // Reset the durations
   mDiscontinuity = false;
   mDuration1 = mInitDuration1;
   mDuration2 = mInitDuration2;
   if (AtOrBefore(time, GapStart()))
      mDuration1 = std::max(0.0,
         mDuration1 - fabs(schedule.ComputeWarpedLength(mStart, time)));
   else {
      mDuration1 = 0;
      mDuration2 = std::max(0.0,
         mDuration2 - fabs(schedule.ComputeWarpedLength(GapEnd(), time)));
   }

   return time;
}

PlaybackSlice CutPreviewPlaybackPolicy::GetPlaybackSlice(
   PlaybackSchedule &, size_t available)
{
   size_t frames = available;
   size_t toProduce = frames;
   sampleCount samples1(mDuration1 * mRate);
   if (samples1 > 0 && samples1 < frames)
      // Shorter slice than requested, up to the discontinuity
      toProduce = frames = samples1.as_size_t();
   else if (samples1 == 0) {
      sampleCount samples2(mDuration2 * mRate);
      if (samples2 < frames) {
         toProduce = samples2.as_size_t();
         // Produce some extra silence so that the time queue consumer can
         // satisfy its end condition
         frames = std::min(available, toProduce + TimeQueueGrainSize + 1);
      }
   }
   return { available, frames, toProduce };
}

std::pair<double, double> CutPreviewPlaybackPolicy::AdvancedTrackTime(
   PlaybackSchedule &schedule, double trackTime, size_t nSamples)
{
   auto realDuration = nSamples / mRate;
   if (mDuration1 > 0) {
      mDuration1 = std::max(0.0, mDuration1 - realDuration);
      if (sampleCount(mDuration1 * mRate) == 0) {
         mDuration1 = 0;
         mDiscontinuity = true;
         return { GapStart(), GapEnd() };
      }
   }
   else
      mDuration2 = std::max(0.0, mDuration2 - realDuration);
   if (mReversed)
      realDuration *= -1;
   const double time = schedule.SolveWarpedLength(trackTime, realDuration);

   if ( mReversed ? time <= mEnd : time >= mEnd )
      return {mEnd, std::numeric_limits<double>::infinity()};
   else
      return {time, time};
}

bool CutPreviewPlaybackPolicy::RepositionPlayback( PlaybackSchedule &,
   const Mixers &playbackMixers, size_t, size_t )
{
   if (mDiscontinuity) {
      mDiscontinuity = false;
      auto newTime = GapEnd();
      for (auto &pMixer : playbackMixers)
         pMixer->Reposition(newTime, true);
      // Tell TrackBufferExchange that we aren't done yet
      return false;
   }
   return true;
}
}

int ProjectAudioManager::PlayPlayRegion(const SelectedRegion &selectedRegion,
                                   const AudioIOStartStreamOptions &options,
                                   PlayMode mode,
                                   bool backwards, /* = false */
                                   bool playWhiteSpace /* = false */)
{
   auto &projectAudioManager = *this;
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop )
      return -1;

   bool nonWaveToo = options.playNonWaveTracks;

   // Uncomment this for laughs!
   // backwards = true;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   // SelectedRegion guarantees t0 <= t1, so we need another boolean argument
   // to indicate backwards play.
   const bool looped = (mode == PlayMode::loopedPlay);

   if (backwards)
      std::swap(t0, t1);

   projectAudioManager.SetLooping( mode == PlayMode::loopedPlay );
   projectAudioManager.SetCutting( mode == PlayMode::cutPreviewPlay );

   bool success = false;

   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsBusy())
      return -1;

   const bool cutpreview = mode == PlayMode::cutPreviewPlay;
   if (cutpreview && t0==t1)
      return -1; /* msmeyer: makes no sense */

   AudacityProject *p = &mProject;

   auto &tracks = TrackList::Get( *p );

   mLastPlayMode = mode;

   bool hasaudio;
   if (nonWaveToo)
      hasaudio = ! tracks.Any<PlayableTrack>().empty();
   else
      hasaudio = ! tracks.Any<WaveTrack>().empty();

   double latestEnd = (playWhiteSpace)? t1 : tracks.GetEndTime();

   if (!hasaudio)
      return -1;  // No need to continue without audio tracks

#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
   double init_seek = 0.0;
#endif

   double loop_offset = 0.0;

   if (t1 == t0) {
      if (looped) {
         const auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;
         // play selection if there is one, otherwise
         // set start of play region to project start,
         // and loop the project from current play position.

         if ((t0 > selectedRegion.t0()) && (t0 < selectedRegion.t1())) {
            t0 = selectedRegion.t0();
            t1 = selectedRegion.t1();
         }
         else {
            // loop the entire project
            // Bug2347, loop playback from cursor position instead of project start
            loop_offset = t0 - tracks.GetStartTime();
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
         const double diff = tgreater - tless;
         double tcp1 = tgreater+afterLen;
         if (backwards)
            std::swap(tcp0, tcp1);
         AudioIOStartStreamOptions myOptions = options;
         myOptions.policyFactory =
            [tless, diff]() -> std::unique_ptr<PlaybackPolicy> {
               return std::make_unique<CutPreviewPlaybackPolicy>(tless, diff);
            };
         token = gAudioIO->StartStream(
            GetAllPlaybackTracks(TrackList::Get(*p), false, nonWaveToo),
            tcp0, tcp1, myOptions);
      }
      else {
         token = gAudioIO->StartStream(
            GetAllPlaybackTracks( tracks, false, nonWaveToo ),
            t0, t1, options);
      }
      if (token != 0) {
         success = true;
         ProjectAudioIO::Get(*p).SetAudioIOToken(token);
         if (loop_offset != 0.0) {
            // Bug 2347
            gAudioIO->SeekStream(loop_offset);
         }
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR )
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
         auto &window = GetProjectFrame( mProject );
         window.CallAfter( [&]{
            using namespace BasicUI;
            // Show error message if stream could not be opened
            ShowErrorDialog( *ProjectFramePlacement(&mProject),
               XO("Error"),
               XO("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
               wxT("Error_opening_sound_device"),
               ErrorDialogOptions{ ErrorDialogType::ModalErrorReport } );
         });
      }
   }

   if (!success)
      return -1;

   return token;
}

void ProjectAudioManager::PlayCurrentRegion(bool looped /* = false */,
                                       bool cutpreview /* = false */)
{
   auto &projectAudioManager = *this;
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop )
      return;

   AudacityProject *p = &mProject;

   {

      const auto &playRegion = ViewInfo::Get( *p ).playRegion;

      if (looped)
         cutpreview = false;
      auto options = DefaultPlayOptions( *p, looped );
      if (cutpreview)
         options.envelope = nullptr;
      auto mode =
         cutpreview ? PlayMode::cutPreviewPlay
         : looped ? PlayMode::loopedPlay
         : PlayMode::normalPlay;
      PlayPlayRegion(SelectedRegion(playRegion.GetStart(), playRegion.GetEnd()),
                     options,
                     mode);
   }
}

void ProjectAudioManager::Stop(bool stopStream /* = true*/)
{
   AudacityProject *project = &mProject;
   auto &projectAudioManager = *this;
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop )
      return;

   if(project) {
      // Let scrubbing code do some appearance change
      auto &scrubber = Scrubber::Get( *project );
      scrubber.StopScrubbing();
   }

   auto gAudioIO = AudioIO::Get();

   auto cleanup = finally( [&]{
      projectAudioManager.SetStopping( false );
   } );

   if (stopStream && gAudioIO->IsBusy()) {
      // flag that we are stopping
      projectAudioManager.SetStopping( true );
      // Allow UI to update for that
      while( wxTheApp->ProcessIdle() )
         ;
   }

   if(stopStream)
      gAudioIO->StopStream();

   projectAudioManager.SetLooping( false );
   projectAudioManager.SetCutting( false );

   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      gAudioIO->AILADisable();
   #endif

   projectAudioManager.SetPaused( false );
   //Make sure you tell gAudioIO to unpause
   gAudioIO->SetPaused( false );

   // So that we continue monitoring after playing or recording.
   // also clean the MeterQueues
   if( project ) {
      auto &projectAudioIO = ProjectAudioIO::Get( *project );
      auto meter = projectAudioIO.GetPlaybackMeter();
      if( meter ) {
         meter->Clear();
      }

      meter = projectAudioIO.GetCaptureMeter();
      if( meter ) {
         meter->Clear();
      }
   }

   const auto toolbar = ToolManager::Get( *project ).GetToolBar(ScrubbingBarID);
   if (toolbar)
      toolbar->EnableDisableButtons();
}

void ProjectAudioManager::Pause()
{
   auto &projectAudioManager = *this;
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop ) {
      auto gAudioIO = AudioIO::Get();
      gAudioIO->SetPaused(!gAudioIO->IsPaused());
   }
   else {
      OnPause();
   }
}

WaveTrackArray ProjectAudioManager::ChooseExistingRecordingTracks(
   AudacityProject &proj, bool selectedOnly, double targetRate)
{
   auto p = &proj;
   size_t recordingChannels = std::max(0, AudioIORecordChannels.Read());
   bool strictRules = (recordingChannels <= 2);

   // Iterate over all wave tracks, or over selected wave tracks only.
   // If target rate was specified, ignore all tracks with other rates.
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
      if (targetRate != RATE_NOT_SELECTED && candidate->GetRate() != targetRate)
         continue;

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

/*! @excsafety{Strong} -- For state of current project's tracks */
void ProjectAudioManager::OnRecord(bool altAppearance)
{
   bool bPreferNewTrack;
   gPrefs->Read("/GUI/PreferNewTrackRecord", &bPreferNewTrack, false);
   const bool appendRecord = (altAppearance == bPreferNewTrack);

   // Code from CommandHandler start...
   AudacityProject *p = &mProject;

   if (p) {
      const auto &selectedRegion = ViewInfo::Get( *p ).selectedRegion;
      double t0 = selectedRegion.t0();
      double t1 = selectedRegion.t1();
      // When no time selection, recording duration is 'unlimited'.
      if (t1 == t0)
         t1 = DBL_MAX;

      auto options = DefaultPlayOptions(*p);
      WaveTrackArray existingTracks;

      // Checking the selected tracks: counting them and
      // making sure they all have the same rate
      const auto selectedTracks{ GetPropertiesOfSelected(*p) };
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

      if (appendRecord) {
         const auto trackRange = TrackList::Get( *p ).Any< const WaveTrack >();

         // Try to find wave tracks to record into.  (If any are selected,
         // try to choose only from them; else if wave tracks exist, may record into any.)
         existingTracks = ChooseExistingRecordingTracks(*p, true, rateOfSelected);
         if (!existingTracks.empty()) {
            t0 = std::max(t0,
               (trackRange + &Track::IsSelected).max(&Track::GetEndTime));
         }
         else {
            if (numberOfSelected > 0 && rateOfSelected != options.rate) {
               AudacityMessageBox(XO(
                  "Too few tracks are selected for recording at this sample rate.\n"
                  "(Audacity requires two channels at the same sample rate for\n"
                  "each stereo track)"),
                  XO("Too Few Compatible Tracks Selected"),
                  wxICON_ERROR | wxCENTRE);

               return;
            }

            existingTracks = ChooseExistingRecordingTracks(*p, false, options.rate);
            if(!existingTracks.empty())
                t0 = std::max( t0, trackRange.max( &Track::GetEndTime ) );
            // If suitable tracks still not found, will record into NEW ones,
            // starting with t0
         }
         
         // Whether we decided on NEW tracks or not:
         if (t1 <= selectedRegion.t0() && selectedRegion.t1() > selectedRegion.t0()) {
            t1 = selectedRegion.t1();   // record within the selection
         }
         else {
            t1 = DBL_MAX;        // record for a long, long time
         }
      }

      TransportTracks transportTracks;
      if (UseDuplex()) {
         // Remove recording tracks from the list of tracks for duplex ("overdub")
         // playback.
         /* TODO: set up stereo tracks if that is how the user has set up
          * their preferences, and choose sample format based on prefs */
         transportTracks = GetAllPlaybackTracks(TrackList::Get( *p ), false, true);
         for (const auto &wt : existingTracks) {
            auto end = transportTracks.playbackTracks.end();
            auto it = std::find(transportTracks.playbackTracks.begin(), end, wt);
            if (it != end)
               transportTracks.playbackTracks.erase(it);
         }
      }

      transportTracks.captureTracks = existingTracks;

      if (rateOfSelected != RATE_NOT_SELECTED)
         options.rate = rateOfSelected;

      DoRecord(*p, transportTracks, t0, t1, altAppearance, options);
   }
}

bool ProjectAudioManager::UseDuplex()
{
   bool duplex;
   gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex,
#ifdef EXPERIMENTAL_DA
      false
#else
      true
#endif
      );
   return duplex;
}

bool ProjectAudioManager::DoRecord(AudacityProject &project,
   const TransportTracks &tracks,
   double t0, double t1,
   bool altAppearance,
   const AudioIOStartStreamOptions &options)
{
   auto &projectAudioManager = *this;

   CommandFlag flags = AlwaysEnabledFlag; // 0 means recalc flags.

   // NB: The call may have the side effect of changing flags.
   bool allowed = MenuManager::Get(project).TryToMakeActionAllowed(
      flags,
      AudioIONotBusyFlag() | CanStopAudioStreamFlag());

   if (!allowed)
      return false;
   // ...end of code from CommandHandler.

   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsBusy())
      return false;

   projectAudioManager.SetAppending( !altAppearance );

   bool success = false;

   auto transportTracks = tracks;

   // Will replace any given capture tracks with temporaries
   transportTracks.captureTracks.clear();

   const auto p = &project;

   bool appendRecord = !tracks.captureTracks.empty();

   auto makeNewClipName = [&](WaveTrack* track) {
       for (auto i = 1;; ++i)
       {
           //i18n-hint a numerical suffix added to distinguish otherwise like-named clips when new record started
           auto name = XC("%s #%d", "clip name template").Format(track->GetName(), i).Translation();
           if (track->FindClipByName(name) == nullptr)
               return name;
       }
   };

   {
      if (appendRecord) {
         // Append recording:
         // Pad selected/all wave tracks to make them all the same length
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
               TrackList::Get( *p ).RegisterPendingChangedTrack(
                  updater, wt.get() ) );

            // End of current track is before or at recording start time.
            // Less than or equal, not just less than, to ensure a clip boundary.
            // when append recording.
            if (endTime <= t0) {
               pending->CreateClip(t0, makeNewClipName(pending.get()));
            }
            transportTracks.captureTracks.push_back(pending);
         }
         TrackList::Get( *p ).UpdatePendingTracks();
      }

      if( transportTracks.captureTracks.empty() )
      {   // recording to NEW track(s).
         bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
         wxString defaultTrackName, defaultRecordingTrackName;

         // Count the tracks.
         auto &trackList = TrackList::Get( *p );
         auto numTracks = trackList.Leaders< const WaveTrack >().size();

         auto recordingChannels = std::max(1, AudioIORecordChannels.Read());

         gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &recordingNameCustom, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TrackNumber"), &useTrackNumber, false);
         gPrefs->Read(wxT("/GUI/TrackNames/DateStamp"), &useDateStamp, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TimeStamp"), &useTimeStamp, false);
         defaultTrackName = TracksPrefs::GetDefaultAudioTrackNamePreference();
         gPrefs->Read(wxT("/GUI/TrackNames/RecodingTrackName"), &defaultRecordingTrackName, defaultTrackName);

         wxString baseTrackName = recordingNameCustom? defaultRecordingTrackName : defaultTrackName;

         Track *first {};
         for (int c = 0; c < recordingChannels; c++) {
            auto newTrack = WaveTrackFactory::Get( *p ).NewWaveTrack();
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
               nameSuffix += wxString::Format(wxT("%d"), 1 + (int) numTracks + c);
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
            //create a new clip with a proper name before recording is started
            newTrack->CreateClip(.0, makeNewClipName(newTrack.get()));

            TrackList::Get( *p ).RegisterPendingNewTrack( newTrack );

            if ((recordingChannels > 2) &&
                !(ProjectSettings::Get(*p).GetTracksFitVerticallyZoomed())) {
               TrackView::Get( *newTrack ).SetMinimized(true);
            }

            transportTracks.captureTracks.push_back(newTrack);
         }
         TrackList::Get( *p ).MakeMultiChannelTrack(*first, recordingChannels, true);
         // Bug 1548.  First of new tracks needs the focus.
         TrackFocus::Get(*p).Set(first);
         if (TrackList::Get(*p).back())
            TrackList::Get(*p).back()->EnsureVisible();
      }

      //Automated Input Level Adjustment Initialization
      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         gAudioIO->AILAInitialize();
      #endif

      int token = gAudioIO->StartStream(transportTracks, t0, t1, options);

      success = (token != 0);

      if (success) {
         ProjectAudioIO::Get( *p ).SetAudioIOToken(token);
      }
      else {
         CancelRecording();

         // Show error message if stream could not be opened
         auto msg = XO("Error opening recording device.\nError code: %s")
            .Format( gAudioIO->LastPaErrorString() );
         using namespace BasicUI;
         ShowErrorDialog( *ProjectFramePlacement(&mProject),
            XO("Error"), msg, wxT("Error_opening_sound_device"),
            ErrorDialogOptions{ ErrorDialogType::ModalErrorReport } );
      }
   }

   return success;
}

void ProjectAudioManager::OnPause()
{
   auto &projectAudioManager = *this;
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop ) {
      return;
   }

   bool paused = !projectAudioManager.Paused();
   projectAudioManager.SetPaused( paused );

   auto gAudioIO = AudioIO::Get();

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT

   auto project = &mProject;
   auto &scrubber = Scrubber::Get( *project );

   // Bug 1494 - Pausing a seek or scrub should just STOP as
   // it is confusing to be in a paused scrub state.
   bool bStopInstead = paused &&
      ScrubState::IsScrubbing() &&
      !scrubber.IsSpeedPlaying() &&
      !scrubber.IsKeyboardScrubbing();

   if (bStopInstead) {
      Stop();
      return;
   }

   if (ScrubState::IsScrubbing())
      scrubber.Pause(paused);
   else
#endif
   {
      gAudioIO->SetPaused(paused);
   }
}

void ProjectAudioManager::CancelRecording()
{
   const auto project = &mProject;
   TrackList::Get( *project ).ClearPendingTracks();
}

void ProjectAudioManager::OnAudioIORate(int rate)
{
   auto &project = mProject;

   mDisplayedRate = rate;

   auto display = FormatRate( rate );

   ProjectStatus::Get( project ).Set( display, rateStatusBarField );
}

void ProjectAudioManager::OnAudioIOStartRecording()
{
   // Auto-save was done here before, but it is unnecessary, provided there
   // are sufficient autosaves when pushing or modifying undo states.
}

// This is called after recording has stopped and all tracks have flushed.
void ProjectAudioManager::OnAudioIOStopRecording()
{
   auto &project = mProject;
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = GetProjectFrame( project );

   // Only push state if we were capturing and not monitoring
   if (projectAudioIO.GetAudioIOToken() > 0)
   {
      auto &history = ProjectHistory::Get( project );

      if (IsTimerRecordCancelled()) {
         // discard recording
         history.RollbackState();
         // Reset timer record
         ResetTimerRecordCancelled();
      }
      else {
         // Add to history
         // We want this to have No-fail-guarantee if we get here from exception
         // handling of recording, and that means we rely on the last autosave
         // successfully committed to the database, not risking a failure
         history.PushState(XO("Recorded Audio"), XO("Record"),
            UndoPush::NOAUTOSAVE);

         // Now, we may add a label track to give information about
         // dropouts.  We allow failure of this.
         auto &tracks = TrackList::Get( project );
         auto gAudioIO = AudioIO::Get();
         auto &intervals = gAudioIO->LostCaptureIntervals();
         if (intervals.size()) {
            // Make a track with labels for recording errors
            auto uTrack = std::make_shared<LabelTrack>();
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

            history.ModifyState( true ); // this might fail and throw

            // CallAfter so that we avoid any problems of yielding
            // to the event loop while still inside the timer callback,
            // entering StopStream() recursively
            wxTheApp->CallAfter( [&] {
               ShowWarningDialog(&window, wxT("DropoutDetected"), XO("\
Recorded audio was lost at the labeled locations. Possible causes:\n\
\n\
Other applications are competing with Audacity for processor time\n\
\n\
You are saving directly to a slow external storage device\n\
"
                  ),
                  false,
                  XXO("Turn off dropout detection"));
            });
         }
      }
   }
}

void ProjectAudioManager::OnAudioIONewBlocks(const WaveTrackArray *tracks)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   projectFileIO.AutoSave(true);
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
   if ( gAudioIO && &project == gAudioIO->GetOwningProject().get() ) {
      wxTheApp->CallAfter( [this]{ Pause(); } );
   }
}

void ProjectAudioManager::OnCheckpointFailure(wxCommandEvent &evt)
{
   evt.Skip();
   Stop();
}

bool ProjectAudioManager::Playing() const
{
   auto gAudioIO = AudioIO::Get();
   return
      gAudioIO->IsBusy() &&
      CanStopAudioStream() &&
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
      CanStopAudioStream() &&
      gAudioIO->GetNumCaptureChannels() > 0;
}

bool ProjectAudioManager::CanStopAudioStream() const
{
   auto gAudioIO = AudioIO::Get();
   return (!gAudioIO->IsStreamActive() ||
           gAudioIO->IsMonitoring() ||
           gAudioIO->GetOwningProject().get() == &mProject );
}

const ReservedCommandFlag&
   CanStopAudioStreamFlag(){ static ReservedCommandFlag flag{
      [](const AudacityProject &project){
         auto &projectAudioManager = ProjectAudioManager::Get( project );
         bool canStop = projectAudioManager.CanStopAudioStream();
         return canStop;
      }
   }; return flag; }

AudioIOStartStreamOptions
DefaultPlayOptions( AudacityProject &project, bool looped )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   AudioIOStartStreamOptions options { project.shared_from_this(),
      ProjectRate::Get( project ).GetRate() };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   auto timeTrack = *TrackList::Get( project ).Any<TimeTrack>().begin();
   options.envelope = timeTrack ? timeTrack->GetEnvelope() : nullptr;
   options.listener = ProjectAudioManager::Get( project ).shared_from_this();
   
   if (looped)
      options.policyFactory = []() -> std::unique_ptr<PlaybackPolicy> {
         return std::make_unique<LoopingPlaybackPolicy>(); };
   return options;
}

AudioIOStartStreamOptions
DefaultSpeedPlayOptions( AudacityProject &project )
{
   auto result = DefaultPlayOptions( project );
   auto gAudioIO = AudioIO::Get();
   auto PlayAtSpeedRate = gAudioIO->GetBestRate(
      false,     //not capturing
      true,      //is playing
      ProjectRate::Get( project ).GetRate()  //suggested rate
   );
   result.rate = PlayAtSpeedRate;
   return result;
}

TransportTracks ProjectAudioManager::GetAllPlaybackTracks(
   TrackList &trackList, bool selectedOnly, bool nonWaveToo)
{
   TransportTracks result;
   {
      auto range = trackList.Any< WaveTrack >()
         + (selectedOnly ? &Track::IsSelected : &Track::Any );
      for (auto pTrack: range)
         result.playbackTracks.push_back(
            pTrack->SharedPointer< WaveTrack >() );
   }
#ifdef EXPERIMENTAL_MIDI_OUT
   if (nonWaveToo) {
      auto range = trackList.Any< const PlayableTrack >() +
         (selectedOnly ? &Track::IsSelected : &Track::Any );
      for (auto pTrack: range)
         if (!track_cast<const WaveTrack *>(pTrack))
            result.otherPlayableTracks.push_back(
               pTrack->SharedPointer< const PlayableTrack >() );
   }
#else
   WXUNUSED(useMidi);
#endif
   return result;
}

// Stop playing or recording, if paused.
void ProjectAudioManager::StopIfPaused()
{
   if( AudioIOBase::Get()->IsPaused() )
      Stop();
}

bool ProjectAudioManager::DoPlayStopSelect( bool click, bool shift )
{
   auto &project = mProject;
   auto &scrubber = Scrubber::Get( project );
   auto token = ProjectAudioIO::Get( project ).GetAudioIOToken();
   auto &viewInfo = ViewInfo::Get( project );
   auto &selection = viewInfo.selectedRegion;
   auto gAudioIO = AudioIO::Get();

   //If busy, stop playing, make sure everything is unpaused.
   if (scrubber.HasMark() ||
       gAudioIO->IsStreamActive(token)) {
      // change the selection
      auto time = gAudioIO->GetStreamTime();
      // Test WasSpeedPlaying(), not IsSpeedPlaying()
      // as we could be stopped now. Similarly WasKeyboardScrubbing().
      if (click && (scrubber.WasSpeedPlaying() || scrubber.WasKeyboardScrubbing()))
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
void ProjectAudioManager::DoPlayStopSelect()
{
   auto gAudioIO = AudioIO::Get();
   if (DoPlayStopSelect(false, false))
      Stop();
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)

      // Will automatically set mLastPlayMode
      PlayCurrentRegion(false);
   }
}

#include "CommonCommandFlags.h"

static RegisteredMenuItemEnabler stopIfPaused{{
   []{ return PausedFlag(); },
   []{ return AudioIONotBusyFlag(); },
   []( const AudacityProject &project ){
      return MenuManager::Get( project ).mStopIfWasPaused; },
   []( AudacityProject &project, CommandFlag ){
      if ( MenuManager::Get( project ).mStopIfWasPaused )
         ProjectAudioManager::Get( project ).StopIfPaused();
   }
}};

// GetSelectedProperties collects information about 
// currently selected audio tracks
PropertiesOfSelected
GetPropertiesOfSelected(const AudacityProject &proj)
{
   double rateOfSelection{ RATE_NOT_SELECTED };

   PropertiesOfSelected result;
   result.allSameRate = true;

   const auto selectedTracks{ 
      TrackList::Get(proj).Selected< const WaveTrack >() };

   for (const auto & track : selectedTracks) 
   {
      if (rateOfSelection != RATE_NOT_SELECTED &&
         track->GetRate() != rateOfSelection)
         result.allSameRate = false;
      else if (rateOfSelection == RATE_NOT_SELECTED)
         rateOfSelection = track->GetRate();
   }

   result.numberOfSelected = selectedTracks.size();
   result.rateOfSelected = rateOfSelection;

   return  result;
}
