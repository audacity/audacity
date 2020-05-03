/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "Scrubbing.h"

#include <functional>

#include "../../AudioIO.h"
#include "../../CommonCommandFlags.h"
#include "Project.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "../../ProjectWindows.h"
#include "ProjectStatus.h"
#include "../../ScrubState.h"
#include "Track.h"
#include "ViewInfo.h"
#include "../../WaveTrack.h"
#include "../../prefs/PlaybackPrefs.h"
#include "../../prefs/TracksPrefs.h"
#include "../../toolbars/ToolManager.h"
#include "../../widgets/BasicMenu.h"

#undef USE_TRANSCRIPTION_TOOLBAR


#include <algorithm>

#include <wx/app.h>

// Yet another experimental scrub would drag the track under a
// stationary play head
#undef DRAG_SCRUB

enum {
   // PRL:
   // Mouse must move at least this far to distinguish ctrl-drag to scrub
   // from ctrl+click for playback.
   SCRUBBING_PIXEL_TOLERANCE = 10,

#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   ScrubSpeedStepsPerOctave = 4,
#endif

   kOneSecondCountdown =
      1000 / std::chrono::milliseconds{ScrubPollInterval}.count(),
};

static constexpr PlaybackPolicy::Duration MinStutter{0.2};
// static const double MaxDragSpeed = 1.0;

namespace {
   double FindScrubbingSpeed(const ViewInfo &viewInfo, double maxScrubSpeed, double screen, double timeAtMouse)
   {
      // Map a time (which was mapped from a mouse position)
      // to a speed.
      // Map times to positive and negative speeds,
      // with the time at the midline of the screen mapping to 0,
      // and the extremes to the maximum scrub speed.

      auto partScreen = screen * TracksPrefs::GetPinnedHeadPositionPreference();
      const double origin = viewInfo.h + partScreen;
      if (timeAtMouse >= origin)
         partScreen = screen - partScreen;

      // There are various snapping zones that are this fraction of screen:
      const double snap = 0.05;

      // By shrinking denom a bit, we make margins left and right
      // that snap to maximum and negative maximum speeds.
      const double factor = 1.0 - (snap * 2);
      const double denom = factor * partScreen;
      double fraction = (denom <= 0.0) ? 0.0 :
         std::min(1.0, fabs(timeAtMouse - origin) / denom);

      // Snap to 1.0 and -1.0
      const double unity = 1.0 / maxScrubSpeed;
      const double tolerance = snap / factor;
      // Make speeds near 1 available too by remapping fractions outside
      // this snap zone
      if (fraction <= unity - tolerance)
         fraction *= unity / (unity - tolerance);
      else if (fraction < unity + tolerance)
         fraction = unity;
      else
         fraction = unity + (fraction - (unity + tolerance)) *
         (1.0 - unity) / (1.0 - (unity + tolerance));

      double result = fraction * maxScrubSpeed;
      if (timeAtMouse < origin)
         result *= -1.0;
      return result;
   }

   double FindSeekSpeed(const ViewInfo &viewInfo, double maxScrubSpeed, double screen, double timeAtMouse)
   {
      // Map a time (which was mapped from a mouse position)
      // to a signed skip speed: a multiplier of the stutter duration,
      // by which to advance the play position.
      // (The stutter will play at unit speed.)

      // Times near the midline of the screen map to skip-less play,
      // and the extremes to a value proportional to maximum scrub speed.

      // If the maximum scrubbing speed defaults to 1.0 when you begin to scroll-scrub,
      // the extreme skipping for scroll-seek needs to be larger to be useful.
      static const double ARBITRARY_MULTIPLIER = 10.0;
      const double extreme = std::max(1.0, maxScrubSpeed * ARBITRARY_MULTIPLIER);

      // Width of visible track area, in time terms:
      auto partScreen = screen * TracksPrefs::GetPinnedHeadPositionPreference();
      const double origin = viewInfo.h + partScreen;
      if (timeAtMouse >= origin)
         partScreen = screen - partScreen;

      // The snapping zone is this fraction of screen, on each side of the
      // center line:
      const double snap = 0.05;
      const double fraction = (partScreen <= 0.0) ? 0.0 :
         std::max(snap, std::min(1.0, fabs(timeAtMouse - origin) / partScreen));

      double result = 1.0 + ((fraction - snap) / (1.0 - snap)) * (extreme - 1.0);
      if (timeAtMouse < origin)
         result *= -1.0;
      return result;
   }
}

#ifdef USE_SCRUB_THREAD

void Scrubber::ScrubPollerThread()
{
   while (!mFinishThread.load(std::memory_order_acquire)) {
      std::this_thread::sleep_for(ScrubPollInterval);
      ContinueScrubbingPoll();
   }
}

#endif

bool Scrubber::ShouldScrubPinned()
{
   return TracksPrefs::GetPinnedHeadPreference() &&
      !PlaybackPrefs::GetUnpinnedScrubbingPreference();
}

class Scrubber::ScrubPoller : public wxTimer
{
public:
   ScrubPoller(Scrubber &scrubber) : mScrubber( scrubber ) {}

private:
   void Notify() override;

   Scrubber &mScrubber;
};

void Scrubber::ScrubPoller::Notify()
{
   // Call Continue functions here in a timer handler
   // rather than in SelectionHandleDrag()
   // so that even without drag events, we can instruct the play head to
   // keep approaching the mouse cursor, when its maximum speed is limited.

#ifndef USE_SCRUB_THREAD
   // If there is no helper thread, this main thread timer is responsible
   // for playback and for UI
   mScrubber.ContinueScrubbingPoll();
#endif
   mScrubber.ContinueScrubbingUI();
}

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &parent ){
     return std::make_shared< Scrubber >( &parent ); }
};

Scrubber &Scrubber::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< Scrubber >( key );
}

const Scrubber &Scrubber::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

Scrubber::Scrubber(AudacityProject *project)
   : mScrubToken(-1)
   , mScrubSpeedDisplayCountdown(0)
   , mScrubStartPosition(-1)
   , mSmoothScrollingScrub(false)
   , mPaused(true)
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   , mLogMaxScrubSpeed(0)
#endif

   , mProject(project)
   , mPoller { std::make_unique<ScrubPoller>(*this) }
   , mOptions {}

{
   if (wxTheApp)
      wxTheApp->Bind
      (wxEVT_ACTIVATE_APP,
       &Scrubber::OnActivateOrDeactivateApp, this);

   UpdatePrefs();
}

void Scrubber::JoinThread()
{
#ifdef USE_SCRUB_THREAD
   if (mThread.joinable()) {
      mFinishThread.store(true, std::memory_order_release);
      mThread.join();
   }
#endif
}

Scrubber::~Scrubber()
{
   JoinThread();
}

static const auto HasWaveDataPred =
   [](const AudacityProject &project){
      auto range = TrackList::Get( project ).Any<const WaveTrack>()
         + [](const WaveTrack *pTrack){
            return pTrack->GetEndTime() > pTrack->GetStartTime();
         };
      return !range.empty();
   };

static const ReservedCommandFlag
&HasWaveDataFlag() { static ReservedCommandFlag flag{
   HasWaveDataPred
}; return flag; } // jkc

namespace {
   struct MenuItem {
      CommandID name;
      TranslatableString label;
      TranslatableString status;
      CommandFlag flags;
      void (Scrubber::*memFn)(const CommandContext&);
      bool seek;
      bool (Scrubber::*StatusTest)() const;

      const TranslatableString &GetStatus() const { return status; }
   };
   using MenuItems = std::vector< MenuItem >;
   const MenuItems &menuItems()
   {
      static MenuItems theItems{
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
         "Scrubbing" is variable-speed playback, ...
         "Seeking" is normal speed playback but with skips, ...
       */
      { wxT("Scrub"),       XXO("&Scrub"),           XO("Scrubbing"),
         CaptureNotBusyFlag() | HasWaveDataFlag(),
         &Scrubber::OnScrub,       false,      &Scrubber::Scrubs,
      },

      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
         "Scrubbing" is variable-speed playback, ...
         "Seeking" is normal speed playback but with skips, ...
       */
      { wxT("Seek"),        XXO("See&k"),            XO("Seeking"),
         CaptureNotBusyFlag() | HasWaveDataFlag(),
         &Scrubber::OnSeek,        true,       &Scrubber::Seeks,
      },

      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
         "Scrubbing" is variable-speed playback, ...
         "Seeking" is normal speed playback but with skips, ...
       */
      { wxT("ToggleScrubRuler"),            XXO("Scrub &Ruler"),   {},
         AlwaysEnabledFlag,
         &Scrubber::OnToggleScrubRuler, false,    &Scrubber::ShowsBar,
      },
      };
      return theItems;
   };

   inline const MenuItem &FindMenuItem(bool seek)
   {
      return *std::find_if(menuItems().begin(), menuItems().end(),
         [=](const MenuItem &item) {
            return seek == item.seek;
         }
      );
   }

}

void Scrubber::MarkScrubStart(
   // Assume xx is relative to the left edge of TrackPanel!
   wxCoord xx, bool smoothScrolling, bool seek
)
{
   // Don't actually start scrubbing, but collect some information
   // needed for the decision to start scrubbing later when handling
   // drag events.
   mSmoothScrollingScrub  = smoothScrolling;

   auto &projectAudioManager = ProjectAudioManager::Get( *mProject );

   // Stop any play in progress
   // Bug 1492: mCancelled to stop us collapsing the selected region.
   mCancelled = true;
   projectAudioManager.Stop();
   mCancelled = false;

   // Usually the timer handler of TrackPanel does this, but we do this now,
   // so that same timer does not StopPlaying() again after this function and destroy
   // scrubber state
   ProjectAudioIO::Get( *mProject ).SetAudioIOToken(0);

   mSeeking = seek;
   CheckMenuItems();

   // Commented out for Bug 1421
   //   mSeeking
   //   ? ControlToolBar::PlayAppearance::Seek
   //   : ControlToolBar::PlayAppearance::Scrub);

   mScrubStartPosition = xx;
   mCancelled = false;
}

static AudioIOStartStreamOptions::PolicyFactory
ScrubbingPlaybackPolicyFactory(const ScrubbingOptions &options)
{
   return [options](auto&) -> std::unique_ptr<PlaybackPolicy>
   {
      return std::make_unique<ScrubbingPlaybackPolicy>(options);
   };
}


#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
// Assume xx is relative to the left edge of TrackPanel!
bool Scrubber::MaybeStartScrubbing(wxCoord xx)
{
   if (mScrubStartPosition < 0)
      return false;
   if (IsScrubbing())
      return false;
#ifdef USE_SCRUB_THREAD
   if (mThread.joinable())
      return false;
#endif
   else {
      const auto state = ::wxGetMouseState();
      mDragging = state.LeftIsDown();

      auto gAudioIO = AudioIO::Get();
      const bool busy = gAudioIO->IsBusy();
      if (busy && gAudioIO->GetNumCaptureChannels() > 0) {
         // Do not stop recording, and don't try to start scrubbing after
         // recording stops
         mScrubStartPosition = -1;
         return false;
      }

      wxCoord position = xx;
      if (abs(mScrubStartPosition - position) >= SCRUBBING_PIXEL_TOLERANCE) {
         auto &viewInfo = ViewInfo::Get( *mProject );
         auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
         double maxTime = TrackList::Get( *mProject ).GetEndTime();
         const int leftOffset = viewInfo.GetLeftOffset();
         double time0 = std::min(maxTime,
            viewInfo.PositionToTime(mScrubStartPosition, leftOffset)
         );
         double time1 = std::min(maxTime,
            viewInfo.PositionToTime(position, leftOffset)
         );
         if (time1 != time0) {
            if (busy) {
               position = mScrubStartPosition;
               projectAudioManager.Stop();
               mScrubStartPosition = position;
            }

#ifdef DRAG_SCRUB
            if (mDragging && mSmoothScrollingScrub) {
               auto delta = time0 - time1;
               time0 = std::max(0.0, std::min(maxTime,
                  viewInfo.h +
                     (viewInfo.GetScreenEndTime() - viewInfo.h)
                        * TracksPrefs::GetPinnedHeadPositionPreference()
               ));
               time1 = time0 + delta;
            }
#endif
            mSpeedPlaying = false;
            mKeyboardScrubbing = false;
            auto options =
               DefaultPlayOptions( *mProject );

#ifndef USE_SCRUB_THREAD
            // Yuck, we either have to poll "by hand" when scrub polling doesn't
            // work with a thread, or else yield to timer messages, but that would
            // execute too much else
            options.playbackStreamPrimer = [this](){
               ContinueScrubbingPoll();
               return ScrubPollInterval;
            };
#endif
            options.playNonWaveTracks = false;
            options.envelope = nullptr;
            mOptions.delay = ScrubPollInterval;
            mOptions.isKeyboardScrubbing = false;
            mOptions.initSpeed = 0;
            mOptions.minSpeed = 0.0;
#ifdef USE_TRANSCRIPTION_TOOLBAR
            if (!mAlwaysSeeking) {
               // Take the starting speed limit from the transcription toolbar,
               // but it may be varied during the scrub.
               mMaxSpeed = mOptions.maxSpeed =
                  ProjectSettings::Get( *mProject ).GetPlaySpeed();
            }
#else
            // That idea seems unpopular... just make it one for move-scrub,
            // but big for drag-scrub
#ifdef DRAG_SCRUB
            mMaxSpeed = mOptions.maxSpeed = mDragging ? MaxDragSpeed : 1.0;
#else
            mMaxSpeed = mOptions.maxSpeed = 1.0;
#endif

#endif
            mOptions.minTime = 0;
            mOptions.maxTime =
               std::max(0.0, TrackList::Get( *mProject ).GetEndTime());
            mOptions.minStutterTime =
#ifdef DRAG_SCRUB
               mDragging ? PlaybackPolicy::Duration{} :
#endif
               std::max(PlaybackPolicy::Duration{}, MinStutter);

            const bool backwards = time1 < time0;
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
            static const double maxScrubSpeedBase =
               pow(2.0, 1.0 / ScrubSpeedStepsPerOctave);
            mLogMaxScrubSpeed = floor(0.5 +
               log(mMaxSpeed) / log(maxScrubSpeedBase)
            );
#endif
            mScrubSpeedDisplayCountdown = 0;

            // Must start the thread and poller first or else PlayPlayRegion
            // will insert some silence
            StartPolling();
            auto cleanup = finally([this]{
               if (mScrubToken < 0)
                  StopPolling();
            });

            options.policyFactory = ScrubbingPlaybackPolicyFactory(mOptions);
            mScrubToken =
               projectAudioManager.PlayPlayRegion(
                  SelectedRegion(time0, time1), options,
                  PlayMode::normalPlay, backwards);
            if (mScrubToken <= 0) {
               // Bug1627 (part of it):
               // infinite error spew when trying to start scrub:
               // If failed for reasons of audio device problems, do not try
               // again with repeated timer ticks.
               mScrubStartPosition = -1;
               return false;
            }
         }
      }
      else
         // Wait to test again
         ;

      if (IsScrubbing()) {
         mLastScrubPosition = xx;
      }

      // Return true whether we started scrub, or are still waiting to decide.
      return true;
   }
}

bool Scrubber::StartKeyboardScrubbing(double time0, bool backwards)
{
   if (HasMark() || AudioIO::Get()->IsBusy())
      return false;
#ifdef USE_SCRUB_THREAD
   if (mThread.joinable())
      return false;
#endif
   
   mScrubStartPosition = 0;   // so that HasMark() is true
   mSpeedPlaying = false;
   mKeyboardScrubbing = true;
   mBackwards = backwards;
   mMaxSpeed = ScrubbingOptions::MaxAllowedScrubSpeed();
   mDragging = false;

   auto options = DefaultSpeedPlayOptions(*mProject);

#ifndef USE_SCRUB_THREAD
   // Yuck, we either have to poll "by hand" when scrub polling doesn't
   // work with a thread, or else yield to timer messages, but that would
   // execute too much else
   options.playbackStreamPrimer = [this]() {
      ContinueScrubbingPoll();
      return ScrubPollInterval;
   };
#endif

   options.playNonWaveTracks = false;
   options.envelope = nullptr;

   // delay and minStutterTime are used in AudioIO::AllocateBuffers() for setting the
   // values of mPlaybackQueueMinimum and mPlaybackSamplesToCopy respectively.
   mOptions.delay = ScrubPollInterval;
   mOptions.minStutterTime = mOptions.delay;

   mOptions.initSpeed = GetKeyboardScrubbingSpeed();
   if (backwards)
      mOptions.initSpeed *= -1.0;
   mOptions.minSpeed = ScrubbingOptions::MinAllowedScrubSpeed();
   mOptions.maxSpeed = ScrubbingOptions::MaxAllowedScrubSpeed();
   mOptions.minTime = 0;
   mOptions.maxTime = std::max(0.0, TrackList::Get(*mProject).GetEndTime());
   mOptions.bySpeed = true;
   mOptions.adjustStart = false;
   mOptions.isKeyboardScrubbing = true;

   // Must start the thread and poller first or else PlayPlayRegion
   // will insert some silence
   StartPolling();
   auto cleanup = finally([this] {
      if (mScrubToken < 0)
         StopPolling();
   });

   options.policyFactory = ScrubbingPlaybackPolicyFactory(mOptions);
   mScrubToken =
      ProjectAudioManager::Get(*mProject).PlayPlayRegion(
         SelectedRegion(time0, backwards ? mOptions.minTime : mOptions.maxTime),
         options,
         PlayMode::normalPlay,
         backwards);

   return true;
}


double Scrubber::GetKeyboardScrubbingSpeed()
{
   const double speedAtDefaultZoom = 0.5;
   const double maxSpeed = 3.0;
   const double minSpeed = 0.0625;

   auto &viewInfo = ViewInfo::Get(*mProject);
   double speed = speedAtDefaultZoom*viewInfo.GetDefaultZoom() / viewInfo.GetZoom();
   speed = std::min(speed, maxSpeed);
   speed = std::max(speed, minSpeed);
   return speed;
}


void Scrubber::ContinueScrubbingPoll()
{
   // Thus scrubbing relies mostly on periodic polling of mouse and keys,
   // not event notifications.  But there are a few event handlers that
   // leave messages for this routine, in mScrubSeekPress and in mPaused.

   // Decide whether to skip play, because either mouse is down now,
   // or there was a left click event.  (This is then a delayed reaction, in a
   // timer callback, to a left click event detected elsewhere.)
   const bool seek = TemporarilySeeks() || Seeks();

   auto gAudioIO = AudioIO::Get();
   if (mPaused) {
      // When paused, make silent scrubs.
      mOptions.minSpeed = 0.0;
      mOptions.maxSpeed = mMaxSpeed;
      mOptions.adjustStart = false;
      mOptions.bySpeed = true;
      ScrubState::UpdateScrub(0, mOptions);
   }
   else if (mSpeedPlaying) {
      // default speed of 1.3 set, so that we can hear there is a problem
      // when playAtSpeedTB not found.
      double speed = 1.3;
      const auto &projectAudioIO = ProjectAudioIO::Get( *mProject );
      speed = projectAudioIO.GetPlaySpeed();
      mOptions.minSpeed = speed -0.01;
      mOptions.maxSpeed = speed +0.01;
      mOptions.adjustStart = false;
      mOptions.bySpeed = true;
      ScrubState::UpdateScrub(speed, mOptions);
   }
   else if (mKeyboardScrubbing) {
      mOptions.minSpeed = ScrubbingOptions::MinAllowedScrubSpeed();
      mOptions.maxSpeed = ScrubbingOptions::MaxAllowedScrubSpeed();
      mOptions.adjustStart = false;
      mOptions.bySpeed = true;
      double speed = GetKeyboardScrubbingSpeed();
      if (mBackwards)
         speed *= -1.0;
      ScrubState::UpdateScrub(speed, mOptions);
   } else {
      const wxMouseState state(::wxGetMouseState());
      auto &trackPanel = GetProjectPanel( *mProject );
      const wxPoint position = trackPanel.ScreenToClient(state.GetPosition());
      auto &viewInfo = ViewInfo::Get( *mProject );
#ifdef DRAG_SCRUB
      if (mDragging && mSmoothScrollingScrub) {
         const auto lastTime = ScrubState::GetLastScrubTime();
         const auto delta = mLastScrubPosition - position.x;
         const double time = viewInfo.OffsetTimeByPixels(lastTime, delta);
         mOptions.minSpeed = 0.0;
         mOptions.maxSpeed = mMaxSpeed;
         mOptions.adjustStart = true;
         mOptions.bySpeed = false;
         gAudioIO->UpdateScrub(time, mOptions);
         mLastScrubPosition = position.x;
      }
      else
#endif
      {
         const auto origin = viewInfo.GetLeftOffset();
         auto xx = position.x;
         if (!seek && !mSmoothScrollingScrub) {
            // If mouse is out-of-bounds, so that we scrub at maximum speed
            // toward the mouse position, then move the target time to a more
            // extreme position to avoid catching-up and halting before the
            // screen scrolls.
            auto width = viewInfo.GetTracksUsableWidth();
            auto delta = xx - origin;
            if (delta < 0)
               delta -= width;
            else if (delta >= width)
               delta += width;
            xx = origin + delta;
         }
         const double time = viewInfo.PositionToTime(xx, origin);
         mOptions.adjustStart = seek;
         mOptions.minSpeed = seek ? 1.0 : 0.0;
         mOptions.maxSpeed = seek ? 1.0 : mMaxSpeed;

         if (mSmoothScrollingScrub) {
            const double speed = FindScrubSpeed(seek, time);
            mOptions.bySpeed = true;
            ScrubState::UpdateScrub(speed, mOptions);
         }
         else {
            mOptions.bySpeed = false;
            ScrubState::UpdateScrub(time, mOptions);
         }
      }
   }

   mScrubSeekPress = false;

   // else, if seek requested, try again at a later time when we might
   // enqueue a long enough stutter
}

void Scrubber::ContinueScrubbingUI()
{
   const wxMouseState state(::wxGetMouseState());

   if (mDragging && !state.LeftIsDown()) {
      // Dragging scrub can stop with mouse up
      // Stop and set cursor
      bool bShift = state.ShiftDown();
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.DoPlayStopSelect( true, bShift );
      projectAudioManager.Stop();
      return;
   }

   const bool seek = Seeks() || TemporarilySeeks();

   {
      // Show the correct status for seeking.
      bool backup = mSeeking;
      mSeeking = seek;
      mSeeking = backup;
   }

   if (seek)
      mScrubSpeedDisplayCountdown = 0;

   if (mSmoothScrollingScrub)
      ;
   else {
      if (mScrubSpeedDisplayCountdown > 0)
         --mScrubSpeedDisplayCountdown;
   }
}

bool Scrubber::IsTransportingPinned() const
{
   if (!TracksPrefs::GetPinnedHeadPreference())
      return false;
   return
     !(HasMark() &&
       !WasSpeedPlaying() &&
       !ShouldScrubPinned());
}

void Scrubber::StartPolling()
{
   mPaused = false;
   
#ifdef USE_SCRUB_THREAD
   assert(!mThread.joinable());
   mFinishThread.store(false, std::memory_order_relaxed);
   mThread = std::thread{
      std::mem_fn( &Scrubber::ScrubPollerThread ), std::ref(*this) };
#endif

   mPoller->Start( 0.9 *
      std::chrono::duration<double, std::milli>{ScrubPollInterval}.count());
}

void Scrubber::StopPolling()
{
   mPaused = true;

#ifdef USE_SCRUB_THREAD
   JoinThread();
#endif
   
   mPoller->Stop();
}

void Scrubber::StopScrubbing()
{
   auto gAudioIO = AudioIO::Get();
   ScrubState::StopScrub();
   StopPolling();

   if (HasMark() && !mCancelled) {
      const wxMouseState state(::wxGetMouseState());
      // Stop and set cursor
      bool bShift = state.ShiftDown();
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.DoPlayStopSelect(true, bShift);
   }

   mScrubStartPosition = -1;
   mDragging = false;
   mSeeking = false;

   CheckMenuItems();
}

bool Scrubber::ShowsBar() const
{
   return mShowScrubbing;
}

bool Scrubber::IsScrubbing() const
{
   if (mScrubToken <= 0)
      return false;
   auto &projectAudioIO = ProjectAudioIO::Get( *mProject );
   if (mScrubToken == projectAudioIO.GetAudioIOToken() &&
            projectAudioIO.IsAudioActive())
      return true;
   else {
      const_cast<Scrubber&>(*this).mScrubToken = -1;
      const_cast<Scrubber&>(*this).mScrubStartPosition = -1;
      const_cast<Scrubber&>(*this).mSmoothScrollingScrub = false;
      return false;
   }
}

bool Scrubber::ChoseSeeking() const
{
   return
#if !defined(DRAG_SCRUB)
      // Drag always seeks
      mDragging ||
#endif
      mSeeking;
}

bool Scrubber::TemporarilySeeks() const
{
   return mScrubSeekPress ||
      (::wxGetMouseState().LeftIsDown() && MayDragToSeek());
}

bool Scrubber::Seeks() const
{
   return (HasMark() || IsScrubbing()) && ChoseSeeking();
}

bool Scrubber::Scrubs() const
{
   if( Seeks() )
      return false;
   return (HasMark() || IsScrubbing()) && !ChoseSeeking();
}

bool Scrubber::ShouldDrawScrubSpeed()
{
   return IsScrubbing() &&
      !mPaused && (
         // Draw for (non-scroll) scrub, sometimes, but never for seek
         (!(Seeks() || TemporarilySeeks()) && mScrubSpeedDisplayCountdown > 0)
         // Draw always for scroll-scrub and for scroll-seek
         || mSmoothScrollingScrub
      );
}

double Scrubber::FindScrubSpeed(bool seeking, double time) const
{
   auto &viewInfo = ViewInfo::Get( *mProject );
   const double screen =
      viewInfo.GetScreenEndTime() - viewInfo.h;
   return (seeking ? FindSeekSpeed : FindScrubbingSpeed)
      (viewInfo, mMaxSpeed, screen, time);
}

void Scrubber::HandleScrollWheel(int steps)
{
   if (steps == 0)
      return;

   const int newLogMaxScrubSpeed = mLogMaxScrubSpeed + steps;
   static const double maxScrubSpeedBase =
      pow(2.0, 1.0 / ScrubSpeedStepsPerOctave);
   double newSpeed = pow(maxScrubSpeedBase, newLogMaxScrubSpeed);
   if (newSpeed >= ScrubbingOptions::MinAllowedScrubSpeed() &&
       newSpeed <= ScrubbingOptions::MaxAllowedScrubSpeed()) {
      mLogMaxScrubSpeed = newLogMaxScrubSpeed;
      mMaxSpeed = newSpeed;
      if (!mSmoothScrollingScrub)
         // Show the speed for one second
         mScrubSpeedDisplayCountdown = kOneSecondCountdown + 1;
   }
}

void Scrubber::Pause( bool paused )
{
   mPaused = paused;
}

bool Scrubber::IsPaused() const
{
   return mPaused;
}

void Scrubber::OnActivateOrDeactivateApp(wxActivateEvent &event)
{
   // First match priority logic...
   // Pause if Pause down, or not scrubbing.
   if (!mProject)
      Pause(true);
   else if (ProjectAudioManager::Get( *mProject ).Paused())
      Pause( true );
   else if (!IsScrubbing())
      Pause( true );

   // Stop keyboard scrubbing if losing focus
   else if (mKeyboardScrubbing && !event.GetActive()) {
      Cancel();
      ProjectAudioManager::Get(*mProject).Stop();
   }

   // Speed playing does not pause if losing focus.
   else if (mSpeedPlaying)
      Pause( false );

   // But scrub and seek do.
   else if (!event.GetActive())
      Pause( true );
   else
      Pause(false);

   event.Skip();
}

void Scrubber::DoScrub(bool seek)
{
   if( !CanScrub() )
      return;
   const bool wasScrubbing = HasMark() || IsScrubbing();
   const bool scroll = ShouldScrubPinned();
   if (!wasScrubbing) {
      auto &tp = GetProjectPanel( *mProject );
      const auto &viewInfo = ViewInfo::Get( *mProject );
      wxCoord xx = tp.ScreenToClient(::wxGetMouseState().GetPosition()).x;

      // Limit x
      auto width = viewInfo.GetTracksUsableWidth();
      const auto offset = viewInfo.GetLeftOffset();
      xx = (std::max(offset, std::min(offset + width - 1, xx)));

      MarkScrubStart(xx, scroll, seek);
   }
   else if (mSeeking != seek) {
      // just switching mode
   }
   else {
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.Stop();
   }
}

void Scrubber::OnScrubOrSeek(bool seek)
{
   DoScrub(seek);

   mSeeking = seek;
   CheckMenuItems();
}

void Scrubber::OnScrub(const CommandContext&)
{
   OnScrubOrSeek(false);
   CheckMenuItems();
}

void Scrubber::OnSeek(const CommandContext&)
{
   OnScrubOrSeek(true);
   CheckMenuItems();
}

#if 1
namespace {
   static const wxChar *scrubEnabledPrefName = wxT("/QuickPlay/ScrubbingEnabled");

   bool ReadScrubEnabledPref()
   {
      bool result {};
      gPrefs->Read(scrubEnabledPrefName, &result, false);

      return result;
   }

   void WriteScrubEnabledPref(bool value)
   {
      gPrefs->Write(scrubEnabledPrefName, value);
   }
}
#endif

void Scrubber::UpdatePrefs()
{
   mShowScrubbing = ReadScrubEnabledPref();
}

void Scrubber::OnToggleScrubRuler(const CommandContext&)
{
   mShowScrubbing = !mShowScrubbing;
   WriteScrubEnabledPref(mShowScrubbing);
   gPrefs->Flush();
   const auto toolbar =
      ToolManager::Get( *mProject ).GetToolBar( ScrubbingBarID );
   toolbar->EnableDisableButtons();
   CheckMenuItems();
}

//static_assert(menuItems().size() == 3, "wrong number of items");

static auto sPlayAtSpeedStatus = XO("Playing at Speed");

static auto sKeyboardScrubbingStatus = XO("Scrubbing");


const TranslatableString &Scrubber::GetUntranslatedStateString() const
{
   static TranslatableString empty;

   if (IsSpeedPlaying()) {
      return sPlayAtSpeedStatus;
   }
   else if (IsKeyboardScrubbing()) {
      return sKeyboardScrubbingStatus;
   }
   else if (HasMark()) {
      auto &item = FindMenuItem(Seeks() || TemporarilySeeks());
      return item.status;
   }
   else
      return empty;
}

wxString Scrubber::StatusMessageForWave() const
{
   wxString result;

   if(  Seeks() )
      result = _("Move mouse pointer to Seek");
   else if( Scrubs() )
      result = _("Move mouse pointer to Scrub");
   return result;
}



static ProjectStatus::RegisteredStatusWidthFunction
registeredStatusWidthFunction{
   []( const AudacityProject &, StatusBarField field )
      -> ProjectStatus::StatusWidthResult
   {
      if ( field == stateStatusBarField ) {
         TranslatableStrings strings;
         // Note that Scrubbing + Paused is not allowed.
         for (const auto &item : menuItems())
            strings.push_back( item.GetStatus() );
         strings.push_back(
            XO("%s Paused.").Format( sPlayAtSpeedStatus )
         );
         // added constant needed because xMax isn't large enough for some reason, plus some space.
         return { std::move( strings ), 30 };
      }
      return {};
   }
};

bool Scrubber::CanScrub() const
{
   // Recheck the same condition as enables the Scrub/Seek menu item.
   auto gAudioIO = AudioIO::Get();
   return !( gAudioIO->IsBusy() && gAudioIO->GetNumCaptureChannels() > 0 ) &&
      HasWaveDataPred( *mProject );
}

void Scrubber::DoKeyboardScrub(bool backwards, bool keyUp)
{
   auto &project = *mProject;

   static double initT0 = 0;
   static double initT1 = 0;

   if (keyUp) {
      auto &scrubber = Scrubber::Get(project);
      if (scrubber.IsKeyboardScrubbing() && scrubber.IsBackwards() == backwards) {
         auto gAudioIO = AudioIO::Get();
         auto time = gAudioIO->GetStreamTime();
         auto &viewInfo = ViewInfo::Get(project);
         auto &selection = viewInfo.selectedRegion;

         // If the time selection has not changed during scrubbing
         // set the cursor position
         if (selection.t0() == initT0 && selection.t1() == initT1) {
            double endTime = TrackList::Get(project).GetEndTime();
            time = std::min(time, endTime);
            time = std::max(time, 0.0);
            selection.setTimes(time, time);
            ProjectHistory::Get(project).ModifyState(false);
         }

         scrubber.Cancel();
         ProjectAudioManager::Get(project).Stop();
      }
   }
   else {      // KeyDown
      auto gAudioIO = AudioIOBase::Get();
      auto &scrubber = Scrubber::Get(project);
      if (scrubber.IsKeyboardScrubbing() && scrubber.IsBackwards() != backwards) {
         // change direction
         scrubber.SetBackwards(backwards);
      }
      else if (!gAudioIO->IsBusy() && !scrubber.HasMark()) {
         auto &viewInfo = ViewInfo::Get(project);
         auto &selection = viewInfo.selectedRegion;
         double endTime = TrackList::Get(project).GetEndTime();
         double t0 = selection.t0();
    
         if ((!backwards && t0 >= 0 && t0 < endTime) ||
            (backwards && t0 > 0 && t0 <= endTime)) {
            initT0 = t0;
            initT1 = selection.t1();
            scrubber.StartKeyboardScrubbing(t0, backwards);
         }
      }
   }
}

void Scrubber::OnKeyboardScrubBackwards(const CommandContext &context)
{
   auto evt = context.pEvt;
   if (evt)
      DoKeyboardScrub(true, evt->GetEventType() == wxEVT_KEY_UP);
   else {              // called from menu, so simulate keydown and keyup
      DoKeyboardScrub(true, false);
      DoKeyboardScrub(true, true);
   }
}

void Scrubber::OnKeyboardScrubForwards(const CommandContext &context)
{
   auto evt = context.pEvt;
   if (evt)
      DoKeyboardScrub(false, evt->GetEventType() == wxEVT_KEY_UP);
   else {              // called from menu, so simulate keydown and keyup
      DoKeyboardScrub(false, false);
      DoKeyboardScrub(false, true);
   }
}

namespace {

static const auto finder =
   [](AudacityProject &project) -> CommandHandlerObject&
     { return Scrubber::Get( project ); };

using namespace MenuTable;
BaseItemSharedPtr ToolbarMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu { (
   FinderScope{ finder },
   Menu( wxT("Scrubbing"),
      XXO("Scru&bbing"),
      []{
         BaseItemPtrs ptrs;
         for (const auto &item : menuItems()) {
            ptrs.push_back( Command( item.name, item.label,
               item.memFn,
               item.flags,
               item.StatusTest
                  ? // a checkmark item
                     Options{}.CheckTest( [&item](AudacityProject &project){
                     return ( Scrubber::Get(project).*(item.StatusTest) )(); } )
                  : // not a checkmark item
                     Options{}
            ) );
         }
         return ptrs;
      }()
   )
   ) };
   
   return menu;
}

AttachedItem sAttachment{
   wxT("Transport/Basic"),
   Shared( ToolbarMenu() )
};

BaseItemSharedPtr KeyboardScrubbingItems()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr items{
   ( FinderScope{ finder },
   Items( wxT("KeyboardScrubbing"),
      Command(wxT("KeyboardScrubBackwards"), XXO("Scrub Bac&kwards"),
         &Scrubber::OnKeyboardScrubBackwards,
         CaptureNotBusyFlag() | CanStopAudioStreamFlag(),
         Options{ wxT("U") }.WantKeyUp() ),
      Command(wxT("KeyboardScrubForwards"), XXO("Scrub For&wards"),
         &Scrubber::OnKeyboardScrubForwards,
         CaptureNotBusyFlag() | CanStopAudioStreamFlag(),
         Options{ wxT("I") }.WantKeyUp() )
   ) ) };
   return items;
}

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1/Transport"),
   Shared( KeyboardScrubbingItems() )
};

}

void Scrubber::PopulatePopupMenu(BasicMenu::Handle &menu)
{
   auto &cm = CommandManager::Get( *mProject );
   for (const auto &item : menuItems()) {
      if (cm.GetEnabled(item.name)) {
         auto action = [this, memFn = item.memFn] {
            (this->*memFn)(*mProject);
         };
         auto test = item.StatusTest;
         if ( test )
            menu.AppendCheckItem(
               item.label, action, { true, (this->*test)() } );
         else
            menu.Append( item.label, action  );
      }
   }
}

void Scrubber::CheckMenuItems()
{
   auto &cm = CommandManager::Get( *mProject );
   for (const auto &item : menuItems()) {
      auto test = item.StatusTest;
      if (test)
         cm.Check(item.name, (this->*test)());
   }
}

#endif
