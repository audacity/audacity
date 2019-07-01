/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "Scrubbing.h"

#include "../../Experimental.h"

#include <functional>

#include "../../AdornedRulerPanel.h"
#include "../../AudioIO.h"
#include "../../CommonCommandFlags.h"
#include "../../Menus.h"
#include "../../Project.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectAudioManager.h"
#include "../../ProjectSettings.h"
#include "../../Track.h"
#include "../../TrackPanel.h"
#include "../../ViewInfo.h"
#include "../../WaveTrack.h"
#include "../../prefs/PlaybackPrefs.h"
#include "../../prefs/TracksPrefs.h"
#include "../../toolbars/ControlToolBar.h"
#include "../../toolbars/ToolManager.h"

#undef USE_TRANSCRIPTION_TOOLBAR


#include <algorithm>

#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/menu.h>

// Yet another experimental scrub would drag the track under a
// stationary play head
#undef DRAG_SCRUB

enum {
   // PRL:
   // Mouse must move at least this far to distinguish ctrl-drag to scrub
   // from ctrl-click for playback.
   SCRUBBING_PIXEL_TOLERANCE = 10,

#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   ScrubSpeedStepsPerOctave = 4,
#endif

   kOneSecondCountdown = 1000 / ScrubPollInterval_ms,
};

static const double MinStutter = 0.2;
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

class Scrubber::ScrubPollerThread final : public wxThread {
public:
   ScrubPollerThread(Scrubber &scrubber)
      : wxThread { }
      , mScrubber(scrubber)
   {}
   ExitCode Entry() override;

private:
   Scrubber &mScrubber;
};

auto Scrubber::ScrubPollerThread::Entry() -> ExitCode
{
   while( !TestDestroy() )
   {
      wxThread::Sleep(ScrubPollInterval_ms);
      mScrubber.ContinueScrubbingPoll();
   }
   return 0;
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
   , mPaused(true)
   , mScrubSpeedDisplayCountdown(0)
   , mScrubStartPosition(-1)
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   , mSmoothScrollingScrub(false)
   , mLogMaxScrubSpeed(0)
#endif

   , mProject(project)
   , mWindow( FindProjectFrame( project ) )
   , mPoller { std::make_unique<ScrubPoller>(*this) }
   , mOptions {}

{
   if (wxTheApp)
      wxTheApp->Bind
      (wxEVT_ACTIVATE_APP,
       &Scrubber::OnActivateOrDeactivateApp, this);
   if (mWindow)
      mWindow->PushEventHandler(&mForwarder);
}

Scrubber::~Scrubber()
{
#ifdef USE_SCRUB_THREAD
   if (mpThread)
      mpThread->Delete();
#endif

   if ( mWindow )
      mWindow->PopEventHandler();
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
   HasWaveDataFlag{ HasWaveDataPred }; // jkc

namespace {
   const struct MenuItem {
      CommandID name;
      wxString label;
      wxString status;
      CommandFlag flags;
      void (Scrubber::*memFn)(const CommandContext&);
      bool seek;
      bool (Scrubber::*StatusTest)() const;

      const wxString &GetStatus() const { return status; }
   } menuItems[] = {
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
         "Scrubbing" is variable-speed playback, ...
         "Seeking" is normal speed playback but with skips, ...
       */
      { wxT("Scrub"),       XO("&Scrub"),           XO("Scrubbing"),
         CaptureNotBusyFlag | HasWaveDataFlag,
         &Scrubber::OnScrub,       false,      &Scrubber::Scrubs,
      },

      { wxT("Seek"),        XO("See&k"),            XO("Seeking"),
         CaptureNotBusyFlag | HasWaveDataFlag,
         &Scrubber::OnSeek,        true,       &Scrubber::Seeks,
      },

      { wxT("ToggleScrubRuler"),            XO("Scrub &Ruler"),   wxT(""),
         AlwaysEnabledFlag,
         &Scrubber::OnToggleScrubRuler, false,    &Scrubber::ShowsBar,
      },
   };

   enum { nMenuItems = sizeof(menuItems) / sizeof(*menuItems) };

   inline const MenuItem &FindMenuItem(bool seek)
   {
      return *std::find_if(menuItems, menuItems + nMenuItems,
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

   auto &ctb = ControlToolBar::Get( *mProject );

   // Stop any play in progress
   // Bug 1492: mCancelled to stop us collapsing the selected region.
   mCancelled = true;
   ctb.StopPlaying();
   mCancelled = false;

   // Usually the timer handler of TrackPanel does this, but we do this now,
   // so that same timer does not StopPlaying() again after this function and destroy
   // scrubber state
   ProjectAudioIO::Get( *mProject ).SetAudioIOToken(0);

   mSeeking = seek;
   CheckMenuItems();

   ctb.SetPlay(true, ControlToolBar::PlayAppearance::Straight );
   // Commented out for Bug 1421
   //   mSeeking
   //   ? ControlToolBar::PlayAppearance::Seek
   //   : ControlToolBar::PlayAppearance::Scrub);

   mScrubStartPosition = xx;
   mCancelled = false;
}

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
// Assume xx is relative to the left edge of TrackPanel!
bool Scrubber::MaybeStartScrubbing(wxCoord xx)
{
   if (mScrubStartPosition < 0)
      return false;
   if (IsScrubbing())
      return false;
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
         auto &ctb = ControlToolBar::Get( *mProject );
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
               ctb.StopPlaying();
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
            auto options =
               DefaultPlayOptions( *mProject );

#ifndef USE_SCRUB_THREAD
            // Yuck, we either have to poll "by hand" when scrub polling doesn't
            // work with a thread, or else yield to timer messages, but that would
            // execute too much else
            options.playbackStreamPrimer = [this](){
               ContinueScrubbingPoll();
               return ScrubPollInterval_ms;
            };
#endif
            options.pScrubbingOptions = &mOptions;
            options.envelope = nullptr;
            mOptions.delay = (ScrubPollInterval_ms / 1000.0);
            mOptions.isPlayingAtSpeed = false;
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
               mDragging ? 0.0 :
#endif
               std::max(0.0, MinStutter);

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

            mScrubToken =
               ctb.PlayPlayRegion(SelectedRegion(time0, time1), options,
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



bool Scrubber::StartSpeedPlay(double speed, double time0, double time1)
{
   if (IsScrubbing())
      return false;

   auto gAudioIO = AudioIO::Get();
   const bool busy = gAudioIO->IsBusy();
   if (busy && gAudioIO->GetNumCaptureChannels() > 0) {
      // Do not stop recording, and don't try to start scrubbing after
      // recording stops
      mScrubStartPosition = -1;
      return false;
   }

   auto &ctb = ControlToolBar::Get( *mProject );
   if (busy) {
      ctb.StopPlaying();
   }
   mScrubStartPosition = 0;
   mSpeedPlaying = true;
   mMaxSpeed = speed;
   mDragging = false;

   auto options = DefaultSpeedPlayOptions( *mProject );

#ifndef USE_SCRUB_THREAD
            // Yuck, we either have to poll "by hand" when scrub polling doesn't
            // work with a thread, or else yield to timer messages, but that would
            // execute too much else
            options.playbackStreamPrimer = [this](){
               ContinueScrubbingPoll();
               return ScrubPollInterval_ms;
            };
#endif

   options.pScrubbingOptions = &mOptions;
   options.envelope = nullptr;
   mOptions.delay = (ScrubPollInterval_ms / 1000.0);
   mOptions.minSpeed = speed -0.01;
   mOptions.maxSpeed = speed +0.01;

   if (time1 == time0)
      time1 = std::max(0.0, TrackList::Get( *mProject ).GetEndTime());
   mOptions.minTime = 0;
   mOptions.maxTime = time1;
   mOptions.minStutterTime = std::max(0.0, MinStutter);
   mOptions.bySpeed = true;
   mOptions.adjustStart = false;
   mOptions.isPlayingAtSpeed = true;
      
   const bool backwards = time1 < time0;
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   static const double maxScrubSpeedBase =
      pow(2.0, 1.0 / ScrubSpeedStepsPerOctave);
   mLogMaxScrubSpeed = floor(0.5 +
      log(mMaxSpeed) / log(maxScrubSpeedBase)
   );
#endif

   // Must start the thread and poller first or else PlayPlayRegion
   // will insert some silence
   StartPolling();
   auto cleanup = finally([this]{
      if (mScrubToken < 0)
         StopPolling();
   });
   
   mScrubSpeedDisplayCountdown = 0;
   // Aim to stop within 20 samples of correct position.
   double stopTolerance = 20.0 / options.rate;
   mScrubToken =
      // Reduce time by 'stopTolerance' fudge factor, so that the Play will stop.
      ctb.PlayPlayRegion(SelectedRegion(time0, time1-stopTolerance), options,
         PlayMode::normalPlay, backwards);

   if (mScrubToken >= 0) {
      mLastScrubPosition = 0;
   }

   return true;
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
      gAudioIO->UpdateScrub(0, mOptions);
   }
   else if (mSpeedPlaying) {
      // default speed of 1.3 set, so that we can hear there is a problem
      // when playAtSpeedTB not found.
      double speed = 1.3;
      const auto &settings = ProjectSettings::Get( *mProject );
      speed = settings.GetPlaySpeed();
      mOptions.minSpeed = speed -0.01;
      mOptions.maxSpeed = speed +0.01;
      mOptions.adjustStart = false;
      mOptions.bySpeed = true;
      gAudioIO->UpdateScrub(speed, mOptions);
   } else {
      const wxMouseState state(::wxGetMouseState());
      auto &trackPanel = TrackPanel::Get( *mProject );
      const wxPoint position = trackPanel.ScreenToClient(state.GetPosition());
      auto &viewInfo = ViewInfo::Get( *mProject );
#ifdef DRAG_SCRUB
      if (mDragging && mSmoothScrollingScrub) {
         const auto lastTime = gAudioIO->GetLastScrubTime();
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
            gAudioIO->UpdateScrub(speed, mOptions);
         }
         else {
            mOptions.bySpeed = false;
            gAudioIO->UpdateScrub(time, mOptions);
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
      TransportActions::DoPlayStopSelect(*mProject, true, bShift);
      wxCommandEvent evt;
      ControlToolBar::Get( *mProject ).OnStop(evt);
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

void Scrubber::StartPolling()
{
   mPaused = false;
   
#ifdef USE_SCRUB_THREAD
   // Detached thread is self-deleting, after it receives the Delete() message
   mpThread = safenew ScrubPollerThread{ *this };
   mpThread->Create(4096);
   mpThread->Run();
#endif
   
   mPoller->Start(ScrubPollInterval_ms * 0.9);
}

void Scrubber::StopPolling()
{
   mPaused = true;

#ifdef USE_SCRUB_THREAD
   if (mpThread) {
      mpThread->Delete();
      mpThread = nullptr;
   }
#endif
   
   mPoller->Stop();
}

void Scrubber::StopScrubbing()
{
   auto gAudioIO = AudioIO::Get();
   gAudioIO->StopScrub();
   StopPolling();

   if (HasMark() && !mCancelled) {
      const wxMouseState state(::wxGetMouseState());
      // Stop and set cursor
      bool bShift = state.ShiftDown();
      TransportActions::DoPlayStopSelect(*mProject, true, bShift);
   }

   mScrubStartPosition = -1;
   mDragging = false;
   mSeeking = false;

   if (!IsScrubbing())
   {
      // Marked scrub start, but
      // didn't really play, but did change button apperance
      auto &ctb = ControlToolBar::Get( *mProject );
      ctb.SetPlay(false, ControlToolBar::PlayAppearance::Straight);
   }

   AdornedRulerPanel::Get( *mProject ).DrawBothOverlays();
   CheckMenuItems();
}

bool Scrubber::ShowsBar() const
{
   return AdornedRulerPanel::Get( *mProject ).ShowingScrubRuler();
}

bool Scrubber::IsScrubbing() const
{
   if (mScrubToken <= 0)
      return false;
   auto projectAudioIO = ProjectAudioIO::Get( *mProject );
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

bool Scrubber::MayDragToSeek() const
{
   // Return true only if the pointer is in the
   // ruler or the track panel
   const auto &state = ::wxGetMouseState();
   const auto &position = state.GetPosition();

   auto &ruler = AdornedRulerPanel::Get( *mProject );
   if (ruler.GetScreenRect().Contains(position))
      return true;

   /*
   auto trackPanel = mProject->GetTrackPanel();
   if (trackPanel &&
       trackPanel->GetScreenRect().Contains(position))
      return true;
    */

   return false;
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

void Scrubber::Forwarder::OnMouse(wxMouseEvent &event)
{
   //auto ruler = scrubber.mProject->GetRulerPanel();
   auto isScrubbing = scrubber.IsScrubbing();
   if (isScrubbing && !event.HasAnyModifiers()) {
      if(event.LeftDown() && scrubber.MayDragToSeek()) {
         // This event handler may catch mouse transitions that are missed
         // by the polling of mouse state by the timer.
         scrubber.mScrubSeekPress = true;
      }
      else if (event.m_wheelRotation) {
         double steps = event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);
         scrubber.HandleScrollWheel(steps);
      }
      else
         event.Skip();
   }
   else
      event.Skip();
}

///////////////////////////////////////////////////////////////////////////////
// class ScrubbingOverlay is responsible for drawing the speed numbers

static const AudacityProject::AttachedObjects::RegisteredFactory sOverlayKey{
  []( AudacityProject &parent ){
     auto result = std::make_shared< ScrubbingOverlay >( &parent );
     TrackPanel::Get( parent ).AddOverlay( result );
     return result;
   }
};

ScrubbingOverlay::ScrubbingOverlay(AudacityProject *project)
   : mProject(project)
   , mLastScrubRect()
   , mNextScrubRect()
   , mLastScrubSpeedText()
   , mNextScrubSpeedText()
{
   mProject->Bind(EVT_TRACK_PANEL_TIMER,
      &ScrubbingOverlay::OnTimer,
      this);
}

unsigned ScrubbingOverlay::SequenceNumber() const
{
   return 40;
}

std::pair<wxRect, bool> ScrubbingOverlay::DoGetRectangle(wxSize)
{
   wxRect rect(mLastScrubRect);
   const bool outdated =
      (mLastScrubRect != mNextScrubRect) ||
      (!mLastScrubRect.IsEmpty() && !GetScrubber().ShouldDrawScrubSpeed()) ||
      (mLastScrubSpeedText != mNextScrubSpeedText);
   return std::make_pair(
      rect,
      outdated
   );
}

void ScrubbingOverlay::Draw(OverlayPanel &, wxDC &dc)
{
   mLastScrubRect = mNextScrubRect;
   mLastScrubSpeedText = mNextScrubSpeedText;

   Scrubber &scrubber = GetScrubber();
   if (!scrubber.ShouldDrawScrubSpeed())
      return;

   static const wxFont labelFont(24, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   dc.SetFont(labelFont);

   // These two colors were previously saturated red and green.  However
   // we have a rule to try to only use red for reserved purposes of
   //  (a) Recording
   //  (b) Error alerts
   // So they were changed to 'orange' and 'lime'.
   static const wxColour clrNoScroll(215, 162, 0), clrScroll(0, 204, 153);
   if (scrubber.IsScrollScrubbing())
      dc.SetTextForeground(clrScroll);
   else
      dc.SetTextForeground(clrNoScroll);

   dc.DrawText(mLastScrubSpeedText, mLastScrubRect.GetX(), mLastScrubRect.GetY());
}

void ScrubbingOverlay::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   Scrubber &scrubber = GetScrubber();
   const auto isScrubbing = scrubber.IsScrubbing();
   auto &ruler = AdornedRulerPanel::Get( *mProject );
   auto position = ::wxGetMousePosition();

   if (scrubber.IsSpeedPlaying())
      return;

   {
      if(scrubber.HasMark()) {
         auto xx = ruler.ScreenToClient(position).x;
         ruler.UpdateQuickPlayPos( xx, false );

         if (!isScrubbing)
            // Really start scrub if motion is far enough
            scrubber.MaybeStartScrubbing(xx);
      }

      if (!isScrubbing) {
         mNextScrubRect = wxRect();
         return;
      }
      else
         ruler.DrawBothOverlays();
   }

   if (!scrubber.ShouldDrawScrubSpeed()) {
      mNextScrubRect = wxRect();
   }
   else {
      TrackPanel &trackPanel = TrackPanel::Get( *mProject );
      auto &viewInfo = ViewInfo::Get( *mProject );
      int panelWidth, panelHeight;
      trackPanel.GetSize(&panelWidth, &panelHeight);

      // Where's the mouse?
      position = trackPanel.ScreenToClient(position);

      const bool seeking = scrubber.Seeks() || scrubber.TemporarilySeeks();

      // Find the text
      const double maxScrubSpeed = GetScrubber().GetMaxScrubSpeed();
      const double speed =
         scrubber.IsScrollScrubbing()
         ? scrubber.FindScrubSpeed( seeking,
             ViewInfo::Get( *mProject )
                .PositionToTime(position.x, viewInfo.GetLeftOffset()))
         : maxScrubSpeed;

      const wxChar *format =
         scrubber.IsScrollScrubbing()
         ? seeking
            ? wxT("%+.2fX")
            : wxT("%+.2f")
         : wxT("%.2f");

      mNextScrubSpeedText = wxString::Format(format, speed);

      // Find the origin for drawing text
      wxCoord width, height;
      {
         wxClientDC dc( &trackPanel );
         static const wxFont labelFont(24, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
         dc.SetFont(labelFont);
         dc.GetTextExtent(mNextScrubSpeedText, &width, &height);
      }
      const auto xx =
         std::max(0, std::min(panelWidth - width, position.x - width / 2));

      // Put the text above the cursor, if it fits.
      enum { offset = 20 };
      auto yy = position.y - height + offset;
      if (yy < 0)
         yy += height + 2 * offset;
      yy = std::max(0, std::min(panelHeight - height, yy));

      mNextScrubRect = wxRect(xx, yy, width, height);
   }
}

const Scrubber &ScrubbingOverlay::GetScrubber() const
{
   return Scrubber::Get( *mProject );
}

Scrubber &ScrubbingOverlay::GetScrubber()
{
   return Scrubber::Get( *mProject );
}

void Scrubber::DoScrub(bool seek)
{
   if( !CanScrub() )
      return;
   const bool wasScrubbing = HasMark() || IsScrubbing();
   const bool scroll = ShouldScrubPinned();
   if (!wasScrubbing) {
      auto &tp = TrackPanel::Get( *mProject );
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
      auto &ctb = ControlToolBar::Get( *mProject );
      ctb.StopPlaying();
   }
}

void Scrubber::OnScrubOrSeek(bool seek)
{
   DoScrub(seek);

   mSeeking = seek;
   CheckMenuItems();

   auto &ruler = AdornedRulerPanel::Get( *mProject );
   // Update button images
   ruler.UpdateButtonStates();
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

void Scrubber::OnToggleScrubRuler(const CommandContext&)
{
   auto &ruler = AdornedRulerPanel::Get( *mProject );
   ruler.OnToggleScrubRuler();
   const auto toolbar =
      ToolManager::Get( *mProject ).GetToolBar( ScrubbingBarID );
   toolbar->EnableDisableButtons();
   CheckMenuItems();
}

enum { CMD_ID = 8000 };

#define THUNK(Name) Scrubber::Thunk<&Scrubber::Name>

BEGIN_EVENT_TABLE(Scrubber, wxEvtHandler)
   EVT_MENU(CMD_ID,     THUNK(OnScrub))
   EVT_MENU(CMD_ID + 1, THUNK(OnSeek))
   EVT_MENU(CMD_ID + 2, THUNK(OnToggleScrubRuler))
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(Scrubber::Forwarder, wxEvtHandler)
   EVT_MOUSE_EVENTS(Scrubber::Forwarder::OnMouse)
END_EVENT_TABLE()

static_assert(nMenuItems == 3, "wrong number of items");

const wxString &Scrubber::GetUntranslatedStateString() const
{
   static wxString empty;

   if (IsSpeedPlaying()) {
      static wxString result = XO("Playing at Speed");
      return result;
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



std::vector<wxString> Scrubber::GetAllUntranslatedStatusStrings()
{
   using namespace std;
   vector<wxString> results;
   for (const auto &item : menuItems) {
      const auto &status = item.GetStatus();
      if (!status.empty())
         results.push_back(status);
   }
   return results;
}

bool Scrubber::CanScrub() const
{
   // Recheck the same condition as enables the Scrub/Seek menu item.
   auto gAudioIO = AudioIO::Get();
   return !( gAudioIO->IsBusy() && gAudioIO->GetNumCaptureChannels() > 0 ) &&
      HasWaveDataPred( *mProject );
}

// To supply the "finder" argument
static CommandHandlerObject &findme(AudacityProject &project)
{ return Scrubber::Get( project ); }

MenuTable::BaseItemPtr Scrubber::Menu()
{
   using Options = CommandManager::Options;

   MenuTable::BaseItemPtrs ptrs;
   for (const auto &item : menuItems) {
      ptrs.push_back( MenuTable::Command( item.name, wxGetTranslation(item.label),
          // No menu items yet have dialogs
          false,
          findme, static_cast<CommandFunctorPointer>(item.memFn),
          item.flags,
          item.StatusTest
             ? // a checkmark item
               Options{}.CheckState( (this->*item.StatusTest)() )
             : // not a checkmark item
               Options{}
      ) );
   }

   return MenuTable::Menu( _("Scru&bbing"), std::move( ptrs ) );
}

void Scrubber::PopulatePopupMenu(wxMenu &menu)
{
   int id = CMD_ID;
   auto &cm = CommandManager::Get( *mProject );
   for (const auto &item : menuItems) {
      if (cm.GetEnabled(item.name)) {
         auto test = item.StatusTest;
         menu.Append(id, wxGetTranslation(item.label), wxString{},
                     test ? wxITEM_CHECK : wxITEM_NORMAL);
         if(test && (this->*test)())
            menu.FindItem(id)->Check();
      }
      ++id;
   }
}

void Scrubber::CheckMenuItems()
{
   auto &cm = CommandManager::Get( *mProject );
   for (const auto &item : menuItems) {
      auto test = item.StatusTest;
      if (test)
         cm.Check(item.name, (this->*test)());
   }
}

#endif
