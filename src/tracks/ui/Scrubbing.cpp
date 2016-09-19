/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "Scrubbing.h"
#include "../../Experimental.h"
#include <functional>

#include "../../AudioIO.h"
#include "../../Project.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelCell.h"
#include "../../TrackPanelCellIterator.h"
#include "../../commands/CommandFunctors.h"
#include "../../prefs/TracksPrefs.h"
#include "../../toolbars/ControlToolBar.h"
#include "../../toolbars/ScrubbingToolBar.h"
#include "../../toolbars/ToolManager.h"

#undef USE_TRANSCRIPTION_TOOLBAR
#ifdef USE_TRANSCRIPTION_TOOLBAR
#include "../../toolbars/TranscriptionToolBar.h"
#endif

#include "../../widgets/Ruler.h"

#include <algorithm>

#include <wx/dc.h>

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

   ScrubPollInterval_ms = 50,

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

      // Width of visible track area, in time terms:
      const double origin = viewInfo.h + screen / 2.0;

      // There are various snapping zones that are this fraction of screen:
      const double snap = 0.05;

      // By shrinking denom a bit, we make margins left and right
      // that snap to maximum and negative maximum speeds.
      const double factor = 1.0 - (snap * 2);
      const double denom = factor * screen / 2.0;
      double fraction = std::min(1.0, fabs(timeAtMouse - origin) / denom);

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
      const double halfScreen = screen / 2.0;
      const double origin = viewInfo.h + halfScreen;

      // The snapping zone is this fraction of screen, on each side of the
      // center line:
      const double snap = 0.05;
      const double fraction =
         std::max(snap, std::min(1.0, fabs(timeAtMouse - origin) / halfScreen));

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
   , mPoller { std::make_unique<ScrubPoller>(*this) }
   , mOptions {}
   , mInOneShotMode( false )

{
   if (wxTheApp)
      wxTheApp->Connect
      (wxEVT_ACTIVATE_APP,
      wxActivateEventHandler(Scrubber::OnActivateOrDeactivateApp), NULL, this);
   mProject->PushEventHandler(&mForwarder);
}

Scrubber::~Scrubber()
{
#ifdef USE_SCRUB_THREAD
   if (mpThread)
      mpThread->Delete();
#endif

   mProject->PopEventHandler();
   if (wxTheApp)
      wxTheApp->Disconnect
      (wxEVT_ACTIVATE_APP,
      wxActivateEventHandler(Scrubber::OnActivateOrDeactivateApp), NULL, this);
}

namespace {
   const struct MenuItem {
      wxString name;
      wxString label;
      wxString status;
      CommandFlag flags;
      void (Scrubber::*memFn)(wxCommandEvent&);
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

      { wxT("ToggleScrubRuler"),            XO("Scrub Ruler"),   XO(""),
         AlwaysEnabledFlag,
         &Scrubber::OnToggleScrubRuler, true,    &Scrubber::ShowsBar,
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

   ControlToolBar * const ctb = mProject->GetControlToolBar();

   // Stop any play in progress
   ctb->StopPlaying();
   // Usually the timer handler of TrackPanel does this, but we do this now,
   // so that same timer does not StopPlaying() again after this function and destroy
   // scrubber state
   mProject->SetAudioIOToken(0);

   mSeeking = seek;
   CheckMenuItems();

   ctb->SetPlay(true, ControlToolBar::PlayAppearance::Straight );
   // Commented out for Bug 1421
   //   mSeeking
   //   ? ControlToolBar::PlayAppearance::Seek
   //   : ControlToolBar::PlayAppearance::Scrub);

   mScrubStartPosition = xx;
   ctb->UpdateStatusBar(mProject);
   mOptions.startClockTimeMillis = ::wxGetLocalTimeMillis();
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

      const bool busy = gAudioIO->IsBusy();
      if (busy && gAudioIO->GetNumCaptureChannels() > 0) {
         // Do not stop recording, and don't try to start scrubbing after
         // recording stops
         mScrubStartPosition = -1;
         return false;
      }

      wxCoord position = xx;
      if (abs(mScrubStartPosition - position) >= SCRUBBING_PIXEL_TOLERANCE) {
         const ViewInfo &viewInfo = mProject->GetViewInfo();
         TrackPanel *const trackPanel = mProject->GetTrackPanel();
         ControlToolBar * const ctb = mProject->GetControlToolBar();
         double maxTime = mProject->GetTracks()->GetEndTime();
         const int leftOffset = trackPanel->GetLeftOffset();
         double time0 = std::min(maxTime,
            viewInfo.PositionToTime(mScrubStartPosition, leftOffset)
         );
         double time1 = std::min(maxTime,
            viewInfo.PositionToTime(position, leftOffset)
         );
         if (time1 != time0)
         {
            if (busy) {
               auto position = mScrubStartPosition;
               ctb->StopPlaying();
               mScrubStartPosition = position;
            }

#ifdef DRAG_SCRUB
            if (mDragging && mSmoothScrollingScrub) {
               auto delta = time0 - time1;
               time0 = std::max(0.0, std::min(maxTime,
                  (viewInfo.h + mProject->GetScreenEndTime()) / 2
               ));
               time1 = time0 + delta;
            }
#endif

            AudioIOStartStreamOptions options(mProject->GetDefaultPlayOptions());
            options.pScrubbingOptions = &mOptions;
            options.timeTrack = NULL;
            mOptions.delay = (ScrubPollInterval_ms * 0.9 / 1000.0);
            mOptions.minSpeed = 0.0;
#ifdef USE_TRANSCRIPTION_TOOLBAR
            if (!mAlwaysSeeking) {
               // Take the starting speed limit from the transcription toolbar,
               // but it may be varied during the scrub.
               mMaxSpeed = mOptions.maxSpeed =
                  mProject->GetTranscriptionToolBar()->GetPlaySpeed();
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
            mOptions.minSample = 0;
            mOptions.maxSample =
               lrint(std::max(0.0, mProject->GetTracks()->GetEndTime()) * options.rate);
            mOptions.minStutter =
#ifdef DRAG_SCRUB
               mDragging ? 0.0 :
#endif
               lrint(std::max(0.0, MinStutter) * options.rate);

            ControlToolBar::PlayAppearance appearance = 
            // commented out to fix Bug 1241
            // mSeeking
            //   ? ControlToolBar::PlayAppearance::Seek
            //   : ControlToolBar::PlayAppearance::Scrub;
                 ControlToolBar::PlayAppearance::Straight;
//            const bool cutPreview = false;
            const bool backwards = time1 < time0;
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
            static const double maxScrubSpeedBase =
               pow(2.0, 1.0 / ScrubSpeedStepsPerOctave);
            mLogMaxScrubSpeed = floor(0.5 +
               log(mMaxSpeed) / log(maxScrubSpeedBase)
            );
#endif
            mScrubSpeedDisplayCountdown = 0;
            mScrubToken =
               ctb->PlayPlayRegion(SelectedRegion(time0, time1), options,
                                   PlayMode::normalPlay, appearance, backwards);
         }
      }
      else
         // Wait to test again
         mOptions.startClockTimeMillis = ::wxGetLocalTimeMillis();

      if (IsScrubbing()) {
         mPaused = false;
         mLastScrubPosition = xx;

#ifdef USE_SCRUB_THREAD
         // Detached thread is self-deleting, after it receives the Delete() message
         mpThread = safenew ScrubPollerThread{ *this };
         mpThread->Create(4096);
         mpThread->Run();
#endif

         mPoller->Start(ScrubPollInterval_ms);
      }

      // Return true whether we started scrub, or are still waiting to decide.
      return true;
   }
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

   bool result = false;
   if (mPaused) {
      // When paused, enqueue silent scrubs.
      mOptions.minSpeed = 0.0;
      mOptions.maxSpeed = mMaxSpeed;
      mOptions.adjustStart = false;
      mOptions.enqueueBySpeed = true;
      result = gAudioIO->EnqueueScrub(0, mOptions);
   }
   else {
      const wxMouseState state(::wxGetMouseState());
      const auto trackPanel = mProject->GetTrackPanel();
      const wxPoint position = trackPanel->ScreenToClient(state.GetPosition());
      const auto &viewInfo = mProject->GetViewInfo();
#ifdef DRAG_SCRUB
      if (mDragging && mSmoothScrollingScrub) {
         const auto lastTime = gAudioIO->GetLastTimeInScrubQueue();
         const auto delta = mLastScrubPosition - position.x;
         const double time = viewInfo.OffsetTimeByPixels(lastTime, delta);
         mOptions.minSpeed = 0.0;
         mOptions.maxSpeed = mMaxSpeed;
         mOptions.adjustStart = true;
         mOptions.enqueueBySpeed = false;
         result = gAudioIO->EnqueueScrub(time, mOptions);
         mLastScrubPosition = position.x;
      }
      else
#endif
      {
         const double time = viewInfo.PositionToTime(position.x, trackPanel->GetLeftOffset());
         mOptions.adjustStart = seek;
         mOptions.minSpeed = seek ? 1.0 : 0.0;
         mOptions.maxSpeed = seek ? 1.0 : mMaxSpeed;

         if (mSmoothScrollingScrub) {
            const double speed = FindScrubSpeed(seek, time);
            mOptions.enqueueBySpeed = true;
            result = gAudioIO->EnqueueScrub(speed, mOptions);
         }
         else {
            mOptions.enqueueBySpeed = false;
            result = gAudioIO->EnqueueScrub(time, mOptions);
         }
      }
   }

   if (result)
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
      mProject->DoPlayStopSelect(true, state.ShiftDown());
      wxCommandEvent evt;
      mProject->GetControlToolBar()->OnStop(evt);
      return;
   }

   const bool seek = Seeks() || TemporarilySeeks();

   {
      // Show the correct status for seeking.
      bool backup = mSeeking;
      mSeeking = seek;
      const auto ctb = mProject->GetControlToolBar();
      if (ctb)
         ctb->UpdateStatusBar(mProject);
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

void Scrubber::StopScrubbing()
{
#ifdef USE_SCRUB_THREAD
   if (mpThread) {
      mpThread->Delete();
      mpThread = nullptr;
   }
#endif

   mPoller->Stop();

   if (HasStartedScrubbing() && !mCancelled) {
      const wxMouseState state(::wxGetMouseState());
      // Stop and set cursor
      mProject->DoPlayStopSelect(true, state.ShiftDown());
   }

   mScrubStartPosition = -1;
   mDragging = false;
   mSeeking = false;

   if (!IsScrubbing())
   {
      // Marked scrub start, but
      // didn't really play, but did change button apperance
      const auto ctb = mProject->GetControlToolBar();
      ctb->SetPlay(false, ControlToolBar::PlayAppearance::Straight);
   }

   mProject->GetRulerPanel()->HideQuickPlayIndicator();
   CheckMenuItems();
}

bool Scrubber::ShowsBar() const
{
   return mProject->GetRulerPanel()->ShowingScrubRuler();
}

bool Scrubber::IsScrubbing() const
{
   if (mScrubToken <= 0)
      return false;
   else if (mScrubToken == mProject->GetAudioIOToken() &&
            mProject->IsAudioActive())
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

   auto ruler = mProject->GetRulerPanel();
   if (ruler &&
       ruler->GetScreenRect().Contains(position))
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
   return (HasStartedScrubbing() || IsScrubbing()) && ChoseSeeking();
}

bool Scrubber::Scrubs() const
{
   if( Seeks() )
      return false;
   return (HasStartedScrubbing() || IsScrubbing()) && !ChoseSeeking();
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
   ViewInfo &viewInfo = mProject->GetViewInfo();
   const double screen = mProject->GetScreenEndTime() - viewInfo.h;
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
   if (event.GetActive())
      Pause(!IsScrubbing() || mProject->GetControlToolBar()->IsPauseDown());
   else
      Pause(true);

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

ScrubbingOverlay::ScrubbingOverlay(AudacityProject *project)
   : mProject(project)
   , mLastScrubRect()
   , mNextScrubRect()
   , mLastScrubSpeedText()
   , mNextScrubSpeedText()
{
   mProject->Connect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(ScrubbingOverlay::OnTimer),
      NULL,
      this);
}

ScrubbingOverlay::~ScrubbingOverlay()
{
   mProject->Disconnect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(ScrubbingOverlay::OnTimer),
      NULL,
      this);
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

   static const wxFont labelFont(24, wxSWISS, wxNORMAL, wxNORMAL);
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
   const auto ruler = mProject->GetRulerPanel();
   auto position = ::wxGetMousePosition();

   {
      if(scrubber.HasStartedScrubbing()) {
         auto xx = ruler->ScreenToClient(position).x;
         ruler->UpdateQuickPlayPos(xx);

         if (!isScrubbing)
            // Really start scrub if motion is far enough
            scrubber.MaybeStartScrubbing(xx);
      }

      if (!isScrubbing) {
         mNextScrubRect = wxRect();
         return;
      }
      else
         ruler->ShowQuickPlayIndicator();
   }

   if (!scrubber.ShouldDrawScrubSpeed()) {
      mNextScrubRect = wxRect();
   }
   else {
      TrackPanel *const trackPanel = mProject->GetTrackPanel();
      int panelWidth, panelHeight;
      trackPanel->GetSize(&panelWidth, &panelHeight);

      // Where's the mouse?
      position = trackPanel->ScreenToClient(position);

      const bool seeking = scrubber.Seeks() || scrubber.TemporarilySeeks();

      // Find the text
      const double maxScrubSpeed = GetScrubber().GetMaxScrubSpeed();
      const double speed =
         scrubber.IsScrollScrubbing()
         ? scrubber.FindScrubSpeed
            (seeking, mProject->GetViewInfo().PositionToTime(position.x, trackPanel->GetLeftOffset()))
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
         wxClientDC dc(trackPanel);
         static const wxFont labelFont(24, wxSWISS, wxNORMAL, wxNORMAL);
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
   return mProject->GetScrubber();
}

Scrubber &ScrubbingOverlay::GetScrubber()
{
   return mProject->GetScrubber();
}

void Scrubber::DoScrub(bool seek)
{
   if( !CanScrub() )
      return;
   const bool wasScrubbing = HasStartedScrubbing() || IsScrubbing();
   const bool scroll = TracksPrefs::GetPinnedHeadPreference();
   if (!wasScrubbing) {
      auto tp = mProject->GetTrackPanel();
      wxCoord xx = tp->ScreenToClient(::wxGetMouseState().GetPosition()).x;

      // Limit x
      int width;
      tp->GetTracksUsableArea(&width, nullptr);
      const auto offset = tp->GetLeftOffset();
      xx = (std::max(offset, std::min(offset + width - 1, xx)));

      MarkScrubStart(xx, scroll, seek);
   }
   else if (mSeeking != seek) {
      // just switching mode
   }
   else
      mProject->GetControlToolBar()->StopPlaying();
}

void Scrubber::OnScrubOrSeek(bool seek)
{
   DoScrub(seek);

   if (HasStartedScrubbing()) {
      // Show the correct status.
      const auto ctb = mProject->GetControlToolBar();
      ctb->UpdateStatusBar(mProject);
   }

   mSeeking = seek;
   CheckMenuItems();

   auto ruler = mProject->GetRulerPanel();
   if (ruler)
      // Update button images
      ruler->UpdateButtonStates();

   auto scrubbingToolBar = mProject->GetScrubbingToolBar();
   scrubbingToolBar->EnableDisableButtons();
   scrubbingToolBar->RegenerateTooltips();
}

void Scrubber::OnScrub(wxCommandEvent&)
{
   OnScrubOrSeek(false);
   CheckMenuItems();
}

void Scrubber::OnSeek(wxCommandEvent&)
{
   OnScrubOrSeek(true);
   CheckMenuItems();
}

void Scrubber::OnToggleScrubRuler(wxCommandEvent&)
{
   mProject->GetRulerPanel()->OnToggleScrubRuler();
   const auto toolbar = mProject->GetToolManager()->GetToolBar(ScrubbingBarID);
   toolbar->EnableDisableButtons();
   CheckMenuItems();
}

enum { CMD_ID = 8000 };

BEGIN_EVENT_TABLE(Scrubber, wxEvtHandler)
   EVT_MENU(CMD_ID,     Scrubber::OnScrub)
   EVT_MENU(CMD_ID + 1, Scrubber::OnSeek)
   EVT_MENU(CMD_ID + 2, Scrubber::OnToggleScrubRuler)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(Scrubber::Forwarder, wxEvtHandler)
   EVT_MOUSE_EVENTS(Scrubber::Forwarder::OnMouse)
END_EVENT_TABLE()

static_assert(nMenuItems == 3, "wrong number of items");

const wxString &Scrubber::GetUntranslatedStateString() const
{
   static wxString empty;

   if (HasStartedScrubbing()) {
      auto &item = FindMenuItem(Seeks() || TemporarilySeeks());
      return item.status;
   }
   else
      return empty;
}

const wxString & Scrubber::StatusMessageForWave() const
{
   static wxString result;
   result = "";

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
   return move(results);
}

bool Scrubber::CanScrub() const
{
   // Return the enabled state for the menu item that really launches the scrub or seek.
   auto cm = mProject->GetCommandManager();
   return cm->GetEnabled(menuItems[ 0 ].name);
}

void Scrubber::AddMenuItems()
{
   auto cm = mProject->GetCommandManager();

   cm->BeginSubMenu(_("Scru&bbing"));
   for (const auto &item : menuItems) {
      if (item.StatusTest)
         cm->AddCheck(item.name, wxGetTranslation(item.label),
                      FNT(Scrubber, this, item.memFn),
                      false,
                      item.flags, item.flags);
      else
         // The start item
         cm->AddItem(item.name, wxGetTranslation(item.label),
                     FNT(Scrubber, this, item.memFn),
                     item.flags, item.flags);
   }
   cm->EndSubMenu();
   CheckMenuItems();
}

void Scrubber::PopulatePopupMenu(wxMenu &menu)
{
   int id = CMD_ID;
   auto cm = mProject->GetCommandManager();
   for (const auto &item : menuItems) {
      if (cm->GetEnabled(item.name)) {
         auto test = item.StatusTest;
         menu.Append(id, item.label, wxString{},
                     test ? wxITEM_CHECK : wxITEM_NORMAL);
         if(test && (this->*test)())
            menu.FindItem(id)->Check();
      }
      ++id;
   }
}

void Scrubber::CheckMenuItems()
{
   auto cm = mProject->GetCommandManager();
   for (const auto &item : menuItems) {
      auto test = item.StatusTest;
      if (test)
         cm->Check(item.name, (this->*test)());
   }
}

#endif
