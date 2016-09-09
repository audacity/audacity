/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SCRUBBING__
#define __AUDACITY_SCRUBBING__

#include "../../MemoryX.h"
#include <vector>
#include <wx/event.h>
#include <wx/longlong.h>

#include "../../Experimental.h"
#include "../../widgets/Overlay.h"
#include "../../../include/audacity/Types.h"

class AudacityProject;

// Conditionally compile either a separate thead, or else use a timer in the main
// thread, to poll the mouse and update scrubbing speed and direction.  The advantage of
// a thread may be immunity to choppy scrubbing in case redrawing takes too much time.
#ifdef __WXGTK__
// Unfortunately some things the thread needs to do are not thread safe
#else
#define USE_SCRUB_THREAD
#endif

// For putting an increment of work in the scrubbing queue
struct ScrubbingOptions {
   ScrubbingOptions() {}

   bool adjustStart {};

   // usually from TrackList::GetEndTime()
   sampleCount maxSample {};
   sampleCount minSample {};

   bool enqueueBySpeed {};

   double delay {};

   // Limiting values for the speed of a scrub interval:
   double minSpeed { 0.0 };
   double maxSpeed { 1.0 };


   // When maximum speed scrubbing skips to follow the mouse,
   // this is the minimum amount of playback allowed at the maximum speed:
   long minStutter {};

   // Scrubbing needs the time of start of the mouse movement that began
   // the scrub:
   wxLongLong startClockTimeMillis { -1 };

   static double MaxAllowedScrubSpeed()
   { return 32.0; } // Is five octaves enough for your amusement?
   static double MinAllowedScrubSpeed()
   { return 0.01; } // Mixer needs a lower bound speed.  Scrub no slower than this.
};

// Scrub state object
class Scrubber : public wxEvtHandler
{
public:
   Scrubber(AudacityProject *project);
   ~Scrubber();

   // Assume xx is relative to the left edge of TrackPanel!
   void MarkScrubStart(wxCoord xx, bool smoothScrolling, bool seek);

   // Returns true iff the event should be considered consumed by this:
   // Assume xx is relative to the left edge of TrackPanel!
   bool MaybeStartScrubbing(wxCoord xx);

   void ContinueScrubbingUI();
   void ContinueScrubbingPoll();

   // This is meant to be called only from ControlToolBar
   void StopScrubbing();

   wxCoord GetScrubStartPosition() const
   { return mScrubStartPosition; }

   // True iff the user has clicked to start scrub and not yet stopped,
   // but IsScrubbing() may yet be false
   bool HasStartedScrubbing() const
   { return GetScrubStartPosition() >= 0; }
   bool IsScrubbing() const;

   bool IsScrollScrubbing() const // If true, implies HasStartedScrubbing()
   { return mSmoothScrollingScrub; }
   void SetScrollScrubbing(bool value)
   { mSmoothScrollingScrub = value; }

   bool ChoseSeeking() const;
   bool MayDragToSeek() const;
   bool TemporarilySeeks() const;
   bool Seeks() const;
   bool Scrubs() const;
   bool ShowsBar() const;

   void Cancel()
   { mCancelled = true; }

   bool ShouldDrawScrubSpeed();
   double FindScrubSpeed(bool seeking, double time) const;
   double GetMaxScrubSpeed() const { return mOptions.maxSpeed; }

   void HandleScrollWheel(int steps);

   // This returns the same as the enabled state of the menu items:
   bool CanScrub() const;

   // For the toolbar
   void AddMenuItems();
   // For popup
   void PopulatePopupMenu(wxMenu &menu);

   void OnScrubOrSeek(bool seek);
   void OnScrub(wxCommandEvent&);
   void OnSeek(wxCommandEvent&);
   void OnToggleScrubRuler(wxCommandEvent&);

   // A string to put in the leftmost part of the status bar
   // when scrub or seek is in progress, or else empty.
   const wxString &GetUntranslatedStateString() const;
   const wxString &StatusMessageForWave() const;

   // All possible status strings.
   static std::vector<wxString> GetAllUntranslatedStatusStrings();

   void Pause(bool paused);
   bool IsPaused() const;
   void CheckMenuItems();
   // Bug 1508
   bool IsOneShotSeeking()const { return mInOneShotMode && IsScrubbing();};
   bool mInOneShotMode;

private:
   void DoScrub(bool seek);
   void OnActivateOrDeactivateApp(wxActivateEvent & event);

   // I need this because I can't push the scrubber as an event handler
   // in two places at once.
   struct Forwarder : public wxEvtHandler {
      Forwarder(Scrubber &scrubber_) : scrubber( scrubber_ ) {}

      Scrubber &scrubber;

      void OnMouse(wxMouseEvent &event);
      DECLARE_EVENT_TABLE()
   };
   Forwarder mForwarder{ *this };

private:
   int mScrubToken;
   bool mPaused;
   int mScrubSpeedDisplayCountdown;
   wxCoord mScrubStartPosition;
   wxCoord mLastScrubPosition {};
   bool mScrubSeekPress {};
   bool mSmoothScrollingScrub;

   bool mSeeking {};

   bool mDragging {};

   bool mCancelled {};

#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   int mLogMaxScrubSpeed;
#endif

   AudacityProject *mProject;

   DECLARE_EVENT_TABLE()

#ifdef USE_SCRUB_THREAD
   // Course corrections in playback are done in a helper thread, unhindered by
   // the complications of the main event dispatch loop
   class ScrubPollerThread;
   ScrubPollerThread *mpThread {};
#endif

   // Other periodic update of the UI must be done in the main thread,
   // by this object which is driven by timer events.
   class ScrubPoller;
   std::unique_ptr<ScrubPoller> mPoller;

   ScrubbingOptions mOptions;
   double mMaxSpeed { 1.0 };
};

// Specialist in drawing the scrub speed, and listening for certain events
class ScrubbingOverlay final : public wxEvtHandler, public Overlay
{
public:
   ScrubbingOverlay(AudacityProject *project);
   virtual ~ScrubbingOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   void OnTimer(wxCommandEvent &event);

   const Scrubber &GetScrubber() const;
   Scrubber &GetScrubber();

   AudacityProject *mProject;

   wxRect mLastScrubRect, mNextScrubRect;
   wxString mLastScrubSpeedText, mNextScrubSpeedText;
};

#endif
