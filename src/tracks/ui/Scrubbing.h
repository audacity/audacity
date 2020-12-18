/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SCRUBBING__
#define __AUDACITY_SCRUBBING__



#include <vector>
#include <wx/longlong.h>

#include "../../ScrubState.h" // for ScrubbingOptions
#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit
#include "../../widgets/Overlay.h" // to inherit
#include "../../commands/CommandContext.h"
#include "../../commands/CommandManager.h" // for MenuTable
#include "Identifier.h"

class AudacityProject;
class TranslatableString;

// Conditionally compile either a separate thead, or else use a timer in the main
// thread, to poll the mouse and update scrubbing speed and direction.  The advantage of
// a thread may be immunity to choppy scrubbing in case redrawing takes too much time.
#ifdef __WXGTK__
// Unfortunately some things the thread needs to do are not thread safe
#else
#define USE_SCRUB_THREAD
#endif

// Scrub state object
class AUDACITY_DLL_API Scrubber final
   : public wxEvtHandler
   , public ClientData::Base
   , private PrefsListener
   , public std::enable_shared_from_this< Scrubber >
{
public:   
   static Scrubber &Get( AudacityProject &project );
   static const Scrubber &Get( const AudacityProject &project );

   explicit
   Scrubber(AudacityProject *project);
   Scrubber( const Scrubber & ) PROHIBITED;
   Scrubber &operator=( const Scrubber & ) PROHIBITED;
   ~Scrubber();

   static bool ShouldScrubPinned();
   
   // Assume xx is relative to the left edge of TrackPanel!
   void MarkScrubStart(wxCoord xx, bool smoothScrolling, bool seek);

   // Returns true iff the event should be considered consumed by this:
   // Assume xx is relative to the left edge of TrackPanel!
   bool MaybeStartScrubbing(wxCoord xx);
   bool StartSpeedPlay(double speed, double time0, double time1);
   bool StartKeyboardScrubbing(double time0, bool backwards);
   double GetKeyboardScrubbingSpeed();

   void ContinueScrubbingUI();
   void ContinueScrubbingPoll();

   // This is meant to be called only from ProjectAudioManager
   void StopScrubbing();

   wxCoord GetScrubStartPosition() const
   { return mScrubStartPosition; }

   bool WasSpeedPlaying() const
   { return mSpeedPlaying;}
   bool IsSpeedPlaying() const
   { return IsScrubbing() && mSpeedPlaying; }
   bool WasKeyboardScrubbing() const
   { return mKeyboardScrubbing; }
   bool IsKeyboardScrubbing() const
   { return IsScrubbing() && mKeyboardScrubbing; }
   void SetBackwards(bool backwards)
   { mBackwards = backwards;}
   bool IsBackwards() const
   { return mBackwards;}
   // True iff the user has clicked to start scrub and not yet stopped,
   // but IsScrubbing() may yet be false
   bool HasMark() const
   { return GetScrubStartPosition() >= 0; }
   bool IsScrubbing() const;

   bool IsScrollScrubbing() const // If true, implies HasMark()
   { return mSmoothScrollingScrub; }
   void SetScrollScrubbing(bool value)
   { mSmoothScrollingScrub = value; }

   bool ChoseSeeking() const;
   void SetMayDragToSeek( bool value ) { mMayDragToSeek = value; }
   bool MayDragToSeek() const { return mMayDragToSeek; }
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

   // For popup
   void PopulatePopupMenu(wxMenu &menu);

   void OnScrubOrSeek(bool seek);
   void OnScrub(const CommandContext&);
   void OnSeek(const CommandContext&);
   void OnToggleScrubRuler(const CommandContext&);

   void OnKeyboardScrubBackwards(const CommandContext&);
   void OnKeyboardScrubForwards(const CommandContext&);
   void DoKeyboardScrub(bool backwards, bool keyUp);

   // Convenience wrapper for the above
   template<void (Scrubber::*pfn)(const CommandContext&)>
      void Thunk(wxCommandEvent &)
         { (this->*pfn)(*mProject); }

   // A string to put in the leftmost part of the status bar
   // when scrub or seek is in progress, or else empty.
   const TranslatableString &GetUntranslatedStateString() const;
   wxString StatusMessageForWave() const;

   void Pause(bool paused);
   bool IsPaused() const;
   void CheckMenuItems();

   bool IsTransportingPinned() const;

   void SetSeekPress( bool value ) { mScrubSeekPress = value; }

private:
   void UpdatePrefs() override;

   void StartPolling();
   void StopPolling();
   void DoScrub(bool seek);
   void OnActivateOrDeactivateApp(wxActivateEvent & event);

private:
   int mScrubToken;
   int mScrubSpeedDisplayCountdown;
   wxCoord mScrubStartPosition;
   wxCoord mLastScrubPosition {};
   bool mScrubSeekPress {};
   bool mSmoothScrollingScrub;

   bool mPaused{};
   bool mSeeking {};
   bool mSpeedPlaying{true};
   bool mKeyboardScrubbing{};
   bool mBackwards{};
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

   bool mShowScrubbing { false };
   bool mMayDragToSeek{ false };
};

#endif
