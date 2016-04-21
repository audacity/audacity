/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SCRUBBING__
#define __AUDACITY_SCRUBBING__

#include <wx/event.h>
#include <wx/longlong.h>

#include "../../Experimental.h"
#include "../../TrackPanelOverlay.h"

class AudacityProject;

// Scrub state object
class Scrubber : public wxEvtHandler
{
public:
   Scrubber(AudacityProject *project);
   ~Scrubber();

   void MarkScrubStart(
      const wxMouseEvent &event
#ifdef EXPERIMENTAL_SCRUBBING_SMOOTH_SCROLL
      , bool smoothScrolling
#endif
      , bool alwaysSeeking // if false, can switch seeking or scrubbing
                           // by mouse button state
   );
   // Returns true iff the event should be considered consumed by this:
   bool MaybeStartScrubbing(const wxMouseEvent &event);
   void ContinueScrubbing();
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

   bool ShouldDrawScrubSpeed();
   double FindScrubSpeed(bool seeking, double time) const;
   double GetMaxScrubSpeed() const { return mMaxScrubSpeed; }

   void HandleScrollWheel(int steps);

   void SetSeeking() { mScrubSeekPress = true; }
   bool PollIsSeeking();

private:
   void OnActivateOrDeactivateApp(wxActivateEvent & event);

private:
   int mScrubToken;
   wxLongLong mScrubStartClockTimeMillis;
   bool mScrubHasFocus;
   int mScrubSpeedDisplayCountdown;
   wxCoord mScrubStartPosition;
   double mMaxScrubSpeed;
   bool mScrubSeekPress;
   bool mSmoothScrollingScrub;
   bool mAlwaysSeeking{};

#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   int mLogMaxScrubSpeed;
#endif

   AudacityProject *mProject;
};

// Specialist in drawing the scrub speed, and listening for certain events
class ScrubbingOverlay final : public wxEvtHandler, public TrackPanelOverlay
{
public:
   ScrubbingOverlay(AudacityProject *project);
   virtual ~ScrubbingOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw
      (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end) override;

   void OnTimer(wxCommandEvent &event);

   const Scrubber &GetScrubber() const;
   Scrubber &GetScrubber();

   AudacityProject *mProject;

   wxRect mLastScrubRect, mNextScrubRect;
   wxString mLastScrubSpeedText, mNextScrubSpeedText;
};

#endif
