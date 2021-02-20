/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 ScrubUI.cpp
 
 Paul Licameli split from Scrubbing.cpp
 
 **********************************************************************/

#include "ScrubUI.h"

#include "Scrubbing.h"
#include "../../widgets/Overlay.h"
#include "ClientData.h"
#include "../../AdornedRulerPanel.h"
#include "Project.h"
#include "../../ProjectWindow.h"
#include "../../ProjectWindows.h"
#include "../../TrackPanel.h"

#include <wx/dcclient.h>
#include <wx/event.h>
#include <wx/windowptr.h>

///////////////////////////////////////////////////////////////////////////////
// class ScrubbingOverlay is responsible for drawing the speed numbers

// Specialist in drawing the scrub speed, and listening for certain events
class ScrubbingOverlay final
   : public wxEvtHandler
   , public Overlay
   , public ClientData::Base
{
public:
   explicit
   ScrubbingOverlay(AudacityProject *project);

private:
   unsigned SequenceNumber() const override;
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   void OnTimer(wxCommandEvent &event);

   const Scrubber &GetScrubber() const;
   Scrubber &GetScrubber();

   AudacityProject *mProject;

   wxRect mLastScrubRect, mNextScrubRect;
   wxString mLastScrubSpeedText, mNextScrubSpeedText;
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

   if (scrubber.IsSpeedPlaying() || scrubber.IsKeyboardScrubbing())
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
      auto &trackPanel = GetProjectPanel( *mProject );
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

static const AudacityProject::AttachedObjects::RegisteredFactory sOverlayKey{
  []( AudacityProject &parent ){
     auto result = std::make_shared< ScrubbingOverlay >( &parent );
     TrackPanel::Get( parent ).AddOverlay( result );
     return result;
   }
};

///////////////////////////////////////////////////////////////////////////////
// class ScrubForwarder intercepts some mouse events of the main window

// I need this because I can't push the scrubber as an event handler
// in two places at once.
struct ScrubForwarder
    : public wxEvtHandler
    , public ClientData::Base
{
   ScrubForwarder( AudacityProject &project )
      : mProject{ project }
   {
      mWindow = &ProjectWindow::Get( project );
      if ( mWindow )
         mWindow->PushEventHandler( this );
      mRuler = &AdornedRulerPanel::Get( project );
      mScrubber = Scrubber::Get( project ).shared_from_this();
   }

   ~ScrubForwarder()
   {
      if ( mWindow )
         mWindow->PopEventHandler();
   }

   AudacityProject &mProject;
   wxWindowPtr<wxWindow> mWindow;
   wxWeakRef<AdornedRulerPanel> mRuler;
   std::weak_ptr<Scrubber> mScrubber;

   void OnMouse(wxMouseEvent &event);
   DECLARE_EVENT_TABLE()
};

void ScrubForwarder::OnMouse(wxMouseEvent &event)
{
   auto pScrubber = mScrubber.lock();
   if ( !pScrubber || !mRuler ) {
      event.Skip();
      return;
   }

   auto &scrubber = *pScrubber;

   auto &ruler = *mRuler;
   const auto &state = ::wxGetMouseState();
   const auto &position = state.GetPosition();
   scrubber.SetMayDragToSeek(
      ruler.GetScreenRect().Contains(position) );

   /*
   auto trackPanel = mProject->GetTrackPanel();
   if (trackPanel &&
       trackPanel->GetScreenRect().Contains(position))
      return true;
    */

   //auto ruler = scrubber.mProject->GetRulerPanel();
   auto isScrubbing = scrubber.IsScrubbing();
   if (isScrubbing && !event.HasAnyModifiers()) {
      if(event.LeftDown() && scrubber.MayDragToSeek()) {
         // This event handler may catch mouse transitions that are missed
         // by the polling of mouse state by the timer.
         scrubber.SetSeekPress( true );
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

BEGIN_EVENT_TABLE(ScrubForwarder, wxEvtHandler)
   EVT_MOUSE_EVENTS(ScrubForwarder::OnMouse)
END_EVENT_TABLE()

static const AudacityProject::AttachedObjects::RegisteredFactory sForwarderKey{
   []( AudacityProject &parent ){
      return std::make_shared< ScrubForwarder >( parent );
   }
};
