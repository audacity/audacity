/**********************************************************************

Audacity: A Digital Audio Editor

PlayIndicatorOverlay.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "PlayIndicatorOverlay.h"

#include "../../AColor.h"
#include "../../AdornedRulerPanel.h"
#include "../../AudioIO.h"
#include "../../Project.h"
#include "../../ProjectAudioIO.h"
#include "../../ProjectAudioManager.h"
#include "../../ProjectWindow.h"
#include "../../Track.h"
#include "../../TrackPanel.h"
#include "../../ViewInfo.h"
#include "Scrubbing.h"
#include "TrackView.h"

#include <wx/dc.h>

#include <algorithm>

namespace {
   template < class LOW, class MID, class HIGH >
   bool between_incexc(LOW l, MID m, HIGH h)
   {
      return (m >= l && m < h);
   }

   enum { IndicatorMediumWidth = 13 };
}

PlayIndicatorOverlayBase::PlayIndicatorOverlayBase(AudacityProject *project, bool isMaster)
: mProject(project)
, mIsMaster(isMaster)
{
}

PlayIndicatorOverlayBase::~PlayIndicatorOverlayBase()
{
}

unsigned PlayIndicatorOverlayBase::SequenceNumber() const
{
   return 10;
}

std::pair<wxRect, bool> PlayIndicatorOverlayBase::DoGetRectangle(wxSize size)
{
   wxCoord width = 1, xx = mLastIndicatorX;

   if ( !mIsMaster ) {
      auto &ruler = AdornedRulerPanel::Get( *mProject );
      auto gAudioIO = AudioIO::Get();
      bool rec = gAudioIO->IsCapturing();
      auto pair = ruler.GetIndicatorBitmap( xx, !rec );
      xx = pair.first.x;
      width = pair.second.GetWidth();
   }

   // May be excessive height, but little matter
   wxRect rect( xx, 0, width, size.GetHeight());
   return {
      rect,
      (mLastIndicatorX != mNewIndicatorX
       || mLastIsCapturing != mNewIsCapturing)
   };
}


void PlayIndicatorOverlayBase::Draw(OverlayPanel &panel, wxDC &dc)
{
   // Set play/record color
   auto gAudioIO = AudioIO::Get();
   bool rec = gAudioIO->IsCapturing();
   AColor::IndicatorColor(&dc, !rec);

   if (mIsMaster
       && mLastIsCapturing != mNewIsCapturing) {
      // Detect transition to recording during punch and roll; make ruler
      // change its button color too
      auto &ruler = AdornedRulerPanel::Get( *mProject );
      ruler.UpdateButtonStates();
      ruler.Refresh();
   }
   mLastIsCapturing = mNewIsCapturing;

   mLastIndicatorX = mNewIndicatorX;
   if (!between_incexc(0, mLastIndicatorX, dc.GetSize().GetWidth()))
      return;

   if(auto tp = dynamic_cast<TrackPanel*>(&panel)) {
      wxASSERT(mIsMaster);

      AColor::Line(dc, mLastIndicatorX, tp->GetRect().GetTop(), mLastIndicatorX, tp->GetRect().GetBottom());
   }
   else if(auto ruler = dynamic_cast<AdornedRulerPanel*>(&panel)) {
      wxASSERT(!mIsMaster);

      auto pair = ruler->GetIndicatorBitmap( mLastIndicatorX, !rec );
      dc.DrawBitmap( pair.second, pair.first.x, pair.first.y );
   }
   else
      wxASSERT(false);
}

static const AudacityProject::AttachedObjects::RegisteredFactory sOverlayKey{
  []( AudacityProject &parent ){
     auto result = std::make_shared< PlayIndicatorOverlay >( &parent );
     TrackPanel::Get( parent ).AddOverlay( result );
     return result;
   }
};

PlayIndicatorOverlay::PlayIndicatorOverlay(AudacityProject *project)
: PlayIndicatorOverlayBase(project, true)
{
   ProjectWindow::Get( *mProject ).GetPlaybackScroller().Bind(
      EVT_TRACK_PANEL_TIMER,
      &PlayIndicatorOverlay::OnTimer,
      this);
}

void PlayIndicatorOverlay::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   // Ensure that there is an overlay attached to the ruler
   if (!mPartner) {
      auto &ruler = AdornedRulerPanel::Get( *mProject );
      mPartner = std::make_shared<PlayIndicatorOverlayBase>(mProject, false);
      ruler.AddOverlay( mPartner );
   }

   const auto &viewInfo = ViewInfo::Get( *mProject );
   auto width = viewInfo.GetTracksUsableWidth();

   if (!ProjectAudioIO::Get( *mProject ).IsAudioActive()) {
      mNewIndicatorX = -1;
      mNewIsCapturing = false;
      const auto &scrubber = Scrubber::Get( *mProject );
      if (scrubber.HasMark()) {
         auto position = scrubber.GetScrubStartPosition();
         const auto offset = viewInfo.GetLeftOffset();
         if(position >= viewInfo.GetLeftOffset() &&
            position < offset + width)
            mNewIndicatorX = position;
      }
   }
   else {
      auto &window = ProjectWindow::Get( *mProject );
      auto &scroller = window.GetPlaybackScroller();
      // Calculate the horizontal position of the indicator
      const double playPos = scroller.GetRecentStreamTime();

      using Mode = ProjectWindow::PlaybackScroller::Mode;
      const Mode mode = scroller.GetMode();
      const bool pinned = ( mode == Mode::Pinned || mode == Mode::Right );

      // Use a small tolerance to avoid flicker of play head pinned all the way
      // left or right
      const auto tolerance = pinned ? 1.5 * kTimerInterval / 1000.0 : 0;
      bool onScreen = playPos >= 0.0 &&
         between_incexc(viewInfo.h - tolerance,
         playPos,
         viewInfo.GetScreenEndTime() + tolerance);

      auto gAudioIO = AudioIO::Get();
      const auto &scrubber = Scrubber::Get( *mProject );

      // BG: Scroll screen if option is set
      if( viewInfo.bUpdateTrackIndicator &&
          playPos >= 0 && !onScreen ) {
         // msmeyer: But only if not playing looped or in one-second mode
         // PRL: and not scrolling with play/record head fixed
         auto mode = ProjectAudioManager::Get( *mProject ).GetLastPlayMode();
         if (!pinned &&
             mode != PlayMode::oneSecondPlay &&
             !gAudioIO->IsPaused() &&
             // Bug 2656 allow scrolling when paused in 
             // scrubbing/play-at-speed.
             // ONLY do this additional test if scrubbing/play-at-speed
             // is active.
             (!scrubber.IsScrubbing() || !scrubber.IsPaused())
            )
         {
            auto newPos = playPos;
            if (playPos < viewInfo.h) {
               // This is possible when scrubbing backwards.
               // We want to page leftward by (at least) a whole screen, not
               // just a little bit equal to the scrubbing poll interval
               // duration.
               newPos = viewInfo.OffsetTimeByPixels( newPos, -width );
               newPos = std::max( newPos, window.ScrollingLowerBoundTime() );
            }
            window.TP_ScrollWindow(newPos);
            // Might yet be off screen, check it
            onScreen = playPos >= 0.0 &&
            between_incexc(viewInfo.h,
                           playPos,
                           viewInfo.GetScreenEndTime());
         }
      }

      // Always update scrollbars even if not scrolling the window. This is
      // important when NEW audio is recorded, because this can change the
      // length of the project and therefore the appearance of the scrollbar.
      window.TP_RedrawScrollbars();

      if (onScreen)
         mNewIndicatorX =
            viewInfo.TimeToPosition(playPos, viewInfo.GetLeftOffset());
      else
         mNewIndicatorX = -1;

      mNewIsCapturing = gAudioIO->IsCapturing();
   }

   if(mPartner)
      mPartner->Update(mNewIndicatorX);
}
