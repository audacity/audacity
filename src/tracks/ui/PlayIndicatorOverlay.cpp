/**********************************************************************

Audacity: A Digital Audio Editor

PlayIndicatorOverlay.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "PlayIndicatorOverlay.h"

#include "../../AColor.h"
#include "../../AdornedRulerPanel.h"
#include "../../AudioIO.h"
#include "../../Project.h"
#include "../../TrackPanel.h"
#include "Scrubbing.h"

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

std::pair<wxRect, bool> PlayIndicatorOverlayBase::DoGetRectangle(wxSize size)
{
   auto width = mIsMaster ? 1 : IndicatorMediumWidth;

   // May be excessive height, but little matter
   wxRect rect(mLastIndicatorX - width / 2, 0, width, size.GetHeight());
   return {
      rect,
      (mLastIndicatorX != mNewIndicatorX
       || mLastIsCapturing != mNewIsCapturing)
   };
}


void PlayIndicatorOverlayBase::Draw(OverlayPanel &panel, wxDC &dc)
{
   // Set play/record color
   bool rec = gAudioIO->IsCapturing();
   AColor::IndicatorColor(&dc, !rec);

   if (mIsMaster
       && mLastIsCapturing != mNewIsCapturing) {
      // Detect transition to recording during punch and roll; make ruler
      // change its button color too
      const auto ruler = mProject->GetRulerPanel();
      ruler->UpdateButtonStates();
      ruler->Refresh();
   }
   mLastIsCapturing = mNewIsCapturing;

   mLastIndicatorX = mNewIndicatorX;
   if (!between_incexc(0, mLastIndicatorX, dc.GetSize().GetWidth()))
      return;

   if(auto tp = dynamic_cast<TrackPanel*>(&panel)) {
      wxASSERT(mIsMaster);

      // Draw indicator in all visible tracks
      tp->VisitCells( [&]( const wxRect &rect, TrackPanelCell &cell ) {
         const auto pTrack = dynamic_cast<Track*>(&cell);
         if (pTrack) pTrack->TypeSwitch(
            [](LabelTrack *) {
               // Don't draw the indicator in label tracks
            },
            [&](Track *) {
               // Draw the NEW indicator in its NEW location
               // AColor::Line includes both endpoints so use GetBottom()
               AColor::Line(dc,
                            mLastIndicatorX,
                            rect.GetTop(),
                            mLastIndicatorX,
                            rect.GetBottom());
            }
         );
      } );
   }
   else if(auto ruler = dynamic_cast<AdornedRulerPanel*>(&panel)) {
      wxASSERT(!mIsMaster);

      ruler->DoDrawIndicator(&dc, mLastIndicatorX, !rec, IndicatorMediumWidth, false, false);
   }
   else
      wxASSERT(false);
}

PlayIndicatorOverlay::PlayIndicatorOverlay(AudacityProject *project)
: PlayIndicatorOverlayBase(project, true)
{
   mProject->GetPlaybackScroller().Bind(EVT_TRACK_PANEL_TIMER,
      &PlayIndicatorOverlay::OnTimer,
      this);
}

void PlayIndicatorOverlay::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   // Ensure that there is an overlay attached to the ruler
   if (!mPartner) {
      auto ruler = mProject->GetRulerPanel();
      if (ruler) {
         mPartner = std::make_shared<PlayIndicatorOverlayBase>(mProject, false);
         ruler->AddOverlay( mPartner );
      }
   }

   auto trackPanel = mProject->GetTrackPanel();
   int width;
   trackPanel->GetTracksUsableArea(&width, nullptr);

   if (!mProject->IsAudioActive()) {
      mNewIndicatorX = -1;
      mNewIsCapturing = false;
      const auto &scrubber = mProject->GetScrubber();
      if (scrubber.HasMark()) {
         auto position = scrubber.GetScrubStartPosition();
         const auto offset = trackPanel->GetLeftOffset();
         if(position >= trackPanel->GetLeftOffset() &&
            position < offset + width)
            mNewIndicatorX = position;
      }
   }
   else {
      ViewInfo &viewInfo = mProject->GetViewInfo();

      // Calculate the horizontal position of the indicator
      const double playPos = viewInfo.mRecentStreamTime;

      using Mode = AudacityProject::PlaybackScroller::Mode;
      const Mode mode = mProject->GetPlaybackScroller().GetMode();
      const bool pinned = ( mode == Mode::Pinned || mode == Mode::Right );

      // Use a small tolerance to avoid flicker of play head pinned all the way
      // left or right
      const auto tolerance = pinned ? 1.5 * kTimerInterval / 1000.0 : 0;
      bool onScreen = playPos >= 0.0 &&
         between_incexc(viewInfo.h - tolerance,
         playPos,
         mProject->GetTrackPanel()->GetScreenEndTime() + tolerance);

      // This displays the audio time, too...
      mProject->TP_DisplaySelection();

      // BG: Scroll screen if option is set
      if( viewInfo.bUpdateTrackIndicator &&
          playPos >= 0 && !onScreen ) {
         // msmeyer: But only if not playing looped or in one-second mode
         // PRL: and not scrolling with play/record head fixed
         if (!pinned &&
             mProject->mLastPlayMode != PlayMode::loopedPlay &&
             mProject->mLastPlayMode != PlayMode::oneSecondPlay &&
             !gAudioIO->IsPaused())
         {
            auto newPos = playPos;
            if (playPos < viewInfo.h) {
               // This is possible when scrubbing backwards.
               // We want to page leftward by (at least) a whole screen, not
               // just a little bit equal to the scrubbing poll interval
               // duration.
               newPos = viewInfo.OffsetTimeByPixels( newPos, -width );
               newPos = std::max( newPos, mProject->ScrollingLowerBoundTime() );
            }
            mProject->TP_ScrollWindow(newPos);
            // Might yet be off screen, check it
            onScreen = playPos >= 0.0 &&
            between_incexc(viewInfo.h,
                           playPos,
                           mProject->GetTrackPanel()->GetScreenEndTime());
         }
      }

      // Always update scrollbars even if not scrolling the window. This is
      // important when NEW audio is recorded, because this can change the
      // length of the project and therefore the appearance of the scrollbar.
      mProject->TP_RedrawScrollbars();

      if (onScreen)
         mNewIndicatorX = viewInfo.TimeToPosition(playPos, trackPanel->GetLeftOffset());
      else
         mNewIndicatorX = -1;

      mNewIsCapturing = gAudioIO->IsCapturing();
   }

   if(mPartner)
      mPartner->Update(mNewIndicatorX);
}
