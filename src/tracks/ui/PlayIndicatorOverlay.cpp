/**********************************************************************

Audacity: A Digital Audio Editor

PlayIndicatorOverlay.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "PlayIndicatorOverlay.h"

#include "../../AColor.h"
#include "../../AudioIO.h"
#include "../../Project.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelCell.h"
#include "../../TrackPanelCellIterator.h"
#include "../../widgets/Ruler.h"
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
   return std::make_pair(
      rect,
      mLastIndicatorX != mNewIndicatorX
   );
}


void PlayIndicatorOverlayBase::Draw(OverlayPanel &panel, wxDC &dc)
{
   // Set play/record color
   bool rec = (gAudioIO->GetNumCaptureChannels() > 0);
   AColor::IndicatorColor(&dc, !rec);
   mLastIndicatorX = mNewIndicatorX;
   if (!between_incexc(0, mLastIndicatorX, dc.GetSize().GetWidth()))
      return;

   if(auto tp = dynamic_cast<TrackPanel*>(&panel)) {
      wxASSERT(mIsMaster);

      TrackPanelCellIterator begin(tp, true);
      TrackPanelCellIterator end(tp, false);

      // Draw indicator in all visible tracks
      for (; begin != end; ++begin)
      {
         TrackPanelCellIterator::value_type data(*begin);
         Track *const pTrack = data.first;
         if (!pTrack)
            continue;

         // Don't draw the indicator in label tracks
         if (pTrack->GetKind() == Track::Label)
         {
            continue;
         }

         // Draw the NEW indicator in its NEW location
         // AColor::Line includes both endpoints so use GetBottom()
         const wxRect &rect = data.second;
         AColor::Line(dc,
                      mLastIndicatorX,
                      rect.GetTop(),
                      mLastIndicatorX,
                      rect.GetBottom());
      }
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
   mProject->Connect(EVT_TRACK_PANEL_TIMER,
                     wxCommandEventHandler(PlayIndicatorOverlay::OnTimer),
                     NULL,
                     this);
}

PlayIndicatorOverlay::~PlayIndicatorOverlay()
{
   if (mPartner) {
      auto ruler = mProject->GetRulerPanel();
      if(ruler)
         ruler->RemoveOverlay(mPartner.get());
   }

   mProject->Disconnect(EVT_TRACK_PANEL_TIMER,
                        wxCommandEventHandler(PlayIndicatorOverlay::OnTimer),
                        NULL,
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
         mPartner = std::make_unique<PlayIndicatorOverlayBase>(mProject, false);
         ruler->AddOverlay(mPartner.get());
      }
   }

   auto trackPanel = mProject->GetTrackPanel();

   if (!mProject->IsAudioActive()) {
      mNewIndicatorX = -1;
      const auto &scrubber = mProject->GetScrubber();
      if (scrubber.HasStartedScrubbing()) {
         auto position = scrubber.GetScrubStartPosition();
         int width;
         trackPanel->GetTracksUsableArea(&width, nullptr);
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

      bool onScreen = playPos >= 0.0 &&
         between_incexc(viewInfo.h,
         playPos,
         mProject->GetScreenEndTime());

      // This displays the audio time, too...
      mProject->TP_DisplaySelection();

      // BG: Scroll screen if option is set
      // msmeyer: But only if not playing looped or in one-second mode
      // PRL: and not scrolling with play/record head fixed right
      if (viewInfo.bUpdateTrackIndicator &&
          mProject->mLastPlayMode != PlayMode::loopedPlay &&
          mProject->mLastPlayMode != PlayMode::oneSecondPlay &&
          mProject->GetPlaybackScroller().GetMode() !=
             AudacityProject::PlaybackScroller::Mode::Right &&
         playPos >= 0 &&
         !onScreen &&
         !gAudioIO->IsPaused())
      {
         mProject->TP_ScrollWindow(playPos);
         // Might yet be off screen, check it
         onScreen = playPos >= 0.0 &&
            between_incexc(viewInfo.h,
                           playPos,
                           mProject->GetScreenEndTime());
      }

      // Always update scrollbars even if not scrolling the window. This is
      // important when NEW audio is recorded, because this can change the
      // length of the project and therefore the appearance of the scrollbar.
      mProject->TP_RedrawScrollbars();

      if (onScreen)
         mNewIndicatorX = viewInfo.TimeToPosition(playPos, trackPanel->GetLeftOffset());
      else
         mNewIndicatorX = -1;
   }

   if(mPartner)
      mPartner->Update(mNewIndicatorX);
}
