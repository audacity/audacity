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
}

PlayIndicatorOverlay::PlayIndicatorOverlay(AudacityProject *project)
   : mProject(project)
   , mLastIndicatorX(-1)
   , mNewIndicatorX(-1)
{
   mProject->Connect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(PlayIndicatorOverlay::OnTimer),
      NULL,
      this);
}

PlayIndicatorOverlay::~PlayIndicatorOverlay()
{
   mProject->Disconnect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(PlayIndicatorOverlay::OnTimer),
      NULL,
      this);
}

std::pair<wxRect, bool> PlayIndicatorOverlay::DoGetRectangle(wxSize size)
{
   wxRect rect(mLastIndicatorX, 0, 1, size.GetHeight());
   return std::make_pair(
      rect,
      mLastIndicatorX != mNewIndicatorX
   );
}


void PlayIndicatorOverlay::Draw
   (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end)
{
   mLastIndicatorX = mNewIndicatorX;
   if (!between_incexc(0, mLastIndicatorX, dc.GetSize().GetWidth()))
      return;

   const ZoomInfo &viewInfo = mProject->GetZoomInfo();
   TrackPanel *const trackPanel = mProject->GetTrackPanel();

   double pos = viewInfo.PositionToTime(mLastIndicatorX, trackPanel->GetLeftOffset());

   // Set play/record color
   bool rec = (gAudioIO->GetNumCaptureChannels() > 0);
   AColor::IndicatorColor(&dc, !rec);

   mProject->GetRulerPanel()->DrawIndicator(pos, rec);

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

void PlayIndicatorOverlay::Erase(wxDC &dc, wxDC &src)
{
   TrackPanelOverlay::Erase(dc, src);
   mProject->GetRulerPanel()->ClearIndicator();
}

void PlayIndicatorOverlay::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   if (!mProject->IsAudioActive()) {
      const auto &scrubber = mProject->GetScrubber();
      if (scrubber.HasStartedScrubbing())
         mNewIndicatorX = scrubber.GetScrubStartPosition();
      else
         mNewIndicatorX = -1;
   }
   else {
      ViewInfo &viewInfo = mProject->GetViewInfo();

      // Calculate the horizontal position of the indicator
      const double playPos = viewInfo.mRecentStreamTime;

      const bool onScreen = playPos >= 0.0 &&
         between_incexc(viewInfo.h,
         playPos,
         mProject->GetScreenEndTime());

      // This displays the audio time, too...
      mProject->TP_DisplaySelection();

      // BG: Scroll screen if option is set
      // msmeyer: But only if not playing looped or in one-second mode
      if (viewInfo.bUpdateTrackIndicator &&
          mProject->mLastPlayMode != PlayMode::loopedPlay &&
          mProject->mLastPlayMode != PlayMode::oneSecondPlay &&
         playPos >= 0 &&
         !onScreen &&
         !gAudioIO->IsPaused())
      {
         mProject->TP_ScrollWindow(playPos);
      }

      // Always update scrollbars even if not scrolling the window. This is
      // important when NEW audio is recorded, because this can change the
      // length of the project and therefore the appearance of the scrollbar.
      mProject->TP_RedrawScrollbars();

      mNewIndicatorX = viewInfo.TimeToPosition(playPos, mProject->GetTrackPanel()->GetLeftOffset());
   }
}
