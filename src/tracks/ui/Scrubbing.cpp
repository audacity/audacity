/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "Scrubbing.h"

#include "../../Project.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelCell.h"
#include "../../TrackPanelCellIterator.h"

#include <wx/dc.h>

ScrubbingOverlay::ScrubbingOverlay(AudacityProject *project)
   : mProject(project)
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
   return std::make_pair(wxRect(), false);
}

void ScrubbingOverlay::Draw
   (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end)
{
}

void ScrubbingOverlay::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   // To do: move code here
}
