/**********************************************************************

Audacity: A Digital Audio Editor

Scrubbing.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SCRUBBING__
#define __AUDACITY_SCRUBBING__

#include <wx/event.h>

#include "../../TrackPanelOverlay.h"

class AudacityProject;

class ScrubbingOverlay : public wxEvtHandler, public TrackPanelOverlay
{
public:
   ScrubbingOverlay(AudacityProject *project);
   virtual ~ScrubbingOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw
      (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end) override;

   void OnTimer(wxCommandEvent &event);

   AudacityProject *mProject;
};

#endif
