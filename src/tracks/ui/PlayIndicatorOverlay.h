/**********************************************************************

Audacity: A Digital Audio Editor

PlayIndicatorOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_PLAY_INDICATOR_OVERLAY__
#define __AUDACITY_PLAY_INDICATOR_OVERLAY__

#include "../../TrackPanelOverlay.h"
#include <wx/event.h>

class AudacityProject;


class PlayIndicatorOverlay final : public wxEvtHandler, public TrackPanelOverlay
{
public:
   PlayIndicatorOverlay(AudacityProject *project);
   virtual ~PlayIndicatorOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw
      (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end) override;
   void Erase(wxDC &dc, wxDC &src) override;

   void OnTimer(wxCommandEvent &event);


   AudacityProject *mProject;
   int mLastIndicatorX;
   int mNewIndicatorX;
};

#endif
