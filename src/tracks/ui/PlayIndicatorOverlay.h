/**********************************************************************

Sneedacity: A Digital Audio Editor

PlayIndicatorOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_PLAY_INDICATOR_OVERLAY__
#define __SNEEDACITY_PLAY_INDICATOR_OVERLAY__

#include <wx/event.h> // to inherit
#include <memory>
#include "../../ClientData.h"
#include "../../widgets/Overlay.h" // to inherit

class SneedacityProject;


// Common class for overlaying track panel or ruler
class PlayIndicatorOverlayBase
   : public wxEvtHandler
   , public Overlay
   , public ClientData::Base
{
public:
   PlayIndicatorOverlayBase(SneedacityProject *project, bool isMaster);
   virtual ~PlayIndicatorOverlayBase();

   void Update(int newIndicatorX) { mNewIndicatorX = newIndicatorX; }

private:
   unsigned SequenceNumber() const override;
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

protected:

   SneedacityProject *const mProject;
   const bool mIsMaster;
   int mLastIndicatorX { -1 };
   int mNewIndicatorX { -1 };
   bool mNewIsCapturing { false };
   bool mLastIsCapturing { false };
};

// Master object for track panel, creates the other object for the ruler
class PlayIndicatorOverlay final : public PlayIndicatorOverlayBase
{
public:
   explicit
   PlayIndicatorOverlay(SneedacityProject *project);

private:
   void OnTimer(wxCommandEvent &event);

   std::shared_ptr<PlayIndicatorOverlayBase> mPartner;
};

#endif
