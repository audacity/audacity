/**********************************************************************

Audacity: A Digital Audio Editor

EditCursorOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_EDIT_CURSOR_OVERLAY__
#define __AUDACITY_EDIT_CURSOR_OVERLAY__

#include "../../MemoryX.h"
#include "../../widgets/Overlay.h"

class AudacityProject;

class EditCursorOverlay final : public Overlay
{
public:
   EditCursorOverlay(AudacityProject *project, bool isMaster = true);
   virtual ~EditCursorOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   AudacityProject *mProject;
   bool mIsMaster;
   std::unique_ptr<EditCursorOverlay> mPartner;

   int mLastCursorX;
   double mCursorTime;
   int mNewCursorX;
};

#endif
