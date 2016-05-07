/**********************************************************************

Audacity: A Digital Audio Editor

EditCursorOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_EDIT_CURSOR_OVERLAY__
#define __AUDACITY_EDIT_CURSOR_OVERLAY__

#include "../../widgets/Overlay.h"

class AudacityProject;

class EditCursorOverlay final : public Overlay
{
public:
   EditCursorOverlay(AudacityProject *project);
   virtual ~EditCursorOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   AudacityProject *mProject;

   int mLastCursorX;
   double mCursorTime;
   int mNewCursorX;
};

#endif
