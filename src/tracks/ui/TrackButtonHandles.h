/**********************************************************************

Audacity: A Digital Audio Editor

WavelTrackButtonHandles.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_BUTTON_HANDLES__
#define __AUDACITY_TRACK_BUTTON_HANDLES__

#include "../ui/ButtonHandle.h"

struct HitTestResult;

class MinimizeButtonHandle final : public ButtonHandle
{
   MinimizeButtonHandle(const MinimizeButtonHandle&) = delete;
   MinimizeButtonHandle &operator=(const MinimizeButtonHandle&) = delete;

   MinimizeButtonHandle();
   virtual ~MinimizeButtonHandle();
   static MinimizeButtonHandle& Instance();

protected:
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override;

public:
   static HitTestResult HitTest(const wxMouseEvent &event, const wxRect &rect);
};

////////////////////////////////////////////////////////////////////////////////

class CloseButtonHandle final : public ButtonHandle
{
   CloseButtonHandle(const CloseButtonHandle&) = delete;
   CloseButtonHandle &operator=(const CloseButtonHandle&) = delete;

   CloseButtonHandle();
   virtual ~CloseButtonHandle();
   static CloseButtonHandle& Instance();

protected:
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override;

   bool StopsOnKeystroke () override { return true; }
   
public:
   static HitTestResult HitTest(const wxMouseEvent &event, const wxRect &rect);
};

////////////////////////////////////////////////////////////////////////////////

#include <wx/event.h>
#include "../../widgets/PopupMenuTable.h"

class MenuButtonHandle final : public ButtonHandle
{
   MenuButtonHandle(const MenuButtonHandle&) = delete;
   MenuButtonHandle &operator=(const MenuButtonHandle&) = delete;

   MenuButtonHandle();
   virtual ~MenuButtonHandle();
   static MenuButtonHandle& Instance();

protected:
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override;

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const std::shared_ptr<TrackPanelCell> &pCell);

private:
   std::weak_ptr<TrackPanelCell> mpCell;
};

#endif
