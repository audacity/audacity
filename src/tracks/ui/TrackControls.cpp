/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackControls.h"
#include "TrackButtonHandles.h"
#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelMouseEvent.h"

int TrackControls::gCaptureState;

TrackControls::~TrackControls()
{
}

HitTestResult TrackControls::HitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   HitTestResult result;

   if (NULL != (result = CloseButtonHandle::HitTest(event, rect)).handle)
      return result;

   if (NULL != (result = MenuButtonHandle::HitTest(event, rect, this)).handle)
      return result;

   if (NULL != (result = MinimizeButtonHandle::HitTest(event, rect)).handle)
      return result;

   return result;
}

Track *TrackControls::FindTrack()
{
   return GetTrack();
}

class TrackMenuTable : public PopupMenuTable
{
   TrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(TrackMenuTable);

public:
   static TrackMenuTable &Instance();

private:

   void InitMenu(Menu*, void *pUserData) override
   {
      mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;
};

TrackMenuTable &TrackMenuTable::Instance()
{
   static TrackMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(TrackMenuTable)
END_POPUP_MENU()

unsigned TrackControls::DoContextMenu
   (const wxRect &rect, wxWindow *pParent, wxPoint *)
{
   wxRect buttonRect;
   TrackInfo::GetTitleBarRect(rect, buttonRect);

   InitMenuData data{ mpTrack, pParent, RefreshCode::RefreshNone };

   const auto pTable = &TrackMenuTable::Instance();
   auto pMenu = PopupMenuTable::BuildMenu(pParent, pTable, &data);

   PopupMenuTable *const pExtension = GetMenuExtension(mpTrack);
   if (pExtension)
      pMenu->Extend(pExtension);

   pParent->PopupMenu
      (pMenu.get(), buttonRect.x + 1, buttonRect.y + buttonRect.height + 1);

   return data.result;
}
