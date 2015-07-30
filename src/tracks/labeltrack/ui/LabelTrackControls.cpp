/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelTrackControls.h"
#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../widgets/PopupMenuTable.h"

LabelTrackControls::LabelTrackControls()
{
}

LabelTrackControls &LabelTrackControls::Instance()
{
   static LabelTrackControls instance;
   return instance;
}

LabelTrackControls::~LabelTrackControls()
{
}

HitTestResult LabelTrackControls::HitTest
(const TrackPanelMouseEvent & event,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(event, pProject);
}

class LabelTrackMenuTable : public PopupMenuTable
{
   LabelTrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(LabelTrackMenuTable);

public:
   static LabelTrackMenuTable &Instance();

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

LabelTrackMenuTable &LabelTrackMenuTable::Instance()
{
   static LabelTrackMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(LabelTrackMenuTable)
END_POPUP_MENU()

PopupMenuTable *LabelTrackControls::GetMenuExtension(Track *)
{
   return &LabelTrackMenuTable::Instance();
}
