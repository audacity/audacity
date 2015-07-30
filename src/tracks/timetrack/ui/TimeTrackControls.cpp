/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "TimeTrackControls.h"
#include "../../../HitTestResult.h"
#include "../../../TimeTrack.h"
#include "../../../widgets/PopupMenuTable.h"

TimeTrackControls::TimeTrackControls()
{
}

TimeTrackControls &TimeTrackControls::Instance()
{
   static TimeTrackControls instance;
   return instance;
}

TimeTrackControls::~TimeTrackControls()
{
}

HitTestResult TimeTrackControls::HitTest
(const TrackPanelMouseEvent & event,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(event, pProject);
}

class TimeTrackMenuTable : public PopupMenuTable
{
   TimeTrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(TimeTrackMenuTable);

public:
   static TimeTrackMenuTable &Instance();

private:
   void InitMenu(Menu *pMenu, void *pUserData) override
   {
      mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;
};

TimeTrackMenuTable &TimeTrackMenuTable::Instance()
{
   static TimeTrackMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(TimeTrackMenuTable)
END_POPUP_MENU()

PopupMenuTable *TimeTrackControls::GetMenuExtension(Track *)
{
   return &TimeTrackMenuTable::Instance();
}
