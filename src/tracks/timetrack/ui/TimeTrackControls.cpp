/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "TimeTrackControls.h"

#include "../../../HitTestResult.h"
#include "../../../Project.h"
#include "../../../RefreshCode.h"
#include "../../../TimeTrack.h"
#include "../../../widgets/PopupMenuTable.h"
#include <wx/numdlg.h>

TimeTrackControls::~TimeTrackControls()
{
}

std::vector<UIHandlePtr> TimeTrackControls::HitTest
(const TrackPanelMouseState & state,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(state, pProject);
}

enum
{
   OnTimeTrackLinID = 30000,
   OnTimeTrackLogID,
   OnTimeTrackLogIntID,
   OnSetTimeTrackRangeID,
};

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
      TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);

      pMenu->Check(OnTimeTrackLogIntID, pTrack->GetInterpolateLog());

      auto isLog = pTrack->GetDisplayLog();
      pMenu->Check(OnTimeTrackLinID, !isLog);
      pMenu->Check(OnTimeTrackLogID, isLog);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;

   void OnSetTimeTrackRange(wxCommandEvent & /*event*/);
   void OnTimeTrackLin(wxCommandEvent & /*event*/);
   void OnTimeTrackLog(wxCommandEvent & /*event*/);
   void OnTimeTrackLogInt(wxCommandEvent & /*event*/);
};

TimeTrackMenuTable &TimeTrackMenuTable::Instance()
{
   static TimeTrackMenuTable instance;
   return instance;
}

void TimeTrackMenuTable::OnSetTimeTrackRange(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   if (pTrack) {
      long lower = (long)(pTrack->GetRangeLower() * 100.0 + 0.5);
      long upper = (long)(pTrack->GetRangeUpper() * 100.0 + 0.5);

      // MB: these lower/upper limits match the maximum allowed range of the time track
      // envelope, but this is not strictly required
      lower = wxGetNumberFromUser(_("Change lower speed limit (%) to:"),
         _("Lower speed limit"),
         _("Lower speed limit"),
         lower,
         10,
         1000);

      upper = wxGetNumberFromUser(_("Change upper speed limit (%) to:"),
         _("Upper speed limit"),
         _("Upper speed limit"),
         upper,
         lower + 1,
         1000);

      if (lower >= 10 && upper <= 1000 && lower < upper) {
         AudacityProject *const project = ::GetActiveProject();
         pTrack->SetRangeLower((double)lower / 100.0);
         pTrack->SetRangeUpper((double)upper / 100.0);
         ProjectManager::Get( *project )
            .PushState(wxString::Format(_("Set range to '%ld' - '%ld'"),
            lower,
            upper),
            /* i18n-hint: (verb)*/
            _("Set Range"));
         mpData->result = RefreshCode::RefreshAll;
      }
   }
}

void TimeTrackMenuTable::OnTimeTrackLin(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   pTrack->SetDisplayLog(false);
   AudacityProject *const project = ::GetActiveProject();
   ProjectManager::Get( *project )
      .PushState(_("Set time track display to linear"), _("Set Display"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLog(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   pTrack->SetDisplayLog(true);
   AudacityProject *const project = ::GetActiveProject();
   ProjectManager::Get( *project )
      .PushState(_("Set time track display to logarithmic"), _("Set Display"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLogInt(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   AudacityProject *const project = ::GetActiveProject();
   if (pTrack->GetInterpolateLog()) {
      pTrack->SetInterpolateLog(false);
      ProjectManager::Get( *project )
         .PushState(_("Set time track interpolation to linear"), _("Set Interpolation"));
   }
   else {
      pTrack->SetInterpolateLog(true);
      ProjectManager::Get( *project ).
         PushState(_("Set time track interpolation to logarithmic"), _("Set Interpolation"));
   }
   mpData->result = RefreshCode::RefreshAll;
}

BEGIN_POPUP_MENU(TimeTrackMenuTable)
   POPUP_MENU_SEPARATOR()
   POPUP_MENU_RADIO_ITEM(OnTimeTrackLinID, _("&Linear scale"), OnTimeTrackLin)
   POPUP_MENU_RADIO_ITEM(OnTimeTrackLogID, _("L&ogarithmic scale"), OnTimeTrackLog)
   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnSetTimeTrackRangeID, _("&Range..."), OnSetTimeTrackRange)
   POPUP_MENU_CHECK_ITEM(OnTimeTrackLogIntID, _("Logarithmic &Interpolation"), OnTimeTrackLogInt)
END_POPUP_MENU()

PopupMenuTable *TimeTrackControls::GetMenuExtension(Track *)
{
   return &TimeTrackMenuTable::Instance();
}
