/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TimeTrackControls.h"

#include "../../../HitTestResult.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "TimeTrack.h"
#include "../../../widgets/PopupMenuTable.h"
#include <wx/numdlg.h>

TimeTrackControls::~TimeTrackControls()
{
}

std::vector<UIHandlePtr> TimeTrackControls::HitTest
(const TrackPanelMouseState & state,
 const AudacityProject *pProject)
{
   return CommonTrackControls::HitTest(state, pProject);
}

enum
{
   OnTimeTrackLinID = 30000,
   OnTimeTrackLogID,
   OnTimeTrackLogIntID,
   OnSetTimeTrackRangeID,
};

TimeTrackMenuTable &TimeTrackMenuTable::Instance()
{
   static TimeTrackMenuTable instance;
   return instance;
}

void TimeTrackMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<CommonTrackControls::InitMenuData*>(pUserData);
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
         TimeTrackControls::kRangeMin,
         TimeTrackControls::kRangeMax);

      upper = wxGetNumberFromUser(_("Change upper speed limit (%) to:"),
         _("Upper speed limit"),
         _("Upper speed limit"),
         upper,
         lower + 1,
         TimeTrackControls::kRangeMax);

      if (lower >= TimeTrackControls::kRangeMin &&
          upper <= TimeTrackControls::kRangeMax &&
          lower < upper) {
         AudacityProject *const project = &mpData->project;
         pTrack->SetRangeLower((double)lower / 100.0);
         pTrack->SetRangeUpper((double)upper / 100.0);
         ProjectHistory::Get( *project )
            .PushState(XO("Set range to '%ld' - '%ld'").Format( lower, upper ),
               /* i18n-hint: (verb)*/
               XO("Set Range"));
         mpData->result = RefreshCode::RefreshAll;
      }
   }
}

void TimeTrackMenuTable::OnTimeTrackLin(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   pTrack->SetDisplayLog(false);
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project )
      .PushState(XO("Set time track display to linear"), XO("Set Display"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLog(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   pTrack->SetDisplayLog(true);
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project )
      .PushState(XO("Set time track display to logarithmic"), XO("Set Display"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLogInt(wxCommandEvent & /*event*/)
{
   TimeTrack *const pTrack = static_cast<TimeTrack*>(mpData->pTrack);
   AudacityProject *const project = &mpData->project;
   if (pTrack->GetInterpolateLog()) {
      pTrack->SetInterpolateLog(false);
      ProjectHistory::Get( *project )
         .PushState(XO("Set time track interpolation to linear"), XO("Set Interpolation"));
   }
   else {
      pTrack->SetInterpolateLog(true);
      ProjectHistory::Get( *project ).
         PushState(XO("Set time track interpolation to logarithmic"), XO("Set Interpolation"));
   }
   mpData->result = RefreshCode::RefreshAll;
}

BEGIN_POPUP_MENU(TimeTrackMenuTable)
   static const auto findTrack = []( PopupMenuHandler &handler ){
      return static_cast<TimeTrack*>(
         static_cast<TimeTrackMenuTable&>( handler ).mpData->pTrack );
   };

   BeginSection( "Scales" );
      AppendRadioItem( "Linear", OnTimeTrackLinID, XXO("&Linear scale"),
         POPUP_MENU_FN( OnTimeTrackLin ),
         []( PopupMenuHandler &handler, wxMenu &menu, int id ){
            menu.Check( id, !findTrack(handler)->GetDisplayLog() );
         } );
      AppendRadioItem( "Log", OnTimeTrackLogID, XXO("L&ogarithmic scale"),
         POPUP_MENU_FN( OnTimeTrackLog ),
         []( PopupMenuHandler &handler, wxMenu &menu, int id ){
            menu.Check( id, findTrack(handler)->GetDisplayLog() );
         } );
   EndSection();

   BeginSection( "Other" );
      AppendItem( "Range", OnSetTimeTrackRangeID, XXO("&Range..."),
         POPUP_MENU_FN( OnSetTimeTrackRange ) );
      AppendCheckItem( "LogInterp", OnTimeTrackLogIntID,
         XXO("Logarithmic &Interpolation"), POPUP_MENU_FN( OnTimeTrackLogInt),
         []( PopupMenuHandler &handler, wxMenu &menu, int id ){
            menu.Check( id, findTrack(handler)->GetInterpolateLog() );
         } );
   EndSection();

END_POPUP_MENU()

PopupMenuTable *TimeTrackControls::GetMenuExtension(Track *)
{
   return &TimeTrackMenuTable::Instance();
}

using DoGetTimeTrackControls = DoGetControls::Override< TimeTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetTimeTrackControls) {
   return [](TimeTrack &track) {
      return std::make_shared<TimeTrackControls>( track.SharedPointer() );
   };
}

#include "../../ui/TrackView.h"

using GetDefaultTimeTrackHeight = GetDefaultTrackHeight::Override< TimeTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetDefaultTimeTrackHeight) {
   return [](TimeTrack &) {
      return 100;
   };
}
