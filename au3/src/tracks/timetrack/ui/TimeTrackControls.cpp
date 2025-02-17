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
#include "ShuttleGui.h"
#include "wxPanelWrapper.h"
#include <wx/spinctrl.h>
#include "AudacityMessageBox.h"

TimeTrackControls::~TimeTrackControls()
{
}

std::vector<UIHandlePtr> TimeTrackControls::HitTest
    (const TrackPanelMouseState& state,
    const AudacityProject* pProject)
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

TimeTrackMenuTable& TimeTrackMenuTable::Instance()
{
    static TimeTrackMenuTable instance;
    return instance;
}

void TimeTrackMenuTable::InitUserData(void* pUserData)
{
    mpData = static_cast<CommonTrackControls::InitMenuData*>(pUserData);
}

void TimeTrackMenuTable::OnSetTimeTrackRange(wxCommandEvent& /*event*/)
{
    auto& track = static_cast<TimeTrack&>(mpData->track);
    long lower = (long)(track.GetRangeLower() * 100.0 + 0.5);
    long upper = (long)(track.GetRangeUpper() * 100.0 + 0.5);

    // MB: these lower/upper limits match the maximum allowed range of the time track
    // envelope, but this is not strictly required

    wxDialogWrapper dlg(mpData->pParent, wxID_ANY, XO("Change Speed Limits"));
    dlg.SetName();
    ShuttleGui S(&dlg, eIsCreating);
    wxSpinCtrl* scLower;
    wxSpinCtrl* scUpper;

    S.StartVerticalLay(true);
    {
        S.StartStatic(XO("Change speed limit (%) to:"), 1);
        {
            S.StartMultiColumn(2, wxEXPAND);
            {
                S.SetStretchyCol(1);

                S.AddPrompt(XXO("Lower Speed Limit"));
                scLower = safenew wxSpinCtrl(S.GetParent(), wxID_ANY,
                                             wxT(""),
                                             wxDefaultPosition,
                                             wxDefaultSize,
                                             wxSP_ARROW_KEYS,
                                             TimeTrackControls::kRangeMin, TimeTrackControls::kRangeMax, lower);
                S
                .Name(XO("Lower Speed Limit"))
                .Position(wxALIGN_LEFT | wxALL)
                .AddWindow(scLower);

                S.AddPrompt(XXO("Upper Speed Limit"));
                scUpper = safenew wxSpinCtrl(S.GetParent(), wxID_ANY,
                                             wxT(""),
                                             wxDefaultPosition,
                                             wxDefaultSize,
                                             wxSP_ARROW_KEYS,
                                             TimeTrackControls::kRangeMin, TimeTrackControls::kRangeMax, upper);
                S
                .Name(XO("Upper Speed Limit"))
                .Position(wxALIGN_LEFT | wxALL)
                .AddWindow(scUpper);
            }
            S.EndMultiColumn();
        }
        S.EndStatic();
        S.AddStandardButtons();
    }
    S.EndVerticalLay();

    dlg.Layout();
    dlg.Fit();
    dlg.CenterOnParent();
    if (dlg.ShowModal() == wxID_CANCEL) {
        return;
    }

    while (scLower->GetValue() >= scUpper->GetValue()) {
        AudacityMessageBox(
            XO("Upper Speed Limit must be greater than the Lower Speed Limit"),
            XO("Invalid Limits"),
            wxOK | wxICON_ERROR,
            mpData->pParent);

        if (dlg.ShowModal() == wxID_CANCEL) {
            return;
        }
    }

    lower = scLower->GetValue();
    upper = scUpper->GetValue();

    if (lower >= TimeTrackControls::kRangeMin
        && upper <= TimeTrackControls::kRangeMax) {
        AudacityProject* const project = &mpData->project;
        track.SetRangeLower((double)lower / 100.0);
        track.SetRangeUpper((double)upper / 100.0);
        ProjectHistory::Get(*project)
        .PushState(XO("Set range to '%ld' - '%ld'").Format(lower, upper),
                   /* i18n-hint: (verb)*/
                   XO("Set Range"));
        mpData->result = RefreshCode::RefreshAll;
    }
}

void TimeTrackMenuTable::OnTimeTrackLin(wxCommandEvent& /*event*/)
{
    auto& track = static_cast<TimeTrack&>(mpData->track);
    track.SetDisplayLog(false);
    AudacityProject* const project = &mpData->project;
    ProjectHistory::Get(*project)
    .PushState(XO("Set time track display to linear"), XO("Set Display"));

    using namespace RefreshCode;
    mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLog(wxCommandEvent& /*event*/)
{
    auto& track = static_cast<TimeTrack&>(mpData->track);
    track.SetDisplayLog(true);
    AudacityProject* const project = &mpData->project;
    ProjectHistory::Get(*project)
    .PushState(XO("Set time track display to logarithmic"), XO("Set Display"));

    using namespace RefreshCode;
    mpData->result = RefreshAll | UpdateVRuler;
}

void TimeTrackMenuTable::OnTimeTrackLogInt(wxCommandEvent& /*event*/)
{
    auto& track = static_cast<TimeTrack&>(mpData->track);
    AudacityProject* const project = &mpData->project;
    if (track.GetInterpolateLog()) {
        track.SetInterpolateLog(false);
        ProjectHistory::Get(*project)
        .PushState(XO("Set time track interpolation to linear"), XO("Set Interpolation"));
    } else {
        track.SetInterpolateLog(true);
        ProjectHistory::Get(*project).
        PushState(XO("Set time track interpolation to logarithmic"), XO("Set Interpolation"));
    }
    mpData->result = RefreshCode::RefreshAll;
}

BEGIN_POPUP_MENU(TimeTrackMenuTable)
static const auto findTrack
    =[](PopupMenuHandler& handler) -> TimeTrack& {
    return static_cast<TimeTrack&>(
        static_cast<TimeTrackMenuTable&>(handler).mpData->track);
};

BeginSection("Scales");
AppendRadioItem("Linear", OnTimeTrackLinID, XXO("&Linear scale"),
                POPUP_MENU_FN(OnTimeTrackLin),
                []( PopupMenuHandler& handler, wxMenu& menu, int id ){
    menu.Check(id, !findTrack(handler).GetDisplayLog());
});
AppendRadioItem("Log", OnTimeTrackLogID, XXO("L&ogarithmic scale"),
                POPUP_MENU_FN(OnTimeTrackLog),
                []( PopupMenuHandler& handler, wxMenu& menu, int id ){
    menu.Check(id, findTrack(handler).GetDisplayLog());
});
EndSection();

BeginSection("Other");
AppendItem("Range", OnSetTimeTrackRangeID, XXO("&Range..."),
           POPUP_MENU_FN(OnSetTimeTrackRange));
AppendCheckItem("LogInterp", OnTimeTrackLogIntID,
                XXO("Logarithmic &Interpolation"), POPUP_MENU_FN(OnTimeTrackLogInt),
                []( PopupMenuHandler& handler, wxMenu& menu, int id ){
    menu.Check(id, findTrack(handler).GetInterpolateLog());
});
EndSection();

END_POPUP_MENU()

PopupMenuTable* TimeTrackControls::GetMenuExtension(Track*)
{
    return &TimeTrackMenuTable::Instance();
}

using DoGetTimeTrackControls = DoGetControls::Override< TimeTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetTimeTrackControls) {
    return [](TimeTrack& track) {
        return std::make_shared<TimeTrackControls>(track.SharedPointer());
    };
}

#include "../../ui/ChannelView.h"

using GetDefaultTimeTrackHeight = GetDefaultTrackHeight::Override< TimeTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetDefaultTimeTrackHeight) {
    return [](TimeTrack&) {
        return 100;
    };
}
