/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeSignatureToolBar.cpp

  Dmitry Vedenko

*******************************************************************/

#include "TimeSignatureToolBar.h"

#include <algorithm>
#include <cassert>

#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/combobox.h>

#include "ToolManager.h"

#include "widgets/BasicMenu.h"
#include "wxWidgetsWindowPlacement.h"

#include "Prefs.h"
#include "Project.h"
#include "ViewInfo.h"

#include "AllThemeResources.h"
#include "AudioIO.h"

#include "ProjectTimeSignature.h"
#include "wxArrayStringEx.h"

#include "ProjectHistory.h"
#include "UndoManager.h"

#include "SpinControl.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

IMPLEMENT_CLASS(TimeSignatureToolBar, ToolBar);

BEGIN_EVENT_TABLE(TimeSignatureToolBar, ToolBar)
EVT_SIZE(TimeSignatureToolBar::OnSize)
END_EVENT_TABLE()

Identifier TimeSignatureToolBar::ID()
{
    return wxT("TimeSignature");
}

TimeSignatureToolBar::TimeSignatureToolBar(AudacityProject& project)
    : ToolBar(project, XO("Time Signature"), ID())
    , mTimeSignatureChangedSubscription(ProjectTimeSignature::Get(mProject).Subscribe(
                                            [this](auto settings)
{
    if (mTempoControl)
        mTempoControl->SetValue(settings.newTempo);

    if (mUpperSignatureControl)
        mUpperSignatureControl->SetValue(
            settings.newUpperTimeSignature);

    if (mLowerSignatureControl)
        mLowerSignatureControl->SetValue(
            wxString::Format("%d", settings.newLowerTimeSignature));
})),
    mPlaybackStateChangedSubscription(AudioIO::Get()->Subscribe(*this, &TimeSignatureToolBar::OnAudioIOEvent))
{
}

TimeSignatureToolBar::~TimeSignatureToolBar() = default;

bool TimeSignatureToolBar::ShownByDefault() const
{
    return true;
}

ToolBar::DockID TimeSignatureToolBar::DefaultDockID() const
{
    return BotDockID;
}

TimeSignatureToolBar& TimeSignatureToolBar::Get(AudacityProject& project)
{
    auto& toolManager = ToolManager::Get(project);
    return *static_cast<TimeSignatureToolBar*>(toolManager.GetToolBar(ID()));
}

const TimeSignatureToolBar& TimeSignatureToolBar::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void TimeSignatureToolBar::Create(wxWindow* parent)
{
    ToolBar::Create(parent);
    UpdatePrefs();
}

void TimeSignatureToolBar::Populate()
{
#ifndef __WXGTK__
    const auto tempoSigSize = wxSize(60, -1);
    auto timeSigSize = wxSize(50, -1);
#else
    const auto tempoSigSize = wxSize(60, -1);
    auto timeSigSize = wxSize(60, -1);
#endif

    auto& projectTimeSignature = ProjectTimeSignature::Get(mProject);

    SetBackgroundColour(theTheme.Colour(clrMedium));

    auto sizer = safenew wxFlexGridSizer(2, 5, 1);
    Add(sizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

    AddTitle(XO("Tempo"), sizer);
    AddTitle(XO("Time Signature"), sizer);

    mTempoControl = safenew SpinControl(
        this, wxID_ANY, projectTimeSignature.GetTempo(), 1.0, 999.0, 1.0, true,
        wxDefaultPosition, tempoSigSize);

    mTempoControl->SetName(XO("Tempo"));

    sizer->Add(mTempoControl, 0, wxEXPAND | wxRIGHT, 5);

    auto tempoSizer = safenew wxBoxSizer(wxHORIZONTAL);
    sizer->Add(tempoSizer, 0, wxEXPAND | wxRIGHT, 5);

    mUpperSignatureControl = safenew SpinControl(
        this, wxID_ANY, projectTimeSignature.GetUpperTimeSignature(), 1.0, 128.0, 1.0, false,
        wxDefaultPosition, timeSigSize);

    mUpperSignatureControl->SetName(XO("Upper Time Signature"));

    tempoSizer->Add(mUpperSignatureControl, 0, wxEXPAND | wxRIGHT, 5);

    AddTitle(
        Verbatim(L"/"), tempoSizer, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5, 1.5);

    timeSigSize.y = mUpperSignatureControl->GetSize().y;

    mLowerSignatureControl = safenew wxComboBox(
        this, wxID_ANY,
        wxString::Format(
            "%d", ProjectTimeSignature::Get(mProject).GetLowerTimeSignature()),
        wxDefaultPosition, timeSigSize,
        wxArrayStringEx { L"2", L"4", L"8", L"16", L"32", L"64" },
        wxCB_READONLY);

    mLowerSignatureControl->SetName(XO("Lower Time Signature").Translation());

    tempoSizer->Add(mLowerSignatureControl, 0, wxEXPAND | wxRIGHT, 5);

    mTempoControl->Bind(
        wxEVT_SPINCTRL,
        [this](auto)
    {
        const auto tempo = mTempoControl->GetValue();

        ProjectTimeSignature::Get(mProject).SetTempo(tempo);

        ProjectHistory::Get(mProject).PushState(
            XO("Tempo Changed"), XO("Tempo Changed"), UndoPush::CONSOLIDATE);
    });

    mUpperSignatureControl->Bind(
        wxEVT_SPINCTRL,
        [this](auto)
    {
        const auto upper = int(mUpperSignatureControl->GetValue());

        ProjectTimeSignature::Get(mProject).SetUpperTimeSignature(upper);

        ProjectHistory::Get(mProject).PushState(
            XO("Upper Time Signature Changed"),
            XO("Upper Time Signature Changed"), UndoPush::CONSOLIDATE);
    });

    mLowerSignatureControl->Bind(
        wxEVT_COMBOBOX,
        [this](auto)
    {
        long value;

        if (!mLowerSignatureControl->GetValue().ToLong(&value)) {
            return;
        }

        ProjectTimeSignature::Get(mProject).SetLowerTimeSignature(value);

        ProjectHistory::Get(mProject).PushState(
            XO("Lower Time Signature Changed"),
            XO("Lower Time Signature Changed"), UndoPush::CONSOLIDATE);
    });

#if wxUSE_ACCESSIBILITY
    mTempoControl->SetAccessible(safenew WindowAccessible(mTempoControl));
    mUpperSignatureControl->SetAccessible(
        safenew WindowAccessible(mUpperSignatureControl));
    mLowerSignatureControl->SetAccessible(
        safenew WindowAccessible(mLowerSignatureControl));
#endif

    RegenerateTooltips();
    Fit();
    Layout();
}

void TimeSignatureToolBar::UpdatePrefs()
{
    // Set label to pull in language change
    SetLabel(XO("Time Signature"));

    RegenerateTooltips();
    // Give base class a chance
    ToolBar::UpdatePrefs();
}

void TimeSignatureToolBar::RegenerateTooltips()
{
}

void TimeSignatureToolBar::OnSize(wxSizeEvent& evt)
{
    Refresh(true);

    evt.Skip();
}

void TimeSignatureToolBar::OnAudioIOEvent(const AudioIOEvent& event)
{
    switch (event.type) {
    case AudioIOEvent::PLAYBACK:
    case AudioIOEvent::CAPTURE:
    {
        if (mTempoControl) {
            mTempoControl->Enable(!event.on);
        }
    } break;
    default: break;
    }
}

void TimeSignatureToolBar::AddTitle(
    const TranslatableString& Title, wxSizer* pSizer, int flags, int border,
    double fontMultiplier)
{
    const auto translated = Title.Translation();

    auStaticText* pTitle = safenew auStaticText(this, translated);

    pTitle->SetBackgroundColour(theTheme.Colour(clrMedium));
    pTitle->SetForegroundColour(theTheme.Colour(clrTrackPanelText));
    pTitle->ScaleFont(fontMultiplier);

    pSizer->Add(pTitle, 0, flags, border);
}

static RegisteredToolbarFactory factory{
    []( AudacityProject& project ){
        return ToolBar::Holder { safenew TimeSignatureToolBar { project } };
    } };

namespace {
AttachedToolBarMenuItem sAttachment{
    /* i18n-hint: Clicking this menu item shows the toolbar
       for selecting a time range of audio */
    TimeSignatureToolBar::ID(), wxT("ShowTimeSignatureTB"), XXO("Time Signature Toolbar")
};

// Undo/redo handling of time signature changes
// DV: where should this really go?

struct TimeSignatureRestorer final : UndoStateExtension
{
    explicit TimeSignatureRestorer(AudacityProject& project)
        : mTempo{ProjectTimeSignature::Get(project).GetTempo()}
        , mUpper{ProjectTimeSignature::Get(project).GetUpperTimeSignature()}
        , mLower{ProjectTimeSignature::Get(project).GetLowerTimeSignature()}
    {
    }

    void RestoreUndoRedoState(AudacityProject& project) override
    {
        auto& timeSignature = ProjectTimeSignature::Get(project);

        timeSignature.SetTempo(mTempo);
        timeSignature.SetUpperTimeSignature(mUpper);
        timeSignature.SetLowerTimeSignature(mLower);
    }

    double mTempo;
    int mUpper;
    int mLower;
};

UndoRedoExtensionRegistry::Entry sEntry {
    [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension>
    { return std::make_shared<TimeSignatureRestorer>(project); }
};
}
