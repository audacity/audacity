/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni
            2023 Dmitry Vedenko

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class SelectionBar
\brief (not quite a Toolbar) at foot of screen for setting and viewing the
selection range.

*//*******************************************************************/

#include "SelectionBar.h"

#include <algorithm>

#include "ToolManager.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/valtext.h>
#include <wx/stattext.h>
#endif
#include <wx/statline.h>
#include <wx/menu.h>

#include "AudioIO.h"
#include "AColor.h"
#include "../KeyboardCapture.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "ProjectSelectionManager.h"
#include "ProjectTimeSignature.h"
#include "ViewInfo.h"
#include "AllThemeResources.h"
#include "ImageManipulation.h"
#include "../widgets/AButton.h"
#include "../widgets/auStaticText.h"
#include "../widgets/BasicMenu.h"
#include "../widgets/NumericTextCtrl.h"
#include "wxWidgetsWindowPlacement.h"
#include "NumericConverterFormats.h"

IntSetting SelectionToolbarMode { "/SelectionToolbarMode", 0 };

namespace {
SelectionBar::SelectionMode ReadSelectionMode()
{
    return static_cast<SelectionBar::SelectionMode>(
        std::clamp(SelectionToolbarMode.Read(), 0, 3));
}

void UpdateSelectionMode(SelectionBar::SelectionMode selectionMode)
{
    SelectionToolbarMode.Write(static_cast<int>(selectionMode));
    gPrefs->Flush();
}

/* i18n-hint noun */
const TranslatableString StartTimeText = XO("Start");
/* i18n-hint noun */
const TranslatableString EndTimeText = XO("End");
/* i18n-hint noun */
const TranslatableString LengthTimeText = XO("Length");
/* i18n-hint noun */
const TranslatableString CenterTimeText = XO("Center");

std::pair<const TranslatableString&, const TranslatableString&> ModeNames[] = {
    { StartTimeText,  EndTimeText },
    { StartTimeText,  LengthTimeText },
    { LengthTimeText, EndTimeText },
    { LengthTimeText, CenterTimeText },
};

std::unordered_map<TranslatableString, wxWindowID> WindowIDs {
    { StartTimeText, 2705 },
    { LengthTimeText, 2706 },
    { CenterTimeText, 2707 },
    { EndTimeText, 2708 }
};

const NumericConverterType TimeConverterType[][2] {
    { NumericConverterType_TIME(), NumericConverterType_TIME() },
    { NumericConverterType_TIME(), NumericConverterType_DURATION() },
    { NumericConverterType_DURATION(), NumericConverterType_TIME() },
    { NumericConverterType_DURATION(), NumericConverterType_TIME() },
};
}

IMPLEMENT_CLASS(SelectionBar, ToolBar);

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
EVT_SIZE(SelectionBar::OnSize)

EVT_IDLE(SelectionBar::OnIdle)

EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, SelectionBar::OnCaptureKey)
END_EVENT_TABLE()

Identifier SelectionBar::ID()
{
    return wxT("Selection");
}

SelectionBar::SelectionBar(AudacityProject& project)
    : ToolBar(project, XO("Selection"), ID()),
    mRate(0.0),
    mStart(0.0), mEnd(0.0), mLength(0.0), mCenter(0.0),
    mFormatsSubscription{ProjectNumericFormats::Get(project).Subscribe(
                             *this, &SelectionBar::OnFormatsChanged)}
{
    // Make sure we have a valid rate as the NumericTextCtrl()s
    // created in Populate()
    // depend on it.  Otherwise, division-by-zero floating point exceptions
    // will occur.
    // Refer to bug #462 for a scenario where the division-by-zero causes
    // Audacity to fail.
    // We expect mRate to be set from the project later.
    mRate = ProjectRate::Get(project).GetRate();

    // Selection mode of 0 means showing 'start' and 'end' only.
    mSelectionMode = ReadSelectionMode();
}

SelectionBar::~SelectionBar()
{
}

bool SelectionBar::ShownByDefault() const
{
    return true;
}

ToolBar::DockID SelectionBar::DefaultDockID() const
{
    return BotDockID;
}

SelectionBar& SelectionBar::Get(AudacityProject& project)
{
    auto& toolManager = ToolManager::Get(project);
    return *static_cast<SelectionBar*>(toolManager.GetToolBar(ID()));
}

const SelectionBar& SelectionBar::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void SelectionBar::Create(wxWindow* parent)
{
    ToolBar::Create(parent);
    UpdatePrefs();
}

AButton* SelectionBar::MakeSetupButton()
{
    wxImage up = theTheme.Image(bmpRecoloredUpSmall);
    up.Rescale(23, 23, wxIMAGE_QUALITY_HIGH);
    wxImage down = theTheme.Image(bmpRecoloredDownSmall);
    down.Rescale(23, 23, wxIMAGE_QUALITY_HIGH);
    wxImage hiliteUp = theTheme.Image(bmpRecoloredUpHiliteSmall);
    hiliteUp.Rescale(23, 23, wxIMAGE_QUALITY_HIGH);
    wxImage hiliteDown = theTheme.Image(bmpRecoloredHiliteSmall);
    hiliteDown.Rescale(23, 23, wxIMAGE_QUALITY_HIGH);

    auto btn = safenew AButton(
        this, wxID_ANY, wxDefaultPosition, wxSize { 23, 23 }, up, hiliteUp, down,
        hiliteDown, up, false);

    btn->SetButtonType(AButton::FrameButton);
    btn->SetIcon(theTheme.Image(bmpCogwheel));
    btn->SetLabel({});
    btn->SetName(XO("Selection Toolbar Setup").Translation());

    return btn;
}

void SelectionBar::AddTitle(
    const TranslatableString& Title, wxSizer* pSizer)
{
    const auto translated = Title.Translation();
    auStaticText* pTitle = safenew auStaticText(this, translated);
    pTitle->SetBackgroundColour(theTheme.Colour(clrMedium));
    pTitle->SetForegroundColour(theTheme.Colour(clrTrackPanelText));
    pSizer->Add(pTitle, 0, wxEXPAND | wxRIGHT, 5);
}

void SelectionBar::AddTime(int id, wxSizer* pSizer)
{
    auto& formats = ProjectNumericFormats::Get(mProject);
    auto formatName = formats.GetSelectionFormat();
    auto pCtrl = safenew NumericTextCtrl(FormatterContext::ProjectContext(mProject),
                                         this, id, NumericConverterType_TIME(), formatName, 0.0);

    pCtrl->Bind(
        wxEVT_TEXT,
        [this, id](auto& evt) { ModifySelection(id, evt.GetInt() != 0); });

    pSizer->Add(pCtrl, 0, wxALIGN_TOP | wxRIGHT, 5);

    mTimeControls[id] = pCtrl;

    mFormatChangedToFitValueSubscription[id] = pCtrl->Subscribe(
        [this, id](const auto& msg)
    {
        auto altCtrl = mTimeControls[id == 0 ? 1 : 0];
        if (altCtrl != nullptr) {
            altCtrl->UpdateFormatToFit(msg.value);
        }

        FitToTimeControls();
    });
}

void SelectionBar::AddSelectionSetupButton(wxSizer* pSizer)
{
    auto setupBtn = MakeSetupButton();

    auto showMenu = [this, setupBtn]()
    {
        static const wxString choices[4] = {
            _("Start and End of Selection"),
            _("Start and Length of Selection"),
            _("Length and End of Selection"),
            _("Length and Center of Selection"),
        };

        wxMenu menu;
        int selectionMode {};
        for (auto& choice : choices) {
            auto subMenu = menu.AppendRadioItem(wxID_ANY, choice);

            if (static_cast<SelectionMode>(selectionMode) == mSelectionMode) {
                subMenu->Check();
            }

            menu.Bind(
                wxEVT_MENU,
                [this, selectionMode](auto& evt)
            {
                SetSelectionMode(static_cast<SelectionMode>(selectionMode));
                SelectionModeUpdated();
            },
                subMenu->GetId());

            ++selectionMode;
        }

        menu.Bind(wxEVT_MENU_CLOSE, [setupBtn](auto&) { setupBtn->PopUp(); });

        BasicMenu::Handle { &menu }.Popup(wxWidgetsWindowPlacement { setupBtn });
    };

    setupBtn->Bind(wxEVT_BUTTON, [this, showMenu](auto&) { showMenu(); });
    pSizer->Add(setupBtn, 0, wxALIGN_RIGHT | wxBOTTOM | wxRIGHT, 5);

    mSetupButton = setupBtn;
}

void SelectionBar::Populate()
{
    SetBackgroundColour(theTheme.Colour(clrMedium));

    mTimeControls[0] = mTimeControls[1] = {};

    // Outer sizer has space top and left.
    // Inner sizers have space on right only.
    // This choice makes for a nice border and internal spacing and places clear responsibility
    // on each sizer as to what spacings it creates.
    wxFlexGridSizer* mainSizer = safenew wxFlexGridSizer(2, 1, 1);
    Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

    AddTitle(XO("Selection"), mainSizer);
    AddTime(0, mainSizer);
    AddSelectionSetupButton(mainSizer);
    AddTime(1, mainSizer);

    mSetupButton->MoveBeforeInTabOrder(mTimeControls[0]);

    // Update the selection mode before the layout
    SetSelectionMode(mSelectionMode);
    mainSizer->Layout();
    RegenerateTooltips();
    Layout();

    CallAfter([this]{
        auto& formats = ProjectNumericFormats::Get(mProject);
        SetSelectionFormat(formats.GetSelectionFormat());
    });
}

void SelectionBar::UpdatePrefs()
{
    // The project rate is no longer driven from here.
    // When preferences change, the Project learns about it too.
    // If necessary we can drive the SelectionBar mRate via the Project
    // calling our SetRate().
    // As of 13-Sep-2018, changes to the sample rate pref will only affect
    // creation of new projects, not the sample rate in existing ones.

    // This will only change the selection mode during a "Reset Configuration"
    // action since the read value will be the same during a normal preferences
    // update.
    mSelectionMode = ReadSelectionMode();

    // This will only change the time format during a "Reset Configuration"
    // action since the read value will be the same during a normal preferences
    // update.
    wxCommandEvent e;
    e.SetString(NumericConverterFormats::Lookup(
                    FormatterContext::ProjectContext(mProject),
                    NumericConverterType_TIME(),
                    gPrefs->Read(wxT("/SelectionFormat"), wxT(""))).Internal());
    OnUpdate(e);

    // Set label to pull in language change
    SetLabel(XO("Selection"));

    RegenerateTooltips();
    // Give base class a chance
    ToolBar::UpdatePrefs();
}

void SelectionBar::RegenerateTooltips()
{
}

void SelectionBar::OnSize(wxSizeEvent& evt)
{
    Refresh(true);

    evt.Skip();
}

// When a control value is changed, this function is called.
// It determines the values for the other controls.
void SelectionBar::ModifySelection(int driver, bool done)
{
    // Only update a value if user typed something in.
    // The reason is the controls may be less accurate than
    // the values.
    double start = mStart;
    double end = mEnd;
    double center = mCenter;
    double length = mLength;

    if (driver == 0) {
        if (
            mSelectionMode == SelectionMode::StartEnd
            || mSelectionMode == SelectionMode::StartLength) {
            start = mTimeControls[0]->GetValue();
        } else if (
            mSelectionMode == SelectionMode::LengthCenter
            || mSelectionMode == SelectionMode::LengthEnd) {
            length = mTimeControls[0]->GetValue();
        } else {
            wxFAIL_MSG("Unexpected selection mode");
        }
    } else if (driver == 1) {
        if (
            mSelectionMode == SelectionMode::StartEnd
            || mSelectionMode == SelectionMode::LengthEnd) {
            end = mTimeControls[1]->GetValue();
        } else if (mSelectionMode == SelectionMode::StartLength) {
            length = mTimeControls[1]->GetValue();
        } else if (mSelectionMode == SelectionMode::LengthCenter) {
            center = mTimeControls[1]->GetValue();
        } else {
            wxFAIL_MSG("Unexpected selection mode");
        }
    } else {
        wxFAIL_MSG("Illegal selection driver");
    }

    switch (mSelectionMode) {
    case SelectionBar::SelectionMode::StartEnd:
        if (driver == 0 && end < start) {
            end = start;
        } else if (driver == 1 && start > end) {
            start = end;
        }
        break;
    case SelectionBar::SelectionMode::StartLength:
        // Nothing can go wrong here
        end = start + length;
        break;
    case SelectionBar::SelectionMode::LengthEnd:
        start = end - length;

        if (start < 0) {
            // Length is set by user
            if (driver == 0) {
                end -= start;
            }

            start = 0;
        }
        break;
    case SelectionBar::SelectionMode::LengthCenter:
        start = center - length / 2.0;

        if (start < 0) {
            // Length is set by user
            if (driver == 0) {
                center = length / 2.0;
            } else {
                // Center is set by user
                length = center * 2.0;
            }

            start = 0;
        }

        end = center + length / 2.0;
        break;
    default:
        break;
    }

    // Places the start-end markers on the track panel.
    auto& manager = ProjectSelectionManager::Get(mProject);
    manager.ModifySelection(start, end, done);
}

// Called when one of the format drop downs is changed.
void SelectionBar::OnUpdate(wxCommandEvent& evt)
{
    evt.Skip(false);

    wxWindow* w = FindFocus();

    auto focusedCtrlIt
        =std::find(mTimeControls.begin(), mTimeControls.end(), w);

    const auto focusedCtrlIdx
        =focusedCtrlIt != mTimeControls.end()
          ? std::distance(mTimeControls.begin(), focusedCtrlIt)
          : -1;

    auto format = evt.GetString();

    // Save format name before recreating the controls so they resize properly
    if (mTimeControls.front()) {
        auto& formats = ProjectNumericFormats::Get(mProject);
        formats.SetSelectionFormat(format);
        // Then my Subscription is called
    }

    // ReCreateButtons() will get rid of our sizers and controls
    // so reset pointers first.
    std::fill(mTimeControls.begin(), mTimeControls.end(), nullptr);

    ToolBar::ReCreateButtons();

    ValuesToControls();

    UpdateTimeControlsFormat(format);

    if (focusedCtrlIdx >= 0 && mTimeControls[focusedCtrlIdx]) {
        mTimeControls[focusedCtrlIdx]->SetFocus();
    }

    RegenerateTooltips();

    Updated();
}

void SelectionBar::OnIdle(wxIdleEvent& evt)
{
    evt.Skip();
    const auto& selectedRegion = ViewInfo::Get(mProject).selectedRegion;

    SetTimes(selectedRegion.t0(), selectedRegion.t1());
}

void SelectionBar::SelectionModeUpdated()
{
    // We just changed the mode.  Remember it.
    UpdateSelectionMode(mSelectionMode);

    FitToTimeControls();
}

// We used to have 8 modes which showed different combinations of the
// length, start, end, center controls.
// Mode 7 for example showed all four at the same time.
void SelectionBar::SetSelectionMode(SelectionMode mode)
{
    mSelectionMode = mode;

    // Update names and WindowIds for the screen readers
    auto& modeName = ModeNames[static_cast<size_t>(mode)];

    if (mTimeControls[0]) {
        mTimeControls[0]->SetName(modeName.first);
        mTimeControls[0]->SetId(WindowIDs.at(modeName.first));
    }

    if (mTimeControls[1]) {
        mTimeControls[1]->SetName(modeName.second);
        mTimeControls[1]->SetId(WindowIDs.at(modeName.second));
    }

    UpdateTimeControlsFormat(mTimeControls[0]->GetFormatName());

    ValuesToControls();
}

void SelectionBar::ValuesToControls()
{
    const double valuePairs[4][2] = {
        { mStart, mEnd },
        { mStart, mLength },
        { mLength, mEnd },
        { mLength, mCenter } };

    const auto value = valuePairs[static_cast<size_t>(mSelectionMode)];

    size_t i = 0;
    for (auto ctrl : mTimeControls) {
        if (ctrl != nullptr) {
            ctrl->SetValue(value[i]);
        }

        i++;
    }
}

// A time has been set.  Update the control values.
void SelectionBar::SetTimes(double start, double end)
{
    if (start != mStart || end != mEnd
        || mLastSelectionMode != mSelectionMode
        ) {
        mStart = start;
        mEnd = end;
        mLength = end - start;
        mCenter = (end + start) / 2.0;
        mLastSelectionMode = mSelectionMode;

        ValuesToControls();
    }
}

void SelectionBar::SetSelectionFormat(const NumericFormatID& format)
{
    if (mTimeControls.front() == nullptr) {
        return;
    }

    const bool changed = mTimeControls.front()->SetFormatName(format);

    // Test first whether changed, to avoid infinite recursion from OnUpdate
    if (changed) {
        wxCommandEvent e;
        e.SetString(format.GET());
        OnUpdate(e);
    }
}

void SelectionBar::OnFormatsChanged(ProjectNumericFormatsEvent evt)
{
    auto& formats = ProjectNumericFormats::Get(mProject);
    switch (evt.type) {
    case ProjectNumericFormatsEvent::ChangedSelectionFormat:
        return SetSelectionFormat(formats.GetSelectionFormat());
    default:
        break;
    }
}

void SelectionBar::OnFocus(wxFocusEvent& event)
{
    KeyboardCapture::OnFocus(*this, event);
}

void SelectionBar::OnCaptureKey(wxCommandEvent& event)
{
    wxKeyEvent* kevent = (wxKeyEvent*)event.GetEventObject();
    wxWindow* w = FindFocus();
    int keyCode = kevent->GetKeyCode();

    // Convert numeric keypad entries.
    if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9)) {
        keyCode -= WXK_NUMPAD0 - '0';
    }

    if (keyCode >= '0' && keyCode <= '9') {
        return;
    }

    event.Skip();
}

void SelectionBar::UpdateTimeControlsFormat(const NumericFormatID& format)
{
    for (size_t controlIndex = 0; controlIndex < mTimeControls.size();
         ++controlIndex) {
        auto ctrl = mTimeControls[controlIndex];

        if (ctrl == nullptr) {
            continue;
        }

        const auto type
            =TimeConverterType[static_cast<size_t>(mSelectionMode)][controlIndex];

        ctrl->SetTypeAndFormatName(
            type, type != NumericConverterType_DURATION()
            ? format
            : NumericConverterFormats::GetBestDurationFormat(format));
    }
}

void SelectionBar::FitToTimeControls()
{
    wxSize sz = GetMinSize();
    sz.SetWidth(10);
    SetMinSize(sz);
    Fit();
    Layout();
    Updated();
}

static RegisteredToolbarFactory factory{
    []( AudacityProject& project ){
        return ToolBar::Holder{ safenew SelectionBar{ project } };
    }
};

namespace {
AttachedToolBarMenuItem sAttachment{
    /* i18n-hint: Clicking this menu item shows the toolbar
       for selecting a time range of audio */
    SelectionBar::ID(), wxT("ShowSelectionTB"), XXO("&Selection Toolbar")
};
}
