/**********************************************************************

 Audacity: A Digital Audio Editor

 TimeToolBar.cpp

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 *//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/sizer.h>
#endif

#include "TimeToolBar.h"
#include "ToolManager.h"

#include "AudioIO.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "ProjectTimeSignature.h"
#include "ViewInfo.h"

#include "NumericConverterFormats.h"

IMPLEMENT_CLASS(TimeToolBar, ToolBar);

// Having a fixed ID for the Audio Position is helpful for
// the Jaws screen reader script for Audacity.
enum {
    TimeBarFirstID = 2800,
    AudioPositionID
};

BEGIN_EVENT_TABLE(TimeToolBar, ToolBar)
EVT_COMMAND(AudioPositionID, EVT_TIMETEXTCTRL_UPDATED, TimeToolBar::OnUpdate)
EVT_SIZE(TimeToolBar::OnSize)
EVT_IDLE(TimeToolBar::OnIdle)
END_EVENT_TABLE()

Identifier TimeToolBar::ID()
{
    return wxT("Time");
}

TimeToolBar::TimeToolBar(AudacityProject& project)
    :  ToolBar(project, XO("Time"), ID(), true)
    , mAudioTime{nullptr}
{
    mFormatsSubscription = ProjectNumericFormats::Get(project)
                           .Subscribe(*this, &TimeToolBar::OnFormatsChanged);
}

TimeToolBar::~TimeToolBar()
{
}

ToolBar::DockID TimeToolBar::DefaultDockID() const
{
    return BotDockID;
}

TimeToolBar& TimeToolBar::Get(AudacityProject& project)
{
    auto& toolManager = ToolManager::Get(project);
    return *static_cast<TimeToolBar*>(toolManager.GetToolBar(ID()));
}

const TimeToolBar& TimeToolBar::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void TimeToolBar::Populate()
{
    const auto& formats = ProjectNumericFormats::Get(mProject);

    // Get the default time format
    auto format = formats.GetAudioTimeFormat();

    // Create the read-only time control
    mAudioTime
        = safenew NumericTextCtrl(FormatterContext::ProjectContext(mProject), this, AudioPositionID, NumericConverterType_TIME(), format,
                                  0.0);
    mAudioTime->SetName(XO("Audio Position"));
    mAudioTime->SetReadOnly(true);

    // Add it to the toolbar
    Add(mAudioTime, 0, wxALIGN_CENTER | wxALL, toolbarSpacing);

    // Calculate the width to height ratio
    wxSize digitSize = mAudioTime->GetDigitSize();
    mDigitRatio = (float)digitSize.x / digitSize.y;

    // During initialization, we need to bypass some resizing to prevent the "best size"
    // from being used as we want to ensure the saved size is used instead. See SetDocked()
    // and OnUpdate() for more info.
    mSettingInitialSize = true;
    CallAfter([this]{
        auto& formats = ProjectNumericFormats::Get(mProject);
        // Get (and set) the saved time format
        SetAudioTimeFormat(formats.GetAudioTimeFormat());

        // During initialization, if the saved format is the same as the default,
        // OnUpdate() will not otherwise be called but we need it to set the
        // initial size.
        if (mSettingInitialSize) {
            wxCommandEvent e;
            e.SetString(mAudioTime->GetFormatName().GET());
            OnUpdate(e);
        }
    });

    // Establish initial resizing limits
    //   SetResizingLimits();
    mFormatChangedToFitValueSubscription = mAudioTime->Subscribe(
        [this](auto)
    {
        wxSizeEvent e;
        OnSize(e);
    });
}

void TimeToolBar::UpdatePrefs()
{
    // Since the language may have changed, we need to force an update to accommodate
    // different length text
    wxCommandEvent e;
    e.SetString(mAudioTime->GetFormatName().GET());
    OnUpdate(e);

    // Language may have changed so reset label
    SetLabel(XO("Time"));

    // Give the toolbar a chance
    ToolBar::UpdatePrefs();
}

void TimeToolBar::SetToDefaultSize()
{
    // Reset
    SetMaxSize(wxDefaultSize);
    SetMinSize(wxDefaultSize);

    // Set the default time format
    SetAudioTimeFormat(
        NumericConverterFormats::HoursMinsSecondsFormat().Internal());

    // Set the default size
    SetSize(GetInitialWidth(), 48);

    // Inform others the toobar has changed
    Updated();
}

wxSize TimeToolBar::GetDockedSize()
{
    wxSize sz = GetSize();

    // Anything less than a single height bar becomes single height
    if (sz.y <= toolbarSingle) {
        sz.y = toolbarSingle;
    }
    // Otherwise it will be a double height bar
    else {
        sz.y = 2 * toolbarSingle + toolbarGap;
    }

    return sz;
}

void TimeToolBar::SetDocked(ToolDock* dock, bool pushed)
{
    // It's important to call this FIRST since it unhides the resizer control.
    // Not doing so causes the calculated best size to be off by the width
    // of the resizer.
    ToolBar::SetDocked(dock, pushed);

    // Recalculate the min and max limits
    SetResizingLimits();

    // When moving from floater to dock, fit to toolbar since the resizer will
    // be mispositioned
    if (dock) {
        // During initialization, the desired size is already set, so do not
        // override it with the "best size". See OnUpdate for further info.
        if (!mSettingInitialSize) {
            // Fit() while retaining height
            SetSize(GetBestSize().x, GetSize().y);

            // Inform others the toolbar has changed
            Updated();
        }
    }
}

void TimeToolBar::SetAudioTimeFormat(const NumericFormatID& format)
{
    // Set the format if it's different from previous
    if (mAudioTime->SetFormatName(format)) {
        // Simulate an update since the format has changed.
        wxCommandEvent e;
        e.SetString(format.GET());
        OnUpdate(e);
    }
}

// The intention of this is to get the resize handle in the
// correct position, after we've let go in dragging.
void TimeToolBar::ResizingDone()
{
    // Fit() while retaining height
    SetSize(GetBestSize().x, GetSize().y);

    // Inform others the toobar has changed
    Updated();
}

void TimeToolBar::SetResizingLimits()
{
    // Reset limits
    SetMinSize(wxDefaultSize);
    SetMaxSize(wxDefaultSize);

    // If docked we use the current bar height since it's always a single or double height
    // toolbar.  For floaters, single height toolbar is the minimum height.
    int minH = IsDocked() ? GetSize().y : toolbarSingle;

    // Get the content size given the smallest digit height we allow
    wxSize minSize = ComputeSizing(minDigitH);

    // Account for any borders added by the window manager
    minSize.x += (mAudioTime->GetSize().x - mAudioTime->GetClientSize().x);

    // Calculate the space used by other controls and sizer borders with this toolbar
    wxSize outer = (GetSize() - GetSizer()->GetSize());

    // And account for them in the width
    minSize.x += outer.x;

    // Override the height
    minSize.y = minH;

    // Get the maximum digit height we can use. This is restricted to the toolbar's
    // current height minus any control borders
    int digH = minH - (mAudioTime->GetSize().y - mAudioTime->GetClientSize().y);

    // Get the content size using the digit height, if docked. Otherwise use the
    // maximum digit height we allow.
    wxSize maxSize = ComputeSizing(IsDocked() ? digH : maxDigitH);

    // Account for the other controls and sizer borders within this toolbar
    maxSize.x += outer.x;

    // Account for any borders added by the window manager and +1 to keep toolbar
    // from dropping to next smaller size when grabbing the resizer.
    maxSize.x += (mAudioTime->GetSize().x - mAudioTime->GetClientSize().x) + 1;

    // Override the height
    maxSize.y = IsDocked() ? minH : wxDefaultCoord;

    // And finally set them both
    SetMinSize(minSize);
    SetMaxSize(maxSize);
}

void TimeToolBar::OnFormatsChanged(ProjectNumericFormatsEvent evt)
{
    auto& settings = ProjectNumericFormats::Get(mProject);
    switch (evt.type) {
    case ProjectNumericFormatsEvent::ChangedAudioTimeFormat:
        SetAudioTimeFormat(settings.GetAudioTimeFormat());
        break;
    default:
        break;
    }
}

// Called when the format drop downs is changed.
// This causes recreation of the toolbar contents.
void TimeToolBar::OnUpdate(wxCommandEvent& evt)
{
    evt.Skip(false);

    ToolBar::ReCreateButtons();

    // Reset to allow resizing to work
    SetMinSize(wxDefaultSize);
    SetMaxSize(wxDefaultSize);

    // Save format name before recreating the controls so they resize properly
    auto& formats = ProjectNumericFormats::Get(mProject);
    formats.SetAudioTimeFormat(evt.GetString());
    // Then my subscription is called

    // During initialization, the desired size will have already been set at this point
    // and the "best" size" would override it, so we simply send a size event to force
    // the content to fit inside the toolbar.
    if (mSettingInitialSize) {
        mSettingInitialSize = false;
        SendSizeEvent();
    }
    // Otherwise we want the toolbar to resize to fit around the content
    else {
        // Fit() while retaining height
        SetSize(GetBestSize().x, GetSize().y);
    }

    // Go set the new size limits
    SetResizingLimits();

    // Inform others the toobar has changed
    Updated();
}

void TimeToolBar::OnSize(wxSizeEvent& evt)
{
    evt.Skip();

    // Can fire before we're ready
    if (!mAudioTime) {
        return;
    }

    // Make sure everything is where it's supposed to be
    Layout();

    // Get the sizer's size and remove any borders the time control might have.
    wxSize sizerBR = GetSizer()->GetSize() - (mAudioTime->GetSize() - mAudioTime->GetClientSize());

    // Get the content size of the time control. This can be different than the
    // control itself due to borders and sizer enduced changes.
    wxSize timeBR = mAudioTime->GetDimensions();

    // Get the current digit box height
    int h = mAudioTime->GetDigitSize().y;

    // Increase current size to find the best fit within the new size
    if (sizerBR.x >= timeBR.x && sizerBR.y >= timeBR.y) {
        do {
            h++;
            timeBR = ComputeSizing(h);
        } while (h < maxDigitH && sizerBR.x >= timeBR.x && sizerBR.y >= timeBR.y);
        h--;
    }
    // In all other cases, we need to decrease current size to fit within new size
    else if (sizerBR.x < timeBR.x || sizerBR.y < timeBR.y) {
        do {
            h--;
            timeBR = ComputeSizing(h);
        } while (h >= minDigitH && (sizerBR.x < timeBR.x || sizerBR.y < timeBR.y));
    }

    if (h != mAudioTime->GetDigitSize().y) {
        mAudioTime->SetDigitSize(h * mDigitRatio, h);
    }

    // Redraw the display immediately to smooth out resizing
    Update();
}

void TimeToolBar::OnIdle(wxIdleEvent& evt)
{
    evt.Skip();

    double audioTime;

    auto& projectAudioIO = ProjectAudioIO::Get(mProject);
    if (projectAudioIO.IsAudioActive()) {
        auto gAudioIO = AudioIO::Get();
        audioTime = gAudioIO->GetStreamTime();
    } else {
        const auto& playRegion = ViewInfo::Get(mProject).playRegion;
        audioTime = playRegion.GetStart();
    }

    mAudioTime->SetValue(wxMax(0.0, audioTime));
}

wxSize TimeToolBar::ComputeSizing(int digitH)
{
    return mAudioTime->ComputeSizing(false, digitH * mDigitRatio, digitH);
}

static RegisteredToolbarFactory factory{
    []( AudacityProject& project )
    {
        return ToolBar::Holder{ safenew TimeToolBar{ project } };
    }
};

namespace {
AttachedToolBarMenuItem sAttachment
{
    TimeToolBar::ID(),
    wxT("ShowTimeTB"),
    /* i18n-hint: Clicking this menu item shows the toolbar
       for viewing actual time of the cursor */
    XXO("&Time Toolbar"),
    {
        Registry::OrderingHint::After,
        "ShowSelectionTB"
    }
};
}
