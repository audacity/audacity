/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file SpinControl.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "SpinControl.h"

#include <algorithm>

#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/valnum.h>
// For wxEVT_SPINCTRL
#include <wx/spinctrl.h>

#ifndef __WXGTK__
#include <wx/spinbutt.h>
#else
#include <wx/button.h>
#endif

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"

class SpinControl::SpinControlAx final : public WindowAccessible
{
public:
    explicit SpinControlAx(SpinControl* owner, wxWindow* control)
        : WindowAccessible{control}
        , mOwner{owner}
    {
    }

    wxAccStatus GetName(int childId, wxString* name) override
    {
        if (childId != wxACC_SELF) {
            return wxACC_NOT_IMPLEMENTED;
        }

        *name = mName.StrippedTranslation();

        return wxACC_OK;
    }

    void SetName(TranslatableString name)
    {
        mName = std::move(name);
    }

private:
    SpinControl* mOwner;
    TranslatableString mName;
};
#endif

SpinControl::SpinControl(
    wxWindow* parent, wxWindowID winid /*= wxID_ANY*/, double value /*= 0.0*/,
    double min /*= 0.0f*/, double max /*= 100.0*/, double step /*= 1.0*/,
    bool allowFractional /*= false*/, const wxPoint& pos /*= wxDefaultPosition*/,
    const wxSize& size /*= wxDefaultSize*/,
    const TranslatableString& name /*= {}*/)
    : wxControl(parent, winid, pos, size, wxBORDER_NONE)
    , mStep(step)
    , mFractionalAllowed(allowFractional)
{
    CreateUI();

    // Call setter explicitly to ensure that all the clamping happens correctly
    SetMinValue(min);
    SetMaxValue(max);
    SetValue(value);

    SetupControls();

    SetName(name);
}

void SpinControl::SetValue(double value)
{
    SetValue(value, true);
}

double SpinControl::GetValue() const
{
    return mValue;
}

void SpinControl::SetMinValue(double value)
{
    mMinValue = std::min(value, mMaxValue);
    SetValue(mValue);
    SetupControls();
}

double SpinControl::GetMinValue() const
{
    return mMinValue;
}

void SpinControl::SetMaxValue(double value)
{
    mMaxValue = std::max(value, mMinValue);
    SetValue(mValue);
    SetupControls();
}

double SpinControl::GetMaxValue() const
{
    return mMaxValue;
}

void SpinControl::SetStep(double step)
{
    mStep = step;
    SetupControls();
}

double SpinControl::GetStep() const
{
    return mStep;
}

void SpinControl::SetFractionalAllowed(bool allow)
{
    if (mFractionalAllowed == allow) {
        return;
    }

    mFractionalAllowed = allow;
    SetupControls();
}

bool SpinControl::GetFractionalAllowed()
{
    return mFractionalAllowed;
}

void SpinControl::SetName(const TranslatableString& name)
{
#if wxUSE_ACCESSIBILITY
    if (mWindowAccessible == nullptr) {
        mWindowAccessible = safenew SpinControlAx(this, mTextControl);
        mTextControl->SetAccessible(mWindowAccessible);
    }

    mWindowAccessible->SetName(name);
#endif
}

void SpinControl::UpdatePrefs()
{
}

void SpinControl::CreateUI()
{
    mTextControl = safenew wxTextCtrl(this, wxID_ANY);
#ifndef __WXGTK__
    const auto editorHeight = mTextControl->GetSize().y;
#else
    // GTK requires the buttons to be at least 16x16
    // TODO: rely on GTK_ICON_SIZE_BUTTON instead of hardcoding the size
    constexpr auto minGtkSize = 16;
    const auto editorHeight = std::max(minGtkSize * 2, mTextControl->GetSize().y);
#endif

    auto boxSizer = safenew wxBoxSizer(wxHORIZONTAL);

    boxSizer->Add(mTextControl, wxSizerFlags().Border(wxALL, 0));

#ifndef __WXGTK__
    mSpinButton = safenew wxSpinButton(this);
    mSpinButton->SetMaxSize({ -1, editorHeight });

    // SpinButton is only used to generate the events,
    // so keep the value between min (0 by default) and max (100 by default)
    mSpinButton->SetValue(50);

    boxSizer->Add(mSpinButton, wxSizerFlags().Border(wxALL, 0));
#else
    auto buttonsSizer = safenew wxBoxSizer(wxVERTICAL);

    const auto buttonSize = wxSize { editorHeight / 2, editorHeight / 2 };

    mUpButton = safenew wxButton(this, wxID_ANY, L"+", wxDefaultPosition, buttonSize);
    mUpButton->SetMinSize(buttonSize);
    mUpButton->SetMaxSize(buttonSize);
    buttonsSizer->Add(mUpButton, wxSizerFlags().Border(wxALL, 0));

    mDownButton = safenew wxButton(this, wxID_ANY, L"-", wxDefaultPosition, buttonSize);
    mDownButton->SetMinSize(buttonSize);
    mDownButton->SetMaxSize(buttonSize);
    buttonsSizer->Add(mDownButton, wxSizerFlags().Border(wxALL, 0));

    boxSizer->Add(buttonsSizer, wxSizerFlags().Border(wxALL, 0));
#endif

    const auto width = GetSize().x;

    if (width > 0) {
#ifndef __WXGTK__
        auto spinWidth = mSpinButton->GetSize().x;
        const auto editorWidth = std::max(10, width - spinWidth);
        mTextControl->SetMaxSize({ editorWidth, editorHeight });
#else
        const auto editorWidth = std::max(10, width - editorHeight / 2);

        mTextControl->SetMinSize({ editorWidth, editorHeight });
        mTextControl->SetMaxSize({ editorWidth, editorHeight });
        mTextControl->SetSize({ editorWidth, editorHeight });
#endif
    }

    SetSizerAndFit(boxSizer);
    Layout();

    Bind(
        wxEVT_SET_FOCUS,
        [this](auto& evt)
    {
        mTextControl->SetFocus();
        evt.Skip();
    });

    Bind(wxEVT_CHAR_HOOK, &SpinControl::OnCharHook, this);

    mTextControl->Bind(
        wxEVT_KILL_FOCUS,
        [this](auto& evt)
    {
        CommitTextControlValue();
        evt.Skip();
    });

    mTextControl->Bind(
        wxEVT_MOUSEWHEEL,
        [this](auto& evt)
    {
        const auto delta = evt.GetWheelDelta();
        const auto rotation = evt.GetWheelRotation();

        if (rotation >= delta) {
            DoSteps(evt.ShiftDown() ? 10 : 1);
        } else if (rotation <= delta) {
            DoSteps(evt.ShiftDown() ? -10 : -1);
        }
    });

#ifndef __WXGTK__
    mSpinButton->Bind(
        wxEVT_SPIN_UP,
        [this](auto& evt)
    {
        DoSteps(1);
        evt.Veto();
    });

    mSpinButton->Bind(
        wxEVT_SPIN_DOWN,
        [this](auto& evt)
    {
        DoSteps(-1);
        evt.Veto();
    });
#else
    mUpButton->Bind(
        wxEVT_BUTTON,
        [this](auto&)
    {
        DoSteps(1);
    });

    mDownButton->Bind(
        wxEVT_BUTTON,
        [this](auto&)
    {
        DoSteps(-1);
    });
#endif
}

void SpinControl::SetupControls()
{
    if (mFractionalAllowed) {
        auto validator = wxFloatingPointValidator<ValueType>(
            mPrecision, nullptr, wxNUM_VAL_NO_TRAILING_ZEROES);

        validator.SetMin(mMinValue);
        validator.SetMax(mMaxValue);

        mTextControl->SetValidator(validator);
    } else {
        auto validator = wxIntegerValidator<int>();

        validator.SetMin(static_cast<int>(std::ceil(mMinValue)));
        validator.SetMax(static_cast<int>(std::floor(mMaxValue)));

        mTextControl->SetValidator(validator);
    }
}

void SpinControl::CommitTextControlValue()
{
    double value;
    if (!mTextControl->GetValue().ToDouble(&value)) {
        return;
    }

    SetValue(value, false);
}

void SpinControl::OnCharHook(wxKeyEvent& evt)
{
    const auto keyCode = evt.GetKeyCode();

    if (keyCode == WXK_UP || keyCode == WXK_NUMPAD_UP) {
        DoSteps(evt.ShiftDown() ? 10.0 : 1.0);
    } else if (keyCode == WXK_PAGEUP || keyCode == WXK_NUMPAD_PAGEUP) {
        DoSteps(10.0);
    } else if (keyCode == WXK_DOWN || keyCode == WXK_NUMPAD_DOWN) {
        DoSteps(evt.ShiftDown() ? -10.0 : -1.0);
    } else if (keyCode == WXK_PAGEDOWN || keyCode == WXK_NUMPAD_PAGEDOWN) {
        DoSteps(-10.0);
    } else if (keyCode == WXK_RETURN || keyCode == WXK_NUMPAD_ENTER) {
        CommitTextControlValue();
    } else {
        evt.Skip();
    }
}

void SpinControl::SetValue(double value, bool silent)
{
    value = std::clamp(value, mMinValue, mMaxValue);

    // Should some epsilon be used here?
    if (value == mValue) {
        return;
    }

    mValue = value;
    mTextControl->SetValue(wxString::FromDouble(value));

    if (!silent) {
        NotifyValueChanged();
    }
}

void SpinControl::DoSteps(double direction)
{
    SetValue(mValue + direction * mStep, false);
}

void SpinControl::NotifyValueChanged()
{
    wxCommandEvent e(wxEVT_SPINCTRL, GetId());
    e.SetEventObject(this);
    ProcessWindowEvent(e);
}
