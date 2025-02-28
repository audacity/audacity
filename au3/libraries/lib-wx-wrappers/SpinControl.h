/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file SpinControl.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <numeric>

#include <wx/control.h>

#include "TranslatableString.h"
#include "Prefs.h"

class wxTextCtrl;
class wxKeyEvent;

#ifndef __WXGTK__
class wxSpinButton;
#else
class wxButton;
#endif

class WX_WRAPPERS_API SpinControl final : public wxControl, private PrefsListener
{
public:
    using ValueType = double;

    SpinControl(
        wxWindow* parent, wxWindowID winid = wxID_ANY, ValueType value = 0.0, ValueType min = 0.0f, ValueType max = 100.0,
        ValueType step = 1.0, bool allowFractional = false, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
        const TranslatableString& name = {});

    void SetValue(double value);
    double GetValue() const;

    void SetMinValue(double value);
    double GetMinValue() const;

    void SetMaxValue(double value);
    double GetMaxValue() const;

    void SetStep(double step);
    double GetStep() const;

    void SetFractionalAllowed(bool allow);
    bool GetFractionalAllowed();

    void SetName(const TranslatableString& name);

private:
    void UpdatePrefs() override;

    void CreateUI();
    void SetupControls();
    void CommitTextControlValue();
    void OnCharHook(wxKeyEvent& evt);

    void SetValue(double value, bool silent);

    void DoSteps(double direction);

    void NotifyValueChanged();

    ValueType mValue { std::numeric_limits<ValueType>::quiet_NaN() };
    ValueType mMinValue { -std::numeric_limits<ValueType>::infinity() };
    ValueType mMaxValue { std::numeric_limits<ValueType>::infinity() };
    ValueType mStep;
    int mPrecision { 2 };

    bool mFractionalAllowed;

    wxTextCtrl* mTextControl { nullptr };
#ifndef __WXGTK__
    wxSpinButton* mSpinButton { nullptr };
#else
    wxButton* mUpButton { nullptr };
    wxButton* mDownButton { nullptr };
#endif

#if wxUSE_ACCESSIBILITY
    class SpinControlAx;
    // As usual, wxWidgets manages the lifetime
    SpinControlAx* mWindowAccessible { nullptr };
#endif
};
