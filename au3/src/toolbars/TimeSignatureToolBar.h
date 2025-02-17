/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeSignatureToolBar.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <type_traits>
#include <wx/weakref.h>

#include <array>

#include "ToolBar.h"
#include "widgets/auStaticText.h"

#include "Observer.h"

class wxComboBox;
class wxSizeEvent;
class SpinControl;

class AudacityProject;

struct AudioIOEvent;

class TimeSignatureToolBar final : public ToolBar
{
public:
    static Identifier ID();

    TimeSignatureToolBar(AudacityProject& project);
    virtual ~TimeSignatureToolBar();

    bool ShownByDefault() const override;
    DockID DefaultDockID() const override;

    static TimeSignatureToolBar& Get(AudacityProject& project);
    static const TimeSignatureToolBar& Get(const AudacityProject& project);

    void Create(wxWindow* parent) override;

    void Populate() override;
    void Repaint(wxDC* WXUNUSED(dc)) override {}
    void EnableDisableButtons() override {}
    void UpdatePrefs() override;

    void RegenerateTooltips() override;

private:
    void OnSize(wxSizeEvent& evt);

    void OnAudioIOEvent(const AudioIOEvent& event);

    void AddTitle(
        const TranslatableString& Title, wxSizer* pSizer, int flags = wxEXPAND | wxRIGHT, int border = 5, double fontMultiplier = 1.0);

    Observer::Subscription mTimeSignatureChangedSubscription;
    Observer::Subscription mPlaybackStateChangedSubscription;

    wxWeakRef<SpinControl> mTempoControl;
    wxWeakRef<SpinControl> mUpperSignatureControl;
    wxWeakRef<wxComboBox> mLowerSignatureControl;

public:

    DECLARE_CLASS(TimeSignatureToolBar)
    DECLARE_EVENT_TABLE()
};
