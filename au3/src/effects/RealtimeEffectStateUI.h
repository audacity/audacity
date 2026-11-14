/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RealtimeEffectStateUI.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

// wx/weakref.h misses this include
#include <type_traits>
#include <wx/weakref.h>
#include <wx/event.h>

#include "ClientData.h"
#include "Observer.h"

class RealtimeEffectState;
class EffectUIHost;
class EffectInstance;

class AudacityProject;

//! UI state for realtime effect
class RealtimeEffectStateUI final : public wxEvtHandler, // Must be the first base class!
    public ClientData::Base
{
public:
    explicit RealtimeEffectStateUI(RealtimeEffectState& state);
    ~RealtimeEffectStateUI() override;

    [[nodiscard]] bool IsShown() const noexcept;

    void Show(AudacityProject& project);
    // Pass non-null to cause autosave
    void Hide(AudacityProject* project = nullptr);
    void Toggle(AudacityProject& project);

    //! Sets the display name of the target, that will help
    //! distinguish effect UI among others.
    void SetTargetName(const wxString& name);

    static RealtimeEffectStateUI& Get(RealtimeEffectState& state);
    static const RealtimeEffectStateUI& Get(const RealtimeEffectState& state);

    // Cause immediate or delayed autosave at a time when transport is stopped
    void AutoSave(AudacityProject& project);

private:
    void UpdateTitle();

    RealtimeEffectState& mRealtimeEffectState;

    wxWeakRef<EffectUIHost> mEffectUIHost;

    TranslatableString mEffectName;
    wxString mTargetName;
    AudacityProject* mpProject{};

    Observer::Subscription mProjectWindowDestroyedSubscription;
    Observer::Subscription mParameterChangedSubscription;

    void OnClose(wxCloseEvent& evt);
    DECLARE_EVENT_TABLE()
}; // class RealtimeEffectStateUI
