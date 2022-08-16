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

#include "ClientData.h"
#include "Observer.h"

class RealtimeEffectState;
class Track;
class EffectUIHost;
class EffectInstance;

class AudacityProject;

//! UI state for realtime effect
class RealtimeEffectStateUI final :
    public ClientData::Base
{
public:
   explicit RealtimeEffectStateUI(RealtimeEffectState& state);
   ~RealtimeEffectStateUI() override;

   [[nodiscard]] bool IsShown() const noexcept;

   void Show(AudacityProject& project);
   void Hide();
   void Toggle(AudacityProject& project);

   void UpdateTrackData(const Track& track);

   static RealtimeEffectStateUI &Get(RealtimeEffectState &state);
   static const RealtimeEffectStateUI &Get(const RealtimeEffectState &state);

private:
   void UpdateTitle();
   
   RealtimeEffectState& mRealtimeEffectState;

   wxWeakRef<EffectUIHost> mEffectUIHost;

   TranslatableString mEffectName;
   wxString mTrackName;

   Observer::Subscription mProjectWindowDestroyedSubscription;
}; // class RealtimeEffectStateUI
