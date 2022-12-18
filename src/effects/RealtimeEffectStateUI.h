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
class Track;
class EffectUIHost;
class EffectInstance;

class AudacityProject;

//! UI state for realtime effect
class RealtimeEffectStateUI final
   : public wxEvtHandler // Must be the first base class!
   , public ClientData::Base
{
public:
   explicit RealtimeEffectStateUI(RealtimeEffectState& state);
   ~RealtimeEffectStateUI() override;

   [[nodiscard]] bool IsShown() const noexcept;

   void Show(AudacityProject& project);
   // Pass non-null to cause autosave
   void Hide(AudacityProject* project = nullptr);
   void Toggle(AudacityProject& project);

   void UpdateTrackData(const Track& track);

   static RealtimeEffectStateUI &Get(RealtimeEffectState &state);
   static const RealtimeEffectStateUI &Get(const RealtimeEffectState &state);

   // Cause immediate or delayed autosave at a time when transport is stopped
   void AutoSave(AudacityProject &project);

private:
   // To push event handlers onto two windows requires distinct handler objects
   struct ProjectWindowSubscriber : wxEvtHandler {
      explicit ProjectWindowSubscriber(RealtimeEffectStateUI &state)
         : mState{ state } {}
      void InterceptCloseFrame(wxCloseEvent & evt);
      void PushHandler(AudacityProject &project);

      RealtimeEffectStateUI &mState;

      DECLARE_EVENT_TABLE()
   } mSubscriber{ *this };

   void UpdateTitle();
   
   RealtimeEffectState& mRealtimeEffectState;

   wxWeakRef<EffectUIHost> mEffectUIHost;
   wxWeakRef<wxWindow> mpProjectWindow{};

   // Whether to pass a close event to the main window after dialog closes
   bool mDoClose{ false };
   // Context for doing the closing of the main window correctly
   bool mCanVeto{ false };

   TranslatableString mEffectName;
   wxString mTrackName;
   AudacityProject *mpProject{};

   Observer::Subscription mProjectWindowDestroyedSubscription;
   Observer::Subscription mParameterChangedSubscription;
   Observer::Subscription mValidatorDestroyedSubscription;

   void OnCloseDialog(wxCloseEvent & evt);
   DECLARE_EVENT_TABLE()
}; // class RealtimeEffectStateUI
