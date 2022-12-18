/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RealtimeEffectStateUI.cpp

  Dmitry Vedenko

**********************************************************************/
#include "RealtimeEffectStateUI.h"

#include <cassert>

#include "BasicUI.h"
#include "EffectUI.h"
#include "RealtimeEffectState.h"

#include "EffectManager.h"
#include "UndoManager.h"
#include "ProjectHistory.h"
#include "ProjectManager.h"
#include "ProjectWindow.h"
#include "Track.h"

namespace
{
const RealtimeEffectState::RegisteredFactory realtimeEffectStateUIFactory { [](auto& state)
   { return std::make_unique<RealtimeEffectStateUI>(state); }
};
} // namespace


BEGIN_EVENT_TABLE(RealtimeEffectStateUI, wxEvtHandler)
   EVT_CLOSE(RealtimeEffectStateUI::OnCloseDialog)
END_EVENT_TABLE()

RealtimeEffectStateUI::RealtimeEffectStateUI(RealtimeEffectState& state)
    : mRealtimeEffectState(state)
{
}

RealtimeEffectStateUI::~RealtimeEffectStateUI()
{
   Hide();
}

bool RealtimeEffectStateUI::IsShown() const noexcept
{
   return mEffectUIHost != nullptr;
}


static void DelayedCloseEvent(wxWindow &window, bool canVeto)
{
   // See how wxWidgets/src/common/wincmn.cpp constructed the event that was
   // intercepted; do similar
   wxCloseEvent event(wxEVT_CLOSE_WINDOW, window.GetId());
   event.SetEventObject(&window);
   event.SetCanVeto(canVeto);
   window.GetEventHandler()->AddPendingEvent(event);
}

void RealtimeEffectStateUI::Show(AudacityProject& project)
{
   if (mEffectUIHost != nullptr && mEffectUIHost->IsShown())
   {
      // Bring the host to front
      mEffectUIHost->Raise();
      return;
   }

   const auto ID = mRealtimeEffectState.GetID();
   const auto effectPlugin = EffectManager::Get().GetEffect(ID);

   if (effectPlugin == nullptr)
      return;

   EffectUIClientInterface* client = effectPlugin->GetEffectUIClientInterface();

   // Effect has no client interface
   if (client == nullptr)
      return;

   auto& projectWindow = ProjectWindow::Get(project);

   std::shared_ptr<EffectInstance> pInstance;

   auto access = mRealtimeEffectState.GetAccess();

   // EffectUIHost invokes shared_from_this on access
   Destroy_ptr<EffectUIHost> dlg(safenew EffectUIHost(
      &projectWindow, project, *effectPlugin, *client, pInstance, *access,
      mRealtimeEffectState.shared_from_this()));

   if (!dlg->Initialize())
      return;

   // Dialog is owned by its parent now!
   mEffectUIHost = dlg.release();

   // Issue 4062, step 3:
   // For reasons unknown, queueing an event to close the main window really
   // must happen after the LV2 validator is destroyed, and that must be
   // done in idle time, outside of any yield to the event loop
   mValidatorDestroyedSubscription =
      mEffectUIHost->Subscribe([this](auto){
         if (mDoClose) {
            mDoClose = false;
            // Closing the project during the destructor of another dialog makes
            // me nervous -- so defer this closing event, too
            if (mpProjectWindow)
               DelayedCloseEvent(*mpProjectWindow, mCanVeto);
         }
         mpProjectWindow = nullptr;
      });

   UpdateTitle();

   client->ShowClientInterface(
      projectWindow, *mEffectUIHost, mEffectUIHost->GetValidator(), false);

   // The dialog was modal? That shouldn't have happened
   if (mEffectUIHost == nullptr || !mEffectUIHost->IsShown())
   {
      assert(false);
      mEffectUIHost = {};
   }

   if (mEffectUIHost) {
      mEffectUIHost->PushEventHandler(this);
      mpProject = &project;
   }

   mProjectWindowDestroyedSubscription = projectWindow.Subscribe(
      [this, &project](ProjectWindowDestroyedMessage) {
         // The dialog has been completely destroyed first, and now we are
         // closing the project window.
         // This achieves autosave on close of project before other important
         // project state is destroyed
         // Is this redundant now with other AutoSaves in this file?  But keep
         // the precaution
         if (mpProject)
            AutoSave(*mpProject);
      });
   mSubscriber.PushHandler(project);

   mParameterChangedSubscription = mEffectUIHost->GetValidator()->Subscribe(
      [this](auto) { UndoManager::Get(*mpProject).MarkUnsaved(); });
}

void RealtimeEffectStateUI::Hide(AudacityProject* project)
{
   if (mEffectUIHost != nullptr)
   {
      mpProject = project; // May reassign as null to skip autosave in OnClose
      // EffectUIHost calls Destroy in OnClose handler
      mEffectUIHost->Close();
      mEffectUIHost = {};
   }
   if (mpProjectWindow)
      // Closing the dialog normally, did not intercept a main window closing
      mpProjectWindow->RemoveEventHandler(&mSubscriber);
}

void RealtimeEffectStateUI::Toggle(AudacityProject& project)
{
   if(IsShown())
      Hide(&project);
   else
      Show(project);
}

void RealtimeEffectStateUI::UpdateTrackData(const Track& track)
{
   mTrackName = track.GetName();

   UpdateTitle();
}

RealtimeEffectStateUI& RealtimeEffectStateUI::Get(RealtimeEffectState& state)
{      
   return state.Get<RealtimeEffectStateUI&>(realtimeEffectStateUIFactory);
}

const RealtimeEffectStateUI&
RealtimeEffectStateUI::Get(const RealtimeEffectState& state)
{
   return Get(const_cast<RealtimeEffectState&>(state));
}

void RealtimeEffectStateUI::UpdateTitle()
{
   if (mEffectUIHost == nullptr)
      return;

   if (mEffectName.empty())
   {
      const auto ID = mRealtimeEffectState.GetID();
      const auto effectPlugin = EffectManager::Get().GetEffect(ID);

      if (effectPlugin != nullptr)
         mEffectName = effectPlugin->GetDefinition().GetName();
   }

   const auto title =
      mTrackName.empty() ?
         mEffectName :
         /* i18n-hint: First %s is an effect name, second is a track name */
         XO("%s - %s").Format(mEffectName, mTrackName);

   mEffectUIHost->SetTitle(title);
   mEffectUIHost->SetName(title);
}

void RealtimeEffectStateUI::AutoSave(AudacityProject &project)
{
   ProjectHistory::AutoSave::Call(project);
}

void RealtimeEffectStateUI::OnCloseDialog(wxCloseEvent & evt)
{
   auto next = GetNextHandler();
   mEffectUIHost->RemoveEventHandler(this);
   mEffectUIHost = nullptr;
   auto pProject = mpProject;
   if (pProject)
      AutoSave(*pProject);

   // Pass the event through to close the dialog
   if (next)
      next->ProcessEvent(evt);

   // Issue 4062, step 2:
   // We may need to pass the original closing event also back to the
   // main project window.

   // But we need a complete closing and destruction of the dialog first.

   // So, why not just do this?
   // EnqueueCloseEvent(*mpProjectWindow, mCanVeto);
   // -- Because, that still does not fix the crash.

   // So, why not just call wxApp->ProcessIdle() to do delayed destruction now?
   // Because, for reasons unknown, the crash will still happen!

   // It seems that we really must destroy the LV2 validator while there are
   // not any other wxWidgets events in any queues.
   // So see what mValidatorDestroyedSubscription does.
}

void RealtimeEffectStateUI::ProjectWindowSubscriber::
PushHandler(AudacityProject &project)
{
   auto& projectWindow = ProjectWindow::Get(project);
   if (mState.mpProjectWindow == &projectWindow)
      ;
   else {
      if (mState.mpProjectWindow)
         mState.mpProjectWindow->RemoveEventHandler(this);
      mState.mpProjectWindow = &projectWindow;
      projectWindow.PushEventHandler(this);
   }
}

BEGIN_EVENT_TABLE(RealtimeEffectStateUI::ProjectWindowSubscriber, wxEvtHandler)
   EVT_CLOSE(RealtimeEffectStateUI::ProjectWindowSubscriber::InterceptCloseFrame)
END_EVENT_TABLE()

void RealtimeEffectStateUI::
ProjectWindowSubscriber::InterceptCloseFrame(wxCloseEvent & evt)
{
   // Issue 4062, step 1:
   // Intercept the closing of the project window once only; remove handler
   auto next = GetNextHandler();
   mState.mpProjectWindow->RemoveEventHandler(this);
   if (mState.mEffectUIHost) {
      // mState.mpProjectWindow is still needed
      mState.mCanVeto = evt.CanVeto();
      // Tell the dialog to close
      // The close event must be queued, not processed immediately, to fix
      // Issue 4062 for the Command+Q case too
      DelayedCloseEvent(*mState.mEffectUIHost, evt.CanVeto());
      // And cause our interception of that closing to close the project
      // window too, afterward
      mState.mDoClose = true;
   }
   else {
      mState.mpProjectWindow = nullptr;
      // Just pass the event through to the dialog
      if (next)
         next->ProcessEvent(evt);
   }
}
