/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  RealtimeEffectStateUI.cpp

  Dmitry Vedenko

**********************************************************************/
#include "RealtimeEffectStateUI.h"

#include <cassert>

#include "EffectEditor.h"
#include "EffectUI.h"
#include "EffectUIServices.h"
#include "RealtimeEffectState.h"

#include "EffectManager.h"
#include "UndoManager.h"
#include "ProjectHistory.h"
#include "ProjectWindow.h"
#include "Track.h"

namespace
{
const RealtimeEffectState::RegisteredFactory realtimeEffectStateUIFactory { [](auto& state)
   { return std::make_unique<RealtimeEffectStateUI>(state); }
};
} // namespace


BEGIN_EVENT_TABLE(RealtimeEffectStateUI, wxEvtHandler)
   EVT_CLOSE(RealtimeEffectStateUI::OnClose)
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

   const auto client = dynamic_cast<EffectUIServices *>(effectPlugin);

   // Effect has no client interface
   if (client == nullptr)
      return;

   auto& projectWindow = ProjectWindow::Get(project);

   std::shared_ptr<EffectInstance> pInstance;

   auto access = mRealtimeEffectState.GetAccess();

   // EffectUIHost invokes shared_from_this on access
   // EffectContext construction
   auto pContext = std::make_shared<EffectContext>();
   Destroy_ptr<EffectUIHost> dlg(safenew EffectUIHost(
      &projectWindow, project, pContext,
      *effectPlugin, *client, pInstance, *access,
      mRealtimeEffectState.shared_from_this()));

   if (!dlg->Initialize())
      return;

   // Dialog is owned by its parent now!
   mEffectUIHost = dlg.release();

   UpdateTitle();

   client->ShowClientInterface(*effectPlugin,
      projectWindow, *mEffectUIHost, mEffectUIHost->GetEditor(), false);

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
         // This achieves autosave on close of project before other important
         // project state is destroyed
         Hide(&project);
      });

   mParameterChangedSubscription = mEffectUIHost->GetEditor()->Subscribe(
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

void RealtimeEffectStateUI::OnClose(wxCloseEvent & evt)
{
   auto next = GetNextHandler();
   mEffectUIHost->RemoveEventHandler(this);
   auto pProject = mpProject;
   mpProject = nullptr;
   if (pProject)
      AutoSave(*pProject);

   // Pass the event through to the dialog
   if (next)
      next->ProcessEvent(evt);
}
