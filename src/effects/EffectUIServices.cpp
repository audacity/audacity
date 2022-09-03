/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectUIServices.cpp

  Paul Licameli split from EffectPlugin.cpp

**********************************************************************/
#include "EffectUIServices.h"
#include "EffectPlugin.h"
#include "widgets/AudacityMessageBox.h"

EffectUIServices::~EffectUIServices() = default;

int EffectUIServices::ShowHostInterface(
   const std::shared_ptr<EffectContext> &pContext,
   EffectPlugin &plugin, wxWindow &parent, const EffectDialogFactory &factory,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   if (!plugin.IsInteractive())
      // Effect without UI just proceeds quietly to apply it destructively.
      return wxID_APPLY;

   // Create the dialog
   auto results = factory(parent, pContext, plugin, *this, access);
   auto pDialog = results.pDialog;
   pInstance = results.pInstance;
   if (!pDialog)
      return 0;

   // Let the derived class show the dialog and decide whether to keep it open
   auto result = ShowClientInterface(plugin, parent, *pDialog,
      results.pEditor, forceModal);
   if (pDialog && !pDialog->IsShown())
      // Derived class didn't show it, or showed it modally and closed it
      // So destroy it.
      pDialog->Destroy();

   return result;
}

int EffectUIServices::DoMessageBox(const EffectPlugin &plugin,
   const TranslatableString& message,
   long style, const TranslatableString &titleStr)
{
   auto title = titleStr.empty()
      ? plugin.GetName()
      : XO("%s: %s").Format(plugin.GetName(), titleStr);
   return AudacityMessageBox(message, title, style);
}

