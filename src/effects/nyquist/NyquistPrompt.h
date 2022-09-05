/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistPrompt.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_NYQUIST_PROMPT__
#define __AUDACITY_EFFECT_NYQUIST_PROMPT__

#include "Nyquist.h"

class AUDACITY_DLL_API NyquistPrompt final
   : public NyquistEffect
{
public:
   NyquistPrompt();

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   ManualPageID ManualPage() const override;
   bool IsInteractive() const override;
   bool IsDefault() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms)
         const override;
   bool DoLoadSettings(
      const CommandParameters *pParms, EffectSettings &settings) override;
   bool DoVisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const override;

   bool Init() override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   bool AcceptsAllNyquistTypes() override;

   int ShowHostInterface( wxWindow &parent,
      const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access)
         override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   bool RecoverParseTypeFailed() override;

private:
   wxString          mInputCmd; // history: exactly what the user typed
   wxString          mParameters; // The parameters to be fed to a nested prompt
   TranslatableString mPromptName; // If a prompt, we need to remember original name.
   EffectType        mPromptType; // If a prompt, need to remember original type.
   wxTextCtrl *mCommandText;

   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   DECLARE_EVENT_TABLE()
};

#endif
