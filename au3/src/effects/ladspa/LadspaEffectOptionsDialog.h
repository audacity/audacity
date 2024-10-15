/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffectOptionsDialog.h

  Dominic Mazzoni
  Paul Licameli split from LadspaEffect.cpp

**********************************************************************/
#ifndef __AUDACITY_LADSPA_EFFECT_OPTIONS_DIALOG__
#define __AUDACITY_LADSPA_EFFECT_OPTIONS_DIALOG__

#include "wxPanelWrapper.h"
class EffectDefinitionInterface;
class ShuttleGui;

class LadspaEffectOptionsDialog final : public wxDialogWrapper
{
public:
   explicit LadspaEffectOptionsDialog(const EffectDefinitionInterface &effect);
   virtual ~LadspaEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   const EffectDefinitionInterface &mEffect;
   bool mUseLatency{};

   DECLARE_EVENT_TABLE()
};

#endif
