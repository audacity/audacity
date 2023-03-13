/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni

  This class implements a LADSPA Plug-in effect.

*******************************************************************//**

\class LadspaEffect
\brief An Effect that calls up a LADSPA plug in, i.e. many possible
effects from this one class.

*//*******************************************************************/
#include "LadspaEffect.h"       // This class's header file
#include "LadspaEditor.h"
#include "LadspaEffectOptionsDialog.h"

LadspaEffect::~LadspaEffect() = default;

int LadspaEffect::ShowClientInterface(const EffectPlugin &,
   wxWindow &parent, wxDialog &dialog,
   EffectEditor *, bool forceModal) const
{
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      dialog.Show();
      return 0;
   }

   return dialog.ShowModal();
}

std::unique_ptr<EffectEditor> LadspaEffect::MakeEditor(ShuttleGui & S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto pValues = static_cast<const LadspaEffectOutputs *>(pOutputs);
   auto result = std::make_unique<LadspaEditor>(*this,
      dynamic_cast<LadspaInstance&>(instance),
      mNumInputControls, mNumOutputControls, access, mProjectRate,
      GetType(), pValues);
   result->PopulateUI(S);
   return result;
}

void LadspaEffect::ExportPresets(
   const EffectPlugin &, const EffectSettings &) const
{
}

OptionalMessage LadspaEffect::ImportPresets(
   const EffectPlugin &, EffectSettings &) const
{
   return { nullptr };
}

void LadspaEffect::ShowOptions(const EffectPlugin &) const
{
   LadspaEffectOptionsDialog{ *this }.ShowModal();
}

// Inject factory hook to make LadspaEffect capable of UI
static LadspaEffectBase::Factory::SubstituteInUnique<LadspaEffect> scope;
