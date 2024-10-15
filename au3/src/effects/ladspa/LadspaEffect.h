/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_LADSPA_EFFECT__
#define __AUDACITY_LADSPA_EFFECT__

#include "LadspaEffectBase.h"

#include <wx/dynlib.h> // member variable
//#include <wx/event.h> // to inherit
//#include <wx/weakref.h>

#include "../StatelessPerTrackEffect.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

//#include "SampleFormat.h"

class LadspaEffect final
   : public LadspaEffectBase
   , public StatelessEffectUIServices
{
public:
   using LadspaEffectBase::LadspaEffectBase;
   ~LadspaEffect() override;

private:
   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;

   std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs)
   const override;

   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;

   void ShowOptions(const EffectPlugin &plugin) const override;
};
#endif
