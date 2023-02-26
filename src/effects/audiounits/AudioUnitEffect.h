/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_H
#define AUDACITY_AUDIOUNIT_EFFECT_H



#if USE_AUDIO_UNITS

#include "AudioUnitEffectBase.h"

#include "MemoryX.h"
#include <functional>
#include <type_traits>
#include <vector>

#include <AudioToolbox/AudioUnitUtilities.h>
#include <AudioUnit/AudioUnitProperties.h>

#include "StatelessPerTrackEffect.h"
#include "PluginInterface.h"

#include <wx/weakref.h>

class AUControl;

class AudioUnitEffect final
   : public StatelessEffectUIServices
   , public AudioUnitEffectBase
{
public:
   using AudioUnitEffectBase::AudioUnitEffectBase;
   ~AudioUnitEffect() override;

private:
   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;

   std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin &plugin,
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const override;

   bool CloseUI() const override;

   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;

   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;

   void ShowOptions(const EffectPlugin &plugin) const override;

   //! Will never be called
   virtual std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const final;
};

#endif

#endif
