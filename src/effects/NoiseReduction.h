/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReduction.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)
  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REDUCTION__
#define __AUDACITY_EFFECT_NOISE_REDUCTION__

#include "StatefulEffect.h"

class EffectNoiseReduction final : public StatefulEffect {
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectNoiseReduction();
   virtual ~EffectNoiseReduction();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

//   using Effect::TrackProgress;

   int ShowHostInterface(const std::shared_ptr<EffectContext> &pContext,
      EffectPlugin &plugin, wxWindow &parent, const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

   class Settings;
   class Statistics;
   class Dialog;

private:
   class Worker;
   friend class Dialog;

   std::unique_ptr<Settings> mSettings;
   std::unique_ptr<Statistics> mStatistics;
};

#endif
