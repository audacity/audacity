/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReduction.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)
  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REDUCTION__
#define __AUDACITY_EFFECT_NOISE_REDUCTION__

#include "Effect.h"

#include "../MemoryX.h"

#define NOISEREDUCTION_PLUGIN_SYMBOL XO("Noise Reduction")

class EffectNoiseReduction final : public Effect {
public:

   EffectNoiseReduction();
   virtual ~EffectNoiseReduction();

   using Effect::TrackProgress;

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // Effect implementation

//   using Effect::TrackProgress;

   bool PromptUser(wxWindow *parent) override;

   bool Init() override;
   bool CheckWhetherSkipEffect() override;
   bool Process() override;

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
