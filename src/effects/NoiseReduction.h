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

#include <memory>

#define NOISEREDUCTION_PLUGIN_SYMBOL XO("Noise Reduction")

class EffectNoiseReduction: public Effect {
public:

   EffectNoiseReduction();
   virtual ~EffectNoiseReduction();

   using Effect::TrackProgress;

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // Effect implementation

//   using Effect::TrackProgress;

   virtual bool PromptUser(wxWindow *parent);

   virtual bool Init();
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();

   class Settings;
   class Statistics;
   class Dialog;

private:
   class Worker;
   friend class Dialog;

   std::auto_ptr<Settings> mSettings;
   std::auto_ptr<Statistics> mStatistics;
};

#endif
