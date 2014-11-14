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

class EffectNoiseReduction: public Effect {
public:

   EffectNoiseReduction();
   virtual ~EffectNoiseReduction();

   using Effect::TrackProgress;

   virtual wxString GetEffectName();
   virtual std::set<wxString> GetEffectCategories();
   virtual wxString GetEffectIdentifier();
   virtual wxString GetEffectAction();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

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
