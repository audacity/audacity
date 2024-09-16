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
#include "StatefulEffectUIServices.h"

class EffectNoiseReduction final :
    public StatefulEffect,
    public StatefulEffectUIServices
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectNoiseReduction();
   virtual ~EffectNoiseReduction();

   using Effect::TrackProgress;

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

//   using Effect::TrackProgress;

   int ShowHostInterface(EffectBase &plugin, wxWindow &parent,
      const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;

   bool Process(EffectInstance &instance, EffectSettings &settings) override;

   // This object is the memory of the effect between uses
   // (other than noise profile statistics)
   class Settings
   {
   public:
      Settings();
      ~Settings()
      {
      }

      int PromptUser(
         EffectNoiseReduction* effect, EffectSettingsAccess& access,
         wxWindow& parent, bool bHasProfile, bool bAllowTwiddleSettings);
      bool PrefsIO(bool read);
      bool Validate(EffectNoiseReduction* effect) const;

      size_t WindowSize() const
      {
         return 1u << (3 + mWindowSizeChoice);
      }
      unsigned StepsPerWindow() const
      {
         return 1u << (1 + mStepsPerWindowChoice);
      }
      size_t SpectrumSize() const
      {
         return 1 + WindowSize() / 2;
      }
      size_t StepSize() const
      {
         return WindowSize() / StepsPerWindow();
      }

      bool mDoProfile;

      // Stored in preferences:

      // Basic:
      double mNewSensitivity;     // - log10 of a probability... yeah.
      double mFreqSmoothingBands; // really an integer
      double mNoiseGain;          // in dB, positive
      double mAttackTime;         // in secs
      double mReleaseTime;        // in secs

      // Advanced:
      double mOldSensitivity; // in dB, plus or minus

      // Basic:
      int mNoiseReductionChoice;

      // Advanced:
      int mWindowTypes;
      int mWindowSizeChoice;
      int mStepsPerWindowChoice;
      int mMethod;
   };

   class Statistics;
   class Dialog;
   class Worker;

private:
   friend class Dialog;

   std::unique_ptr<Settings> mSettings;
   std::unique_ptr<Statistics> mStatistics;
};

#endif
