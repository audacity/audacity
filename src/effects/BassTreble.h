/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectBassTrebleState
{
public:
   float samplerate;
   double treble;
   double bass;
   double gain;
   double slope, hzBass, hzTreble;
   double a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   double a0Treble, a1Treble, a2Treble, b0Treble, b1Treble, b2Treble;
   double xn1Bass, xn2Bass, yn1Bass, yn2Bass;
   double xn1Treble, xn2Treble, yn1Treble, yn2Treble;
};

class EffectBassTreble final : public StatefulPerTrackEffect
{
public:
   static inline EffectBassTreble *
   FetchParameters(EffectBassTreble &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectBassTreble();
   virtual ~EffectBassTreble();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RealtimeSince RealtimeSupport() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;

   // Effect Implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool TransferDataToWindow(const EffectSettings &settings) override;

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;

private:
   // EffectBassTreble implementation

   void InstanceInit(EffectBassTrebleState & data, float sampleRate);
   size_t InstanceProcess(EffectSettings &settings,
      EffectBassTrebleState & data,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

   void Coefficients(double hz, double slope, double gain, double samplerate, int type,
                    double& a0, double& a1, double& a2, double& b0, double& b1, double& b2);
   float DoFilter(EffectBassTrebleState & data, float in);

   void OnBassText(wxCommandEvent & evt);
   void OnTrebleText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);
   void OnBassSlider(wxCommandEvent & evt);
   void OnTrebleSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);
   void OnLinkCheckbox(wxCommandEvent & evt);

   // Auto-adjust gain to reduce variation in peak level
   void UpdateGain(double oldVal, int control );

private:
   EffectBassTrebleState mMaster;
   std::vector<EffectBassTrebleState> mSlaves;

   double      mBass;
   double      mTreble;
   double      mGain;
   bool        mLink;

   wxSlider    *mBassS;
   wxSlider    *mTrebleS;
   wxSlider    *mGainS;

   wxTextCtrl  *mBassT;
   wxTextCtrl  *mTrebleT;
   wxTextCtrl  *mGainT;

   wxCheckBox  *mLinkCheckBox;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Bass{ &EffectBassTreble::mBass,
   L"Bass",          0.0,     -30.0,   30.0,    1  };
static constexpr EffectParameter Treble{ &EffectBassTreble::mTreble,
   L"Treble",        0.0,     -30.0,   30.0,    1  };
static constexpr EffectParameter Gain{ &EffectBassTreble::mGain,
   L"Gain",          0.0,     -30.0,   30.0,    1  };
static constexpr EffectParameter Link{ &EffectBassTreble::mLink,
   L"Link Sliders",  false,    false,  true,    1  };
};

#endif
