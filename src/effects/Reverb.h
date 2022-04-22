/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.h
   Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERB__
#define __AUDACITY_EFFECT_REVERB__

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"

class wxCheckBox;
class wxSlider;
class wxSpinCtrl;
class ShuttleGui;

struct Reverb_priv_t;

struct EffectReverbSettings
{
   static constexpr double roomSizeDefault     =  75.0;
   static constexpr double preDelayDefault     =  10.0;
   static constexpr double reverberanceDefault =  50.0;
   static constexpr double hfDampingDefault    =  50.0;
   static constexpr double toneLowDefault      = 100.0;
   static constexpr double toneHighDefault     = 100.0;
   static constexpr double wetGainDefault      =  -1.0;
   static constexpr double dryGainDefault      =  -1.0;
   static constexpr double stereoWidthDefault  = 100.0;
   static constexpr bool   wetOnlyDefault      = false;

   double mRoomSize    { roomSizeDefault };
   double mPreDelay    { preDelayDefault };
   double mReverberance{ reverberanceDefault };
   double mHfDamping   { hfDampingDefault };
   double mToneLow     { toneLowDefault };
   double mToneHigh    { toneHighDefault };
   double mWetGain     { wetGainDefault };
   double mDryGain     { dryGainDefault };
   double mStereoWidth { stereoWidthDefault };
   bool   mWetOnly     { wetOnlyDefault };
};


class EffectReverb final : public StatefulPerTrackEffect
{
public:

   static inline EffectReverbSettings*
   FetchParameters(EffectReverb &e, EffectSettings &) { return &e.mSettings; }
   static const ComponentInterfaceSymbol Symbol;

   EffectReverb();
   virtual ~EffectReverb();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool DoLoadFactoryPreset(int id);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectReverb implementation

   


#define SpinSliderHandlers(n) \
   void On ## n ## Slider(wxCommandEvent & evt); \
   void On ## n ## Text(wxCommandEvent & evt);

   SpinSliderHandlers(RoomSize)
   SpinSliderHandlers(PreDelay)
   SpinSliderHandlers(Reverberance)
   SpinSliderHandlers(HfDamping)
   SpinSliderHandlers(ToneLow)
   SpinSliderHandlers(ToneHigh)
   SpinSliderHandlers(WetGain)
   SpinSliderHandlers(DryGain)
   SpinSliderHandlers(StereoWidth)

#undef SpinSliderHandlers

private:
   unsigned mNumChans {};
   Reverb_priv_t *mP;

   EffectReverbSettings mSettings;

   bool mProcessingEvent;

#define SpinSlider(n) \
   wxSpinCtrl  *m ## n ## T; \
   wxSlider    *m ## n ## S;

   SpinSlider(RoomSize)
   SpinSlider(PreDelay)
   SpinSlider(Reverberance)
   SpinSlider(HfDamping)
   SpinSlider(ToneLow)
   SpinSlider(ToneHigh)
   SpinSlider(WetGain)
   SpinSlider(DryGain)
   SpinSlider(StereoWidth)

#undef SpinSlider

   wxCheckBox  *mWetOnlyC;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter RoomSize{ &EffectReverbSettings::mRoomSize,  L"RoomSize",
                                            EffectReverbSettings::roomSizeDefault,      0,       100,  1  };

static constexpr EffectParameter PreDelay{ &EffectReverbSettings::mPreDelay,  L"Delay",
                                            EffectReverbSettings::preDelayDefault,      0,       200,  1  };

static constexpr EffectParameter Reverberance{ &EffectReverbSettings::mReverberance,  L"Reverberance",
                                                EffectReverbSettings::reverberanceDefault,      0,       100,  1  };

static constexpr EffectParameter HfDamping{ &EffectReverbSettings::mHfDamping,  L"HfDamping",
                                             EffectReverbSettings::hfDampingDefault,      0,       100,  1  };

static constexpr EffectParameter ToneLow{ &EffectReverbSettings::mToneLow,  L"ToneLow",
                                           EffectReverbSettings::toneLowDefault,     0,       100,  1  };

static constexpr EffectParameter ToneHigh{ &EffectReverbSettings::mToneHigh,   L"ToneHigh",
                                            EffectReverbSettings::toneHighDefault,     0,       100,  1  };

static constexpr EffectParameter WetGain{ &EffectReverbSettings::mWetGain,   L"WetGain",
                                           EffectReverbSettings::wetGainDefault,      -20,     10,   1  };

static constexpr EffectParameter DryGain{ &EffectReverbSettings::mDryGain,   L"DryGain",
                                           EffectReverbSettings::dryGainDefault,      -20,     10,   1  };

static constexpr EffectParameter StereoWidth{ &EffectReverbSettings::mStereoWidth,   L"StereoWidth",
                                               EffectReverbSettings::stereoWidthDefault,     0,       100,  1  };

static constexpr EffectParameter WetOnly{ &EffectReverbSettings::mWetOnly,   L"WetOnly",
                                           EffectReverbSettings::wetOnlyDefault,   false,   true, 1  };
};

#endif
