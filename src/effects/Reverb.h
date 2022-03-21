/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.h
   Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERB__
#define __AUDACITY_EFFECT_REVERB__

#include "Effect.h"
#include "../ShuttleAutomation.h"

class wxCheckBox;
class wxSlider;
class wxSpinCtrl;
class ShuttleGui;

struct Reverb_priv_t;

class EffectReverb final : public Effect
{
public:
   struct Params;
   static inline Params *
   FetchParameters(EffectReverb &e, EffectSettings &) { return &e.mParams; }
   static const ComponentInterfaceSymbol Symbol;

   EffectReverb();
   virtual ~EffectReverb();

   struct Params
   {
      double mRoomSize;
      double mPreDelay;
      double mReverberance;
      double mHfDamping;
      double mToneLow;
      double mToneHigh;
      double mWetGain;
      double mDryGain;
      double mStereoWidth;
      bool mWetOnly;
   };

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id) override;

   // EffectProcessor implementation

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
      ShuttleGui & S, EffectSettingsAccess &access) override;
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

   Params mParams;

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

static constexpr EffectParameter RoomSize{ &EffectReverb::Params::mRoomSize,
   L"RoomSize",      75.0,      0,       100,  1  };
static constexpr EffectParameter PreDelay{ &EffectReverb::Params::mPreDelay,
   L"Delay",         10.0,      0,       200,  1  };
static constexpr EffectParameter Reverberance{ &EffectReverb::Params::mReverberance,
   L"Reverberance",  50.0,      0,       100,  1  };
static constexpr EffectParameter HfDamping{ &EffectReverb::Params::mHfDamping,
   L"HfDamping",     50.0,      0,       100,  1  };
static constexpr EffectParameter ToneLow{ &EffectReverb::Params::mToneLow,
   L"ToneLow",       100.0,     0,       100,  1  };
static constexpr EffectParameter ToneHigh{ &EffectReverb::Params::mToneHigh,
   L"ToneHigh",      100.0,     0,       100,  1  };
static constexpr EffectParameter WetGain{ &EffectReverb::Params::mWetGain,
   L"WetGain",       -1.0,      -20,     10,   1  };
static constexpr EffectParameter DryGain{ &EffectReverb::Params::mDryGain,
   L"DryGain",       -1.0,      -20,     10,   1  };
static constexpr EffectParameter StereoWidth{ &EffectReverb::Params::mStereoWidth,
   L"StereoWidth",   100.0,     0,       100,  1  };
static constexpr EffectParameter WetOnly{ &EffectReverb::Params::mWetOnly,
   L"WetOnly",       false,   false,   true, 1  };
};

#endif
