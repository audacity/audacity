/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.h

  Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DISTORTION__
#define __AUDACITY_EFFECT_DISTORTION__

#include <queue>

#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"

class ShuttleGui;

#define STEPS 1024      // number of +ve or -ve steps in lookup tabe
#define TABLESIZE 2049  // size of lookup table (steps * 2 + 1)

const bool defaultDCBlock = false;

class EffectDistortionState
{
public:
   float       samplerate;
   sampleCount skipcount;
   int         tablechoiceindx;
   bool        dcblock;
   double      threshold;
   double      noisefloor;
   double      param1;
   double      param2;
   int         repeats;

   // DC block filter variables
   std::queue<float> queuesamples;
   double queuetotal;

   bool mbSavedFilterState{ defaultDCBlock };

   // mMakeupGain is used by some distortion types to pass the
   // amount of gain required to bring overall effect gain to unity
   double mMakeupGain{ 1.0 };
};


struct EffectDistortionSettings
{
   static constexpr int    mDefaultTableChoiceIndx = 0;
   static constexpr bool   mDefaultDCBlock         = defaultDCBlock;
   static constexpr double mDefaultThreshold_dB    = -6.0;
   static constexpr double mDefaultNoiseFloor      = -70.0;
   static constexpr double mDefaultParam1          =  50.0;
   static constexpr double mDefaultParam2          =  50.0;
   static constexpr int    mDefaultRepeats         = 1;

   int    mTableChoiceIndx{ mDefaultTableChoiceIndx };
   bool   mDCBlock        { mDefaultDCBlock };
   double mThreshold_dB   { mDefaultThreshold_dB };
   double mNoiseFloor     { mDefaultNoiseFloor };
   double mParam1         { mDefaultParam1 };
   double mParam2         { mDefaultParam2 };
   int    mRepeats        { mDefaultRepeats };
};


class EffectDistortion final : public EffectWithSettings<EffectDistortionSettings, PerTrackEffect>
{
public:
   struct Params;
   
   static const ComponentInterfaceSymbol Symbol;

   EffectDistortion();
   virtual ~EffectDistortion();


   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RealtimeSince RealtimeSupport() const override;
   RegistryPaths GetFactoryPresets() const override;
   OptionalMessage LoadFactoryPreset(int id, EffectSettings &settings)
      const override;
   OptionalMessage DoLoadFactoryPreset(int id, EffectSettings& settings);


   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;

   struct Validator;
   struct Instance;
   std::shared_ptr<EffectInstance> MakeInstance() const override;

private:

   enum control
   {
      ID_DCBlock = 10001,
      ID_Threshold,
      ID_NoiseFloor,
      ID_Param1,
      ID_Param2,
      ID_Repeats,
   };


private:

   int mTypChoiceIndex;

   const EffectParameterMethods& Parameters() const override;

   enum kTableType
   {
      kHardClip,
      kSoftClip,
      kHalfSinCurve,
      kExpCurve,
      kLogCurve,
      kCubic,
      kEvenHarmonics,
      kSinCurve,
      kLeveller,
      kRectifier,
      kHardLimiter,
      nTableTypes
   };

   static const EnumValueSymbol kTableTypeStrings[nTableTypes];

// (Note: 'Repeats' is the total number of times the effect is applied.)
static constexpr EnumParameter TableTypeIndx
{ &EffectDistortionSettings::mTableChoiceIndx, L"Type",
   EffectDistortionSettings::mDefaultTableChoiceIndx,    0,      nTableTypes-1,    1, kTableTypeStrings, nTableTypes    };

static constexpr EffectParameter DCBlock
{ &EffectDistortionSettings::mDCBlock, L"DC Block",
   EffectDistortionSettings::mDefaultDCBlock ,   false,   true,                1    };

static constexpr EffectParameter Threshold_dB
{ &EffectDistortionSettings::mThreshold_dB, L"Threshold dB",
   EffectDistortionSettings::mDefaultThreshold_dB,  -100.0,     0.0,             1000.0f };

static constexpr EffectParameter NoiseFloor
{ &EffectDistortionSettings::mNoiseFloor, L"Noise Floor",
   EffectDistortionSettings::mDefaultNoiseFloor ,  -80.0,   -20.0,                1    };

static constexpr EffectParameter Param1
{ &EffectDistortionSettings::mParam1, L"Parameter 1",
   EffectDistortionSettings::mDefaultParam1,    0.0,   100.0,                1    };

static constexpr EffectParameter Param2
{ &EffectDistortionSettings::mParam2, L"Parameter 2",
   EffectDistortionSettings::mDefaultParam2,    0.0,   100.0,                1    };

static constexpr EffectParameter Repeats{
   &EffectDistortionSettings::mRepeats,  L"Repeats",
    EffectDistortionSettings::mDefaultRepeats,       0,       5,                  1    };

};

#endif
