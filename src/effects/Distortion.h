/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.h

  Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DISTORTION__
#define __AUDACITY_EFFECT_DISTORTION__

#include <queue>

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"

class ShuttleGui;

#define STEPS 1024      // number of +ve or -ve steps in lookup tabe
#define TABLESIZE 2049  // size of lookup table (steps * 2 + 1)

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

   double mThreshold;
   bool mbSavedFilterState;
};


struct EffectDistortionSettings
{
   static constexpr int    mDefaultTableChoiceIndx = 0;
   static constexpr bool   mDefaultDCBlock         = false;
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


class EffectDistortion final : public StatefulPerTrackEffect
{
public:
   struct Params;
   static inline EffectDistortionSettings*
   FetchParameters(EffectDistortion &e, EffectSettings &) { return &e.mSettings; }
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
   OptionalMessage DoLoadFactoryPreset(int id);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs *pOutputs,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   //bool TransferDataToWindow(const EffectSettings &settings) override;

   struct Validator;

private:

   enum control
   {
      ID_Type = 10000,
      ID_DCBlock,
      ID_Threshold,
      ID_NoiseFloor,
      ID_Param1,
      ID_Param2,
      ID_Repeats,
   };
   // EffectDistortion implementation

   void InstanceInit(EffectDistortionState & data, float sampleRate);
   size_t InstanceProcess(EffectSettings &settings,
      EffectDistortionState & data,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

   void MakeTable();
   float WaveShaper(float sample);
   float DCFilter(EffectDistortionState & data, float sample);

   // Preset tables for gain lookup

   void HardClip();           // hard clipping
   void SoftClip();           // soft clipping
   void ExponentialTable();   // exponential mapping
   void LogarithmicTable();   // logarithmic mapping
   void HalfSinTable();
   void CubicTable();
   void EvenHarmonicTable();
   void SineTable();
   void Leveller();           // 'Leveller' wavetable is modeled on the legacy effect of the same name.
   void Rectifier();          // 0% = Dry, 50% = half-wave rectified, 100% = full-wave rectified (abs value).
   void HardLimiter();        // Same effect as the LADSPA "hardLimiter 1413"

   // Wavetable helper functions

   void CopyHalfTable();   // for symmetric tables

   // Used by Soft Clipping but could be used for other tables.
   // Log curve formula: y = T + (((e^(RT - Rx)) - 1) / -R)
   // where R is the ratio, T is the threshold, and x is from T to 1. 
   inline float LogCurve(double threshold, float value, double ratio);

   // Used by Cubic curve but could be used for other tables
   // Cubic formula: y = x - (x^3 / 3.0)
   inline double Cubic(double x);


private:
   EffectDistortionState mMaster;
   std::vector<EffectDistortionState> mSlaves;

   double mTable[TABLESIZE];    

   // mMakeupGain is used by some distortion types to pass the
   // amount of gain required to bring overall effect gain to unity
   double mMakeupGain;

   int mTypChoiceIndex;

   EffectDistortionSettings mSettings;

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
{ &EffectDistortionSettings::mParam2, L"Parameter 2",    50.0,    0.0,   100.0,                1    };

static constexpr EffectParameter Repeats{
   &EffectDistortionSettings::mRepeats,  L"Repeats",
    EffectDistortionSettings::mDefaultRepeats,       0,       5,                  1    };

};

#endif
