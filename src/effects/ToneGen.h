/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "StatefulEffectUIServices.h"
#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"
#include <float.h> // for DBL_MAX
#include <wx/weakref.h>

class NumericTextCtrl;
class ShuttleGui;

class ToneGenBase : public StatefulPerTrackEffect
{
public:
   static inline ToneGenBase *
   FetchParameters(ToneGenBase &e, EffectSettings &) { return &e; }
   ToneGenBase(bool isChirp);
   virtual ~ToneGenBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

protected:
   double mSampleRate{};
   const bool mChirp;

private:
   // mSample is an external placeholder to remember the last "buffer"
   // position so we use it to reinitialize from where we left
   sampleCount mSample;
   double mPositionInCycles;

protected:
   // If we made these static variables,
   // Tone and Chirp would share the same parameters.
   int mWaveform;
   int mInterpolation;
   double mFrequency0;
   double mFrequency1;
   double mAmplitude0;
   double mAmplitude1;

private:
   double mLogFrequency[2];

   void PostSet();

   const EffectParameterMethods& Parameters() const override;

protected:
   enum kWaveforms
   {
      kSine,
      kSquare,
      kSawtooth,
      kSquareNoAlias,
      kTriangle,
      nWaveforms
   };

   static const EnumValueSymbol kWaveStrings[nWaveforms];

   enum kInterpolations
   {
      kLinear,
      kLogarithmic,
      nInterpolations
   };

   static const EnumValueSymbol kInterStrings[nInterpolations];

// Yes, mFrequency0 and mAmplitude0 are each associated with more than one
static constexpr EffectParameter StartFreq{ &ToneGenBase::mFrequency0,
   L"StartFreq",     440.0,   1.0,     DBL_MAX,                1  };
static constexpr EffectParameter EndFreq{ &ToneGenBase::mFrequency1,
   L"EndFreq",       1320.0,  1.0,     DBL_MAX,                1  };
static constexpr EffectParameter StartAmp{ &ToneGenBase::mAmplitude0,
   L"StartAmp",      0.8,     0.0,     1.0,                    1  };
static constexpr EffectParameter EndAmp{ &ToneGenBase::mAmplitude1,
   L"EndAmp",        0.1,     0.0,     1.0,                    1  };
static constexpr EffectParameter Frequency{ &ToneGenBase::mFrequency0,
   L"Frequency",     440.0,   1.0,     DBL_MAX,                1  };
static constexpr EffectParameter Amplitude{ &ToneGenBase::mAmplitude0,
   L"Amplitude",     0.8,     0.0,     1.0,                    1  };
static constexpr EnumParameter Waveform{ &ToneGenBase::mWaveform,
   L"Waveform",      0,       0,       nWaveforms - 1,      1, kWaveStrings, nWaveforms  };
static constexpr EnumParameter Interp{ &ToneGenBase::mInterpolation,
   L"Interpolation", 0,       0,       nInterpolations - 1, 1, kInterStrings, nInterpolations  };
};

class EffectToneGen : public ToneGenBase, public StatefulEffectUIServices
{
public:
   EffectToneGen(bool isChirp)
       : ToneGenBase { isChirp }
   {
   }

   // Effect implementation

   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

protected:
   DECLARE_EVENT_TABLE()

private:
   // ToneGenBase implementation

   void OnControlUpdate(wxCommandEvent & evt);

   wxWeakRef<wxWindow> mUIParent{};
   NumericTextCtrl *mToneDurationT;
};

class EffectChirp final : public EffectToneGen
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectChirp() : EffectToneGen{ true } {}
   ~EffectChirp() override = default;
};


class EffectTone final : public EffectToneGen
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectTone() : EffectToneGen{ false } {}
   ~EffectTone() override = default;
};

#endif
