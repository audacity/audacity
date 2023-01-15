/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.h

  Max Maisel (based on Normalize effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LOUDNESS__
#define __AUDACITY_EFFECT_LOUDNESS__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "StatefulEffect.h"
#include "Biquad.h"
#include "EBUR128.h"
#include "../ShuttleAutomation.h"
#include "Track.h"

class wxChoice;
class wxSimplebook;
class ShuttleGui;

class EffectLoudness final : public StatefulEffect
{
public:
   enum kNormalizeTargets
   {
      kLoudness,
      kRMS,
      nAlgos
   };

   static inline EffectLoudness *
   FetchParameters(EffectLoudness &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectLoudness();
   virtual ~EffectLoudness();

   // ComponentInterface implementation

   ComponentInterfaceSymbol  GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectLoudness implementation

   void AllocBuffers();
   void FreeBuffers();
   bool GetTrackRMS(WaveTrack* track, float& rms);
   bool ProcessOne(EffectContext &context,
      TrackIterRange<WaveTrack> range, bool analyse);
   void LoadBufferBlock(TrackIterRange<WaveTrack> range,
                        sampleCount pos, size_t len);
   bool AnalyseBufferBlock(EffectContext &context);
   bool ProcessBufferBlock(EffectContext &context);
   void StoreBufferBlock(TrackIterRange<WaveTrack> range,
                         sampleCount pos, size_t len);

   bool UpdateProgress(EffectContext &context);
   void OnChoice(wxCommandEvent & evt);
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   wxWeakRef<wxWindow> mUIParent{};

   bool   mStereoInd;
   double mLUFSLevel;
   double mRMSLevel;
   bool   mDualMono;
   int    mNormalizeTo;

   double mCurT0;
   double mCurT1;
   double mProgressVal;
   int    mSteps;
   TranslatableString mProgressMsg;
   double mTrackLen;
   double mCurRate;

   float  mMult;
   float  mRatio;
   float  mRMS[2];
   std::unique_ptr<EBUR128> mLoudnessProcessor;

   wxSimplebook *mBook;
   wxChoice *mChoice;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;
   wxCheckBox *mDualMonoCheckBox;

   Floats mTrackBuffer[2];    // MM: must be increased once surround channels are supported
   size_t mTrackBufferLen;
   size_t mTrackBufferCapacity;
   bool   mProcStereo;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter StereoInd{ &EffectLoudness::mStereoInd,
   L"StereoIndependent",   false,      false,   true,     1  };
static constexpr EffectParameter LUFSLevel{ &EffectLoudness::mLUFSLevel,
   L"LUFSLevel",           -23.0,      -145.0,  0.0,      1  };
static constexpr EffectParameter RMSLevel{ &EffectLoudness::mRMSLevel,
   L"RMSLevel",            -20.0,      -145.0,  0.0,      1  };
static constexpr EffectParameter DualMono{ &EffectLoudness::mDualMono,
   L"DualMono",            true,       false,   true,     1  };
static constexpr EffectParameter NormalizeTo{ &EffectLoudness::mNormalizeTo,
   L"NormalizeTo",         (int)kLoudness , 0    ,   nAlgos-1, 1  };
};

#endif
