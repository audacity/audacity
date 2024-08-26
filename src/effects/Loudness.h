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
#include <wx/weakref.h>

#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include "Biquad.h"
#include "ShuttleAutomation.h"
#include "Track.h"

class wxChoice;
class wxSimplebook;
class EBUR128;
class ShuttleGui;
class WaveChannel;
using Floats = ArrayOf<float>;

class EffectLoudness final :
    public StatefulEffect,
    public StatefulEffectUIServices
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

   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectLoudness implementation

   void AllocBuffers(TrackList &outputs);
   void FreeBuffers();
   static bool GetTrackRMS(WaveChannel &track,
      double curT0, double curT1, float &rms);
   [[nodiscard]] bool ProcessOne(WaveChannel &track, size_t nChannels,
      double curT0, double curT1, float mult, EBUR128 *pLoudnessProcessor);
   void LoadBufferBlock(WaveChannel &track, size_t nChannels,
      sampleCount pos, size_t len);
   bool AnalyseBufferBlock(EBUR128 &loudnessProcessor);
   bool ProcessBufferBlock(float mult);
   [[nodiscard]] bool StoreBufferBlock(WaveChannel &track, size_t nChannels,
      sampleCount pos, size_t len);

   bool UpdateProgress();
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

   double mProgressVal;
   int    mSteps;
   TranslatableString mProgressMsg;
   double mTrackLen;
   double mCurRate;

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
