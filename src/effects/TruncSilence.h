/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.h

  Lynn Allan (from DM's Normalize)
  //ToDo ... put BlendFrames in Effects, Project, or other class
  //ToDo ... Use ZeroCrossing logic to improve blend
  //ToDo ... BlendFrames on "fade-out"
  //ToDo ... BlendFrameCount is a user-selectable parameter
  //ToDo ... Detect transient signals that are too short to interrupt the TruncatableSilence
  Philip Van Baren (more options and boundary fixes)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TRUNC_SILENCE__
#define __AUDACITY_EFFECT_TRUNC_SILENCE__

#include "Effect.h"
#include "ShuttleAutomation.h"

class ShuttleGui;
class wxChoice;
class wxTextCtrl;
class wxCheckBox;

class RegionList;

class EffectTruncSilence final : public StatefulEffect
{
public:
   static inline EffectTruncSilence *
   FetchParameters(EffectTruncSilence &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectTruncSilence();
   virtual ~EffectTruncSilence();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;

   // Effect implementation

   double CalcPreviewInputLength(
      const EffectSettings &settings, double previewLength) const override;

   // Analyze a single track to find silences
   // If inputLength is not NULL we are calculating the minimum
   // amount of input for previewing.
   bool Analyze(RegionList &silenceList,
                        RegionList &trackSilences,
                        const WaveTrack *wt,
                        sampleCount* silentFrame,
                        sampleCount* index,
                        int whichTrack,
                        double* inputLength = NULL,
                        double* minInputLength = NULL) const;

   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   bool NeedsDither() const override;

private:
   // EffectTruncSilence implementation

   //ToDo ... put BlendFrames in Effects, Project, or other class
   // void BlendFrames(float* buffer, int leftIndex, int rightIndex, int blendFrameCount);
   void Intersect(RegionList &dest, const RegionList & src);

   void OnControlChange(wxCommandEvent & evt);
   void UpdateUI();

   bool ProcessIndependently();
   bool ProcessAll();
   bool FindSilences
      (RegionList &silences, const TrackList *list,
       const Track *firstTrack, const Track *lastTrack);
   bool DoRemoval
      (const RegionList &silences, unsigned iGroup, unsigned nGroups, Track *firstTrack, Track *lastTrack,
       double &totalCutLen);

   wxWeakRef<wxWindow> mUIParent{};

   double mThresholdDB {} ;
   int mActionIndex;
   double mInitialAllowedSilence;
   double mTruncLongestAllowedSilence;
   double mSilenceCompressPercent;
   bool mbIndependent;

   size_t mBlendFrameCount;

   wxTextCtrl *mThresholdText;
   wxChoice *mActionChoice;
   wxTextCtrl *mInitialAllowedSilenceT;
   wxTextCtrl *mTruncLongestAllowedSilenceT;
   wxTextCtrl *mSilenceCompressPercentT;
   wxCheckBox *mIndependent;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

   enum kActions
   {
      kTruncate,
      kCompress,
      nActions
   };

   static const EnumValueSymbol kActionStrings[nActions];

static constexpr EffectParameter Threshold{ &EffectTruncSilence::mThresholdDB,
   L"Threshold",  -20.0,      -80.0,   -20.0,                     1  };
static constexpr EnumParameter ActIndex{ &EffectTruncSilence::mActionIndex,
   L"Action",     (int)kTruncate,  0,       nActions - 1,           1, kActionStrings, nActions };
static constexpr EffectParameter Minimum{ &EffectTruncSilence::mInitialAllowedSilence,
   L"Minimum",    0.5,        0.001,   10000.0,                   1  };
static constexpr EffectParameter Truncate{ &EffectTruncSilence::mTruncLongestAllowedSilence,
   L"Truncate",   0.5,        0.0,     10000.0,                   1  };
static constexpr EffectParameter Compress{ &EffectTruncSilence::mSilenceCompressPercent,
   L"Compress",   50.0,       0.0,     99.9,                      1  };
static constexpr EffectParameter Independent{ &EffectTruncSilence::mbIndependent,
   L"Independent", false,     false,   true,                      1  };
};

#endif
