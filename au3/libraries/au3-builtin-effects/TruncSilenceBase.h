/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilenceBase.h

  Lynn Allan (from DM's Normalize)
  //ToDo ... put BlendFrames in Effects, Project, or other class
  //ToDo ... Use ZeroCrossing logic to improve blend
  //ToDo ... BlendFrames on "fade-out"
  //ToDo ... BlendFrameCount is a user-selectable parameter
  //ToDo ... Detect transient signals that are too short to interrupt the TruncatableSilence
  Philip Van Baren (more options and boundary fixes)

**********************************************************************/
#pragma once

#include "ShuttleAutomation.h"
#include "StatefulEffect.h"
#include "Track.h"

class RegionList;

class BUILTIN_EFFECTS_API TruncSilenceBase : public StatefulEffect
{
public:
    static inline TruncSilenceBase*
    FetchParameters(TruncSilenceBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    TruncSilenceBase();
    virtual ~TruncSilenceBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    bool LoadSettings(
        const CommandParameters& parms, EffectSettings& settings) const override;

    // Effect implementation

    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;

    // Analyze a single track to find silences
    // If inputLength is not NULL we are calculating the minimum
    // amount of input for previewing.
    bool Analyze(
        RegionList& silenceList, RegionList& trackSilences, const WaveTrack& wt, sampleCount* silentFrame, sampleCount* index,
        int whichTrack, double* inputLength = nullptr, double* minInputLength = nullptr) const;

    bool Process(EffectInstance& instance, EffectSettings& settings) override;

    bool NeedsDither() const override;

protected:
    // TruncSilenceBase implementation

    // ToDo ... put BlendFrames in Effects, Project, or other class
    // void BlendFrames(float* buffer, int leftIndex, int rightIndex, int
    // blendFrameCount);
    void Intersect(RegionList& dest, const RegionList& src);

    bool ProcessIndependently();
    bool ProcessAll();
    bool FindSilences(
        RegionList& silences, const TrackIterRange<const WaveTrack>& range);
    bool DoRemoval(
        const RegionList& silences, const TrackIterRange<Track>& range, unsigned iGroup, unsigned nGroups, double& totalCutLen);

    double mThresholdDB {};
    int mActionIndex;
    double mInitialAllowedSilence;
    double mTruncLongestAllowedSilence;
    double mSilenceCompressPercent;
    bool mbIndependent;

    size_t mBlendFrameCount;

    const EffectParameterMethods& Parameters() const override;

    enum kActions
    {
        kTruncate,
        kCompress,
        nActions
    };

    static const EnumValueSymbol kActionStrings[nActions];

    static constexpr EffectParameter Threshold {
        &TruncSilenceBase::mThresholdDB, L"Threshold", -20.0, -80.0, -20.0, 1
    };
    static constexpr EnumParameter ActIndex { &TruncSilenceBase::mActionIndex,
                                              L"Action",
                                              (int)kTruncate,
                                              0,
                                              nActions - 1,
                                              1,
                                              kActionStrings,
                                              nActions };
    static constexpr EffectParameter Minimum {
        &TruncSilenceBase::mInitialAllowedSilence,
        L"Minimum",
        0.5,
        0.001,
        10000.0,
        1
    };
    static constexpr EffectParameter Truncate {
        &TruncSilenceBase::mTruncLongestAllowedSilence,
        L"Truncate",
        0.5,
        0.0,
        10000.0,
        1
    };
    static constexpr EffectParameter Compress {
        &TruncSilenceBase::mSilenceCompressPercent,
        L"Compress",
        50.0,
        0.0,
        99.9,
        1
    };
    static constexpr EffectParameter Independent {
        &TruncSilenceBase::mbIndependent, L"Independent", false, false, true, 1
    };
};
