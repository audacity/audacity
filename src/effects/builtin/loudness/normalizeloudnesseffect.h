/*
 * Audacity: A Digital Audio Editor
 */
/**********************************************************************

  Audacity: A Digital Audio Editor

  NormalizeLoudnessEffect.h

  Max Maisel (based on Normalize effect)

**********************************************************************/
#pragma once

#include "libraries/lib-utility/MemoryX.h"
#include "libraries/lib-components/SettingsVisitor.h"
#include "libraries/lib-effects/StatefulEffect.h"

class WaveChannel;
class EBUR128;

namespace au::effects {
using Floats = ArrayOf<float>;

class NormalizeLoudnessEffect : public StatefulEffect
{
public:
    enum kNormalizeTargets
    {
        kLoudness,
        kRMS,
        nAlgos
    };

    static inline NormalizeLoudnessEffect* FetchParameters(NormalizeLoudnessEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    NormalizeLoudnessEffect();
    virtual ~NormalizeLoudnessEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;

    // Effect implementation

    bool Process(::EffectInstance& instance, EffectSettings& settings) override;
    bool UpdateProgress();

private:
    // NormalizeLoudnessEffect implementation

    void AllocBuffers(TrackList& outputs);
    void FreeBuffers();
    static bool
    GetTrackRMS(WaveChannel& track, double curT0, double curT1, float& rms);
    [[nodiscard]] bool ProcessOne(
        WaveChannel& track, size_t nChannels, double curT0, double curT1, float mult, EBUR128* pLoudnessProcessor);
    void LoadBufferBlock(
        WaveChannel& track, size_t nChannels, sampleCount pos, size_t len);
    bool AnalyseBufferBlock(EBUR128& loudnessProcessor);
    bool ProcessBufferBlock(float mult);
    [[nodiscard]] bool StoreBufferBlock(
        WaveChannel& track, size_t nChannels, sampleCount pos, size_t len);

public:
    bool mStereoInd;
    double mLUFSLevel;
    double mRMSLevel;
    bool mDualMono;
    int mNormalizeTo;

private:
    double mProgressVal;
    int mSteps;
    TranslatableString mProgressMsg;
    double mTrackLen;
    double mCurRate;

    Floats mTrackBuffer[2]; // MM: must be increased once surround channels are
                            // supported
    size_t mTrackBufferLen;
    size_t mTrackBufferCapacity;
    bool mProcStereo;

    const EffectParameterMethods& Parameters() const override;

public:
    static constexpr EffectParameter StereoInd {
        &NormalizeLoudnessEffect::mStereoInd, L"StereoIndependent", false, false, true, 1
    };
    static constexpr EffectParameter LUFSLevel {
        &NormalizeLoudnessEffect::mLUFSLevel, L"LUFSLevel", -23.0, -145.0, 0.0, 1
    };
    static constexpr EffectParameter RMSLevel {
        &NormalizeLoudnessEffect::mRMSLevel, L"RMSLevel", -20.0, -145.0, 0.0, 1
    };
    static constexpr EffectParameter DualMono {
        &NormalizeLoudnessEffect::mDualMono, L"DualMono", true, false, true, 1
    };
    static constexpr EffectParameter NormalizeTo { &NormalizeLoudnessEffect::mNormalizeTo,
                                                   L"NormalizeTo",
                                                   (int)kLoudness,
                                                   0,
                                                   nAlgos - 1,
                                                   1 };
};
}
