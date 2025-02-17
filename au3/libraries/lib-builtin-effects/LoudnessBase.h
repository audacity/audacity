/**********************************************************************

  Audacity: A Digital Audio Editor

  LoudnessBase.h

  Max Maisel (based on Normalize effect)

**********************************************************************/
#pragma once

#include "MemoryX.h"
#include "SettingsVisitor.h"
#include "StatefulEffect.h"

class WaveChannel;
class EBUR128;
using Floats = ArrayOf<float>;

class BUILTIN_EFFECTS_API LoudnessBase : public StatefulEffect
{
public:
    enum kNormalizeTargets
    {
        kLoudness,
        kRMS,
        nAlgos
    };

    static inline LoudnessBase* FetchParameters(LoudnessBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    LoudnessBase();
    virtual ~LoudnessBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    bool UpdateProgress();

private:
    // LoudnessBase implementation

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

protected:
    bool mStereoInd;
    double mLUFSLevel;
    double mRMSLevel;
    bool mDualMono;
    int mNormalizeTo;

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

    static constexpr EffectParameter StereoInd {
        &LoudnessBase::mStereoInd, L"StereoIndependent", false, false, true, 1
    };
    static constexpr EffectParameter LUFSLevel {
        &LoudnessBase::mLUFSLevel, L"LUFSLevel", -23.0, -145.0, 0.0, 1
    };
    static constexpr EffectParameter RMSLevel {
        &LoudnessBase::mRMSLevel, L"RMSLevel", -20.0, -145.0, 0.0, 1
    };
    static constexpr EffectParameter DualMono {
        &LoudnessBase::mDualMono, L"DualMono", true, false, true, 1
    };
    static constexpr EffectParameter NormalizeTo { &LoudnessBase::mNormalizeTo,
                                                   L"NormalizeTo",
                                                   (int)kLoudness,
                                                   0,
                                                   nAlgos - 1,
                                                   1 };
};
