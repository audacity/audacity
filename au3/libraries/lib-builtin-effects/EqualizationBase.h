#pragma once

#include "EqualizationCurvesList.h"
#include "EqualizationFilter.h"
#include "SampleFormat.h"
#include "StatefulEffect.h"
#include "WaveTrack.h"

struct EqualizationParameters;

class BUILTIN_EFFECTS_API EqualizationBase : public StatefulEffect
{
public:
    static inline EqualizationParameters*
    FetchParameters(EqualizationBase& e, EffectSettings&)
    {
        return &e.mParameters;
    }

    static const ComponentInterfaceSymbol Symbol;

    EqualizationBase(int Options = kEqLegacy);

    virtual ~EqualizationBase();

    // ComponentInterface implementation

    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;
    bool
    VisitSettings(SettingsVisitor& visitor, EffectSettings& settings) override;
    bool VisitSettings(
        ConstSettingsVisitor& visitor, const EffectSettings& settings) const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage
    LoadFactoryPreset(int id, EffectSettings& settings) const override;

    // Effect implementation

    bool Init() override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;

protected:
    // EqualizationBase implementation

    struct Task
    {
        Task(size_t M, size_t idealBlockLen, WaveChannel& channel)
            : buffer{idealBlockLen}
            , idealBlockLen{idealBlockLen}
            , output{channel}
            , leftTailRemaining{(M - 1) / 2}
        {
            memset(lastWindow, 0, windowSize * sizeof(float));
        }

        void AccumulateSamples(constSamplePtr buffer, size_t len)
        {
            auto leftTail = std::min(len, leftTailRemaining);
            leftTailRemaining -= leftTail;
            len -= leftTail;
            buffer += leftTail * sizeof(float);
            output.Append(buffer, floatSample, len);
        }

        static constexpr auto windowSize = EqualizationFilter::windowSize;
        Floats window1 { windowSize };
        Floats window2 { windowSize };

        Floats buffer;
        const size_t idealBlockLen;

        // These pointers are swapped after each FFT window
        float* thisWindow { window1.get() };
        float* lastWindow { window2.get() };

        // a new WaveChannel to hold all of the output,
        // including 'tails' each end
        WaveChannel& output;

        size_t leftTailRemaining;
    };

    bool ProcessOne(
        Task& task, int count, const WaveChannel& t, sampleCount start, sampleCount len);

    EqualizationFilter mParameters;
    EqualizationCurvesList mCurvesList { mParameters };
    const int mOptions;

    const EffectParameterMethods& Parameters() const override;
};
