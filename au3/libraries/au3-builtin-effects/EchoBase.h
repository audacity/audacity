/**********************************************************************

  Audacity: A Digital Audio Editor

  EchoBase.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/
#pragma once

#include "MemoryX.h"
#include "PerTrackEffect.h"
#include "SettingsVisitor.h"
#include <cfloat>

using Floats = ArrayOf<float>;

struct EchoSettings
{
    static constexpr double delayDefault = 1.0;
    static constexpr double decayDefault = 0.5;

    double delay { delayDefault };
    double decay { decayDefault };
};

class BUILTIN_EFFECTS_API EchoBase : public EffectWithSettings<EchoSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EchoBase();
    virtual ~EchoBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    struct BUILTIN_EFFECTS_API Instance : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize
    {
        Instance(const PerTrackEffect& effect)
            : PerTrackEffect::Instance {effect}
        {
        }

        bool ProcessInitialize(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;

        size_t ProcessBlock(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        bool ProcessFinalize() noexcept override;

        unsigned GetAudioOutCount() const override
        {
            return 1;
        }

        unsigned GetAudioInCount() const override
        {
            return 1;
        }

        Floats history;
        size_t histPos;
        size_t histLen;
    };

protected:
    // EchoBase implementation
    const EffectParameterMethods& Parameters() const override;

#if 0
    // TODO simplify like this in C++20
    using ParametersType = CapturedParameters<EchoBase,
                                              EffectParameter {
                                                  & EchoBase::delay, L"Delay",   1.0f, 0.001f,  FLT_MAX, 1.0f
                                              },
                                              EffectParameter {
                                                  & EchoBase::decay, L"Decay",   0.5f, 0.0f,    FLT_MAX, 1.0f
                                              }
                                              >;
#else

    static constexpr EffectParameter Delay { &EchoSettings::delay,
                                             L"Delay",
                                             EchoSettings::delayDefault,
                                             0.001f,
                                             FLT_MAX,
                                             1.0f };
    static constexpr EffectParameter Decay { &EchoSettings::decay,
                                             L"Decay",
                                             EchoSettings::decayDefault,
                                             0.0f,
                                             FLT_MAX,
                                             1.0f };

#endif
};
