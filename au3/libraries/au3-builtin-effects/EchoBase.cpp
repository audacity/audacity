/**********************************************************************

  Audacity: A Digital Audio Editor

  EchoBase.cpp

  Dominic Mazzoni
  Vaughan Johnson (dialog)

***********************************************************************/
#include "EchoBase.h"
#include "BasicUI.h"
#include "ShuttleAutomation.h"

const EffectParameterMethods& EchoBase::Parameters() const
{
    static CapturedParameters<EchoBase, Delay, Decay> parameters;
    return parameters;
}

const ComponentInterfaceSymbol EchoBase::Symbol { XO("Echo") };

EchoBase::EchoBase()
{
    SetLinearEffectFlag(true);
}

EchoBase::~EchoBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EchoBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString EchoBase::GetDescription() const
{
    return XO("Repeats the selected audio again and again");
}

ManualPageID EchoBase::ManualPage() const
{
    return L"Echo";
}

// EffectDefinitionInterface implementation

EffectType EchoBase::GetType() const
{
    return EffectTypeProcess;
}

bool EchoBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames)
{
    auto& echoSettings = GetSettings(settings);
    if (echoSettings.delay == 0.0) {
        return false;
    }

    histPos = 0;
    auto requestedHistLen = (sampleCount)(sampleRate * echoSettings.delay);

    // Guard against extreme delay values input by the user
    try
    {
        // Guard against huge delay values from the user.
        // Don't violate the assertion in as_size_t
        if (
            requestedHistLen
            != (histLen = static_cast<size_t>(requestedHistLen.as_long_long()))) {
            throw std::bad_alloc {}
        }
        history.reinit(histLen, true);
    }
    catch (const std::bad_alloc&)
    {
        BasicUI::ShowMessageBox(XO("Requested value exceeds memory capacity."));
        return false;
    }

    return history != NULL;
}

bool EchoBase::Instance::ProcessFinalize() noexcept
{
    return true;
}

size_t EchoBase::Instance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    auto& echoSettings = GetSettings(settings);

    const float* ibuf = inBlock[0];
    float* obuf = outBlock[0];

    for (decltype(blockLen) i = 0; i < blockLen; i++, histPos++) {
        if (histPos == histLen) {
            histPos = 0;
        }
        history[histPos] = obuf[i]
                               =ibuf[i] + history[histPos] * echoSettings.decay;
    }

    return blockLen;
}
