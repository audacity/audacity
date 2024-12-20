#include "noisegenerator.h"
#include <cmath>

#include "log.h"

using namespace au::effects;

const ComponentInterfaceSymbol NoiseGenerator::Symbol { XO("Noise") };

NoiseGenerator::NoiseGenerator()
    : GeneratorEffect(mT0, mT1)
{
    SetLinearEffectFlag(true);
}

NoiseGenerator::~NoiseGenerator()
{
}

std::shared_ptr<EffectInstance> NoiseGenerator::MakeInstance() const
{
    return std::make_shared <Instance>(*this);
}

// ComponentInterface implementation

ComponentInterfaceSymbol NoiseGenerator::GetSymbol() const
{
    return Symbol;
}

TranslatableString NoiseGenerator::GetDescription() const
{
    return XO("Generates one of three different types of noise");
}

ManualPageID NoiseGenerator::ManualPage() const
{
    return L"Noise";
}

// EffectDefinitionInterface implementation
EffectType NoiseGenerator::GetType() const
{
    return EffectTypeGenerate;
}

RegistryPaths NoiseGenerator::GetFactoryPresets() const
{
    return {};
}

OptionalMessage NoiseGenerator::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    return {};
}

NoiseGenerator::Instance::Instance(const PerTrackEffect& effect)
    : PerTrackEffect::Instance{effect}
{
}

unsigned int NoiseGenerator::Instance::GetAudioOutCount() const
{
    return 1;
}

unsigned int NoiseGenerator::Instance::GetAudioInCount() const
{
    return 0;
}

bool NoiseGenerator::Instance::ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    return InstanceInit(settings, sampleRate, chanMap);
}

size_t NoiseGenerator::Instance::ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
                                              size_t blockLen)
{
    return InstanceProcess(settings, inBlock, outBlock, blockLen);
}

bool NoiseGenerator::Instance::InstanceInit(EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    UNUSED(settings);
    mSampleRate = sampleRate;
    y = z = buf0 = buf1 = buf2 = buf3 = buf4 = buf5 = buf6 = 0;
    return true;
}

size_t NoiseGenerator::Instance::InstanceProcess(
    EffectSettings& settings, const float* const*, float* const* outbuf, size_t size)
{
    auto& ns = GetSettings(settings);

    float* buffer = outbuf[0];

    float white { 0 };
    float amplitude { 0 };
    float div = ((float)RAND_MAX) / 2.0f;

    switch (ns.type) {
    default:
    case NoiseSettings::Type::White:
        for (decltype(size) i = 0; i < size; i++) {
            buffer[i] = ns.amplitude * ((rand() / div) - 1.0f);
        }
        break;

    case NoiseSettings::Type::Pink:
        // based on Paul Kellet's "instrumentation grade" algorithm.

        // 0.129f is an experimental normalization factor.
        amplitude = ns.amplitude * 0.129f;
        for (decltype(size) i = 0; i < size; i++) {
            white = (rand() / div) - 1.0f;
            buf0 = 0.99886f * buf0 + 0.0555179f * white;
            buf1 = 0.99332f * buf1 + 0.0750759f * white;
            buf2 = 0.96900f * buf2 + 0.1538520f * white;
            buf3 = 0.86650f * buf3 + 0.3104856f * white;
            buf4 = 0.55000f * buf4 + 0.5329522f * white;
            buf5 = -0.7616f * buf5 - 0.0168980f * white;
            buffer[i] = amplitude * (buf0 + buf1 + buf2 + buf3 + buf4 + buf5
                                     + buf6 + white * 0.5362);
            buf6 = white * 0.115926;
        }
        break;

    case NoiseSettings::Type::Brownian:
        // float leakage=0.997; // experimental value at 44.1kHz
        // double scaling = 0.05; // experimental value at 44.1kHz
        // min and max protect against instability at extreme sample rates.
        float leakage = ((mSampleRate - 144.0) / mSampleRate < 0.9999)
                        ? (mSampleRate - 144.0) / mSampleRate
                        : 0.9999f;

        float scaling
            =(9.0 / sqrt(mSampleRate) > 0.01) ? 9.0 / sqrt(mSampleRate) : 0.01f;

        for (decltype(size) i = 0; i < size; i++) {
            white = (rand() / div) - 1.0f;
            z = leakage * y + white * scaling;
            y = fabs(z) > 1.0 ? leakage * y - white * scaling : z;
            buffer[i] = ns.amplitude * y;
        }
        break;
    }

    return size;
}

bool NoiseSettings::isApplyAllowed() const
{
    return type < Type::Count && amplitude >= amplitudeMin && amplitude <= amplitudeMax;
}
