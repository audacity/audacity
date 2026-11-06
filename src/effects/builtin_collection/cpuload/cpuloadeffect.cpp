/*
 * Audacity: A Digital Audio Editor
 */
#include "cpuloadeffect.h"

#include <algorithm>
#include <chrono>
#include <cstdint>

#include "au3-command-parameters/ShuttleAutomation.h"

using namespace au::effects;

namespace {
//! Keeps the result of the calibration, so that it isn't optimized away
volatile double calibrationSink = 0.0;
//! Where the calibration starts, unknown to the compiler, so that it doesn't compute the work at compile time
volatile double calibrationSeed = 0.5;

//! The work: a chain of dependent multiply-adds, which doesn't vectorize and doesn't fold away
double burn(double state, uint64_t iterations)
{
    for (uint64_t i = 0; i < iterations; ++i) {
        state = state * 0.999999 + 1e-3;
    }
    return state;
}

//! How many iterations of the work a core does in a second; measured once, on first use
double iterationsPerSecond()
{
    static const double value = [] {
        using Clock = std::chrono::steady_clock;
        constexpr uint64_t batch = 100000;
        // Long enough for the core to reach its clock speed
        constexpr auto duration = std::chrono::milliseconds { 200 };
        double state = calibrationSeed;
        uint64_t done = 0;
        const auto start = Clock::now();
        auto now = start;
        while (now - start < duration) {
            state = burn(state, batch);
            done += batch;
            now = Clock::now();
        }
        calibrationSink = state;
        return done / std::chrono::duration<double>(now - start).count();
    }();
    return value;
}
}

const ComponentInterfaceSymbol CpuLoadEffect::Symbol { "CPU Load", TranslatableString("effects-cpuload", "CPU load") };

const EffectParameterMethods& CpuLoadEffect::Parameters() const
{
    static CapturedParameters<CpuLoadEffect, Load, Spike, SpikeInterval> parameters;
    return parameters;
}

CpuLoadEffect::CpuLoadEffect()
{
    SetLinearEffectFlag(true);
}

ComponentInterfaceSymbol CpuLoadEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString CpuLoadEffect::GetDescription() const
{
    return TranslatableString("effects-cpuload", "Passes the audio through, using CPU time, to test performance");
}

ManualPageID CpuLoadEffect::ManualPage() const
{
    return {};
}

::EffectType CpuLoadEffect::GetType() const
{
    return EffectTypeProcess;
}

EffectGroup CpuLoadEffect::GetGroup() const
{
    return EffectGroup::Special;
}

auto CpuLoadEffect::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::After_3_1;
}

std::shared_ptr<::EffectInstance> CpuLoadEffect::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

bool CpuLoadEffect::Instance::ProcessInitialize(EffectSettings&, double sampleRate, ChannelNames)
{
    iterationsPerSecond();
    m_processors.assign(1, Processor { sampleRate });
    return true;
}

size_t CpuLoadEffect::Instance::ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
                                             size_t blockLen)
{
    return process(0, settings, inBlock, outBlock, blockLen);
}

bool CpuLoadEffect::Instance::RealtimeInitialize(EffectSettings&, double, size_t)
{
    // Not on the audio thread
    iterationsPerSecond();
    SetBlockSize(512);
    m_processors.clear();
    return true;
}

bool CpuLoadEffect::Instance::RealtimeAddProcessor(EffectSettings&, EffectOutputs*, unsigned, float sampleRate)
{
    m_processors.push_back(Processor { sampleRate });
    return true;
}

bool CpuLoadEffect::Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    m_processors.clear();
    return true;
}

size_t CpuLoadEffect::Instance::RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
                                                size_t numSamples)
{
    if (group >= m_processors.size()) {
        return 0;
    }
    return process(group, settings, inBuf, outBuf, numSamples);
}

size_t CpuLoadEffect::Instance::process(size_t group, const EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
                                        size_t numSamples)
{
    if (inBuf[0] != outBuf[0]) {
        std::copy_n(inBuf[0], numSamples, outBuf[0]);
    }

    const auto& s = GetSettings(settings);
    auto& processor = m_processors[group];
    const double perSecond = iterationsPerSecond();
    const double seconds = numSamples / processor.sampleRate;

    // The load is the instance's, shared by its channels
    double iterations = processor.carry + perSecond * seconds * s.load / 100.0 / m_processors.size();

    // The spike is the instance's too: its first channel takes it
    processor.sinceSpike += seconds;
    if (s.spike > 0.0 && processor.sinceSpike >= s.spikeInterval) {
        processor.sinceSpike = 0.0;
        if (group == 0) {
            iterations += perSecond * s.spike / 1000.0;
        }
    }

    const auto whole = static_cast<uint64_t>(iterations);
    processor.carry = iterations - whole;
    processor.state = burn(processor.state, whole);
    return numSamples;
}
