/*
 * Audacity: A Digital Audio Editor
 *
 * Audacity test VST3 plugin: a VST3 effect whose *module load* is controlled by a
 * gate file, so plugin validation / loading can be held, released, or made to fail
 * on purpose while testing the non-blocking plugin validation (#11746).
 *
 * When processing it applies an amplitude tremolo (~5 Hz) whose depth is set by an
 * "Effect depth" parameter, so whether the effect is active/bypassed - and whether a
 * saved parameter value survived a reload - is immediately audible. Handy for
 * checking that a plugin can be re-validated while a track using it is playing.
 *
 * Gate file: $AU_TEST_VST3_GATE_FILE, or <temp dir>/au_test_vst3_gate.
 * Contents (first integer):
 *    1  (or missing/unreadable file)  load normally
 *    0                                 wait, polling the file, until it changes
 *   -1                                 crash (null dereference) while loading
 *    2                                 refuse to load (ModuleEntry returns false)
 * While waiting, a heartbeat line is written to stderr about once a second.
 */

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>

#include "public.sdk/source/main/pluginfactory.h"
#include "public.sdk/source/vst/vstsinglecomponenteffect.h"
#include "pluginterfaces/vst/ivstaudioprocessor.h"
#include "pluginterfaces/vst/ivstparameterchanges.h"
#include "base/source/fstreamer.h"

using namespace Steinberg;
using namespace Steinberg::Vst;

namespace {
constexpr int GATE_LOAD = 1;
constexpr int GATE_WAIT = 0;
constexpr int GATE_CRASH = -1;
constexpr int GATE_REFUSE = 2;

std::filesystem::path gateFilePath()
{
    if (const char* env = std::getenv("AU_TEST_VST3_GATE_FILE"); env && *env) {
        return env;
    }
    return std::filesystem::temp_directory_path() / "au_test_vst3_gate";
}

// a missing or malformed file opens the gate, so a forgotten file never hangs a host
int readGate(const std::filesystem::path& path)
{
    std::ifstream in(path);
    int value = GATE_LOAD;
    if (!in || !(in >> value)) {
        return GATE_LOAD;
    }
    return value;
}

void say(const char* message, const std::filesystem::path& path, int gate)
{
    std::fprintf(stderr, "[AuTestGate] %s (gate file %s = %d)\n", message, path.string().c_str(), gate);
    std::fflush(stderr);
}
}

// Called from ModuleEntry (linuxmain.cpp) right after the host dlopen'ed us.
bool InitModule()
{
    const std::filesystem::path path = gateFilePath();
    auto lastHeartbeat = std::chrono::steady_clock::now() - std::chrono::hours(1);

    for (;;) {
        const int gate = readGate(path);
        switch (gate) {
        case GATE_WAIT: {
            const auto now = std::chrono::steady_clock::now();
            if (now - lastHeartbeat >= std::chrono::seconds(1)) {
                say("gate closed, waiting", path, gate);
                lastHeartbeat = now;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(250));
            continue;
        }
        case GATE_CRASH: {
            say("gate says crash", path, gate);
            volatile int* nowhere = nullptr;
            *nowhere = 1;
            return false;
        }
        case GATE_REFUSE:
            say("gate says refuse to load", path, gate);
            return false;
        case GATE_LOAD:
            say("gate open, loading", path, gate);
            return true;
        default:
            say("unknown gate value, loading anyway", path, gate);
            return true;
        }
    }
}

bool DeinitModule()
{
    return true;
}

namespace {
constexpr ParamID kDepthParamId = 0;

class AuTestGateEffect : public SingleComponentEffect
{
public:
    static FUnknown* createInstance(void*)
    {
        return static_cast<IAudioProcessor*>(new AuTestGateEffect());
    }

    tresult PLUGIN_API initialize(FUnknown* context) override
    {
        const tresult result = SingleComponentEffect::initialize(context);
        if (result != kResultOk) {
            return result;
        }
        addAudioInput(STR16("Stereo In"), SpeakerArr::kStereo);
        addAudioOutput(STR16("Stereo Out"), SpeakerArr::kStereo);
        parameters.addParameter(STR16("Effect depth"), nullptr, 0, 1.0,
                                ParameterInfo::kCanAutomate, kDepthParamId);
        return kResultOk;
    }

    // Persist the depth parameter as the component state, so a saved value round-trips
    // with the project. This is what makes the plugin useful for checking that a
    // late-loaded effect keeps its saved settings.
    tresult PLUGIN_API setState(IBStream* state) override
    {
        if (!state) {
            return kResultFalse;
        }
        IBStreamer streamer(state, kLittleEndian);
        float depth = 1.f;
        if (!streamer.readFloat(depth)) {
            return kResultFalse;
        }
        m_depth = depth;
        setParamNormalized(kDepthParamId, depth);
        return kResultOk;
    }

    tresult PLUGIN_API getState(IBStream* state) override
    {
        if (!state) {
            return kResultFalse;
        }
        IBStreamer streamer(state, kLittleEndian);
        return streamer.writeFloat(static_cast<float>(m_depth)) ? kResultOk : kResultFalse;
    }

    tresult PLUGIN_API setBusArrangements(SpeakerArrangement* inputs, int32 numIns, SpeakerArrangement* outputs,
                                          int32 numOuts) override
    {
        if (numIns == 1 && numOuts == 1 && inputs[0] == outputs[0]) {
            return SingleComponentEffect::setBusArrangements(inputs, numIns, outputs, numOuts);
        }
        return kResultFalse;
    }

    tresult PLUGIN_API canProcessSampleSize(int32 symbolicSampleSize) override
    {
        return symbolicSampleSize == kSample32 ? kResultTrue : kResultFalse;
    }

    tresult PLUGIN_API setupProcessing(ProcessSetup& setup) override
    {
        m_sampleRate = setup.sampleRate > 0 ? setup.sampleRate : 44100.0;
        m_lfoPhase = 0.0;
        return SingleComponentEffect::setupProcessing(setup);
    }

    // Amplitude tremolo whose depth is the "Effect depth" parameter:
    //   gain = 1 - depth * (0.5 + 0.5*cos(phase))
    // depth=0 is pass-through, depth=1 sweeps the gain across the full 0..1 range at
    // kLfoHz. Deliberately obvious, so the effect being active vs bypassed (and the
    // saved depth value) is clearly audible. Bypass is handled by the host not
    // calling us.
    tresult PLUGIN_API process(ProcessData& data) override
    {
        // Pick up depth-parameter changes; block granularity is plenty for a test.
        if (data.inputParameterChanges) {
            const int32 numParams = data.inputParameterChanges->getParameterCount();
            for (int32 p = 0; p < numParams; ++p) {
                IParamValueQueue* const queue = data.inputParameterChanges->getParameterData(p);
                if (!queue || queue->getParameterId() != kDepthParamId) {
                    continue;
                }
                ParamValue value = 0.0;
                int32 sampleOffset = 0;
                const int32 pointCount = queue->getPointCount();
                if (pointCount > 0 && queue->getPoint(pointCount - 1, sampleOffset, value) == kResultTrue) {
                    m_depth = value;
                }
            }
        }

        if (data.numInputs == 0 || data.numOutputs == 0 || data.numSamples == 0) {
            return kResultOk;
        }
        const AudioBusBuffers& in = data.inputs[0];
        AudioBusBuffers& out = data.outputs[0];
        const int32 channels = std::min(in.numChannels, out.numChannels);

        constexpr double kPi = 3.14159265358979323846;
        const double phaseInc = 2.0 * kPi * kLfoHz / m_sampleRate;

        double phase = m_lfoPhase;
        for (int32 i = 0; i < data.numSamples; ++i) {
            const float gain = static_cast<float>(1.0 - m_depth * (0.5 + 0.5 * std::cos(phase)));
            for (int32 ch = 0; ch < channels; ++ch) {
                out.channelBuffers32[ch][i] = in.channelBuffers32[ch][i] * gain;
            }
            phase += phaseInc;
            if (phase >= 2.0 * kPi) {
                phase -= 2.0 * kPi;
            }
        }
        m_lfoPhase = phase;

        // Modulated output isn't silent even across a gain trough within the block.
        out.silenceFlags = 0;
        return kResultOk;
    }

private:
    static constexpr double kLfoHz = 5.0;
    double m_sampleRate = 44100.0;
    double m_lfoPhase = 0.0;
    double m_depth = 1.0;
};

const FUID kAuTestGateUID(0x7A3E9C41, 0x2B6D4F58, 0x9E1C3A7F, 0x5D2B8E64);
}

BEGIN_FACTORY_DEF("Audacity Test", "https://www.audacityteam.org", "mailto:noreply@audacityteam.org")
DEF_CLASS2(INLINE_UID_FROM_FUID(kAuTestGateUID),
           PClassInfo::kManyInstances,
           kVstAudioEffectClass,
           "Audacity test VST3 plugin",
           Vst::kDistributable,
           Vst::PlugType::kFx,
           "1.0.0",
           kVstVersionString,
           AuTestGateEffect::createInstance)
END_FACTORY
