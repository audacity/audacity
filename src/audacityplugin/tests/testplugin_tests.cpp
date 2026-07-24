/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "internal/loadedplugin.h"
#include "internal/manifestreader.h"
#include "internal/offlineeffectadapter.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace au::audacityplugin {
namespace {
Platform testPlatform()
{
#if defined(_WIN32) && (defined(_M_ARM64) || defined(__aarch64__))
    return Platform::WindowsArm64;
#elif defined(_WIN32)
    return Platform::WindowsX64;
#elif defined(__APPLE__) && defined(__aarch64__)
    return Platform::MacArm64;
#elif defined(__APPLE__)
    return Platform::MacX64;
#elif defined(__linux__) && defined(__aarch64__)
    return Platform::LinuxArm64;
#else
    return Platform::LinuxX64;
#endif
}

class LoadedTestPlugin
{
public:
    explicit LoadedTestPlugin(const std::string& testName)
    {
        try {
            const auto bundle = std::filesystem::canonical(
                std::filesystem::u8path(AUP_TEST_PLUGIN_BUNDLE_PATH));
            std::ifstream input(bundle / "plugin.json", std::ios::binary);
            if (!input) {
                ADD_FAILURE() << "Could not read the test plugin manifest";
                return;
            }

            const std::string json(
                (std::istreambuf_iterator<char>(input)),
                std::istreambuf_iterator<char>());
            auto parsed = readManifest(json, testPlatform());
            auto* manifest = std::get_if<Manifest>(&parsed);
            if (!manifest) {
                ADD_FAILURE() << std::get<ManifestError>(parsed).message;
                return;
            }

            m_dataPath = std::filesystem::temp_directory_path()
                         / ("audacityplugin-v0-test-" + testName);
            std::error_code ignored;
            std::filesystem::remove_all(m_dataPath, ignored);

            std::string error;
            plugin = LoadedPlugin::load(
                std::move(*manifest), bundle, m_dataPath, error);
            if (!plugin) {
                ADD_FAILURE() << error;
            }
        } catch (const std::exception& exception) {
            ADD_FAILURE() << exception.what();
        }
    }

    ~LoadedTestPlugin()
    {
        plugin.reset();
        std::error_code ignored;
        std::filesystem::remove_all(m_dataPath, ignored);
    }

    std::shared_ptr<LoadedPlugin> plugin;

private:
    std::filesystem::path m_dataPath;
};

struct CapturedOutput {
    AudioFormat format;
    std::vector<std::vector<float> > channels;
};

class StreamingHost final : public IOfflineHost
{
public:
    static constexpr uint64_t SampleCount = 3000u;
    static constexpr uint32_t SampleRate = 48000u;

    explicit StreamingHost(bool cancelOnProgress = false)
        : m_cancelOnProgress(cancelOnProgress)
        , m_source(2u, std::vector<float>(SampleCount))
    {
        for (uint64_t sample = 0; sample < SampleCount; ++sample) {
            const auto value = static_cast<float>(
                static_cast<int>(sample % 13u) - 6) / 6.0f;
            m_source[0][sample] = value;
            m_source[1][sample] = -0.5f * value;
        }
    }

    OfflineArgs args() const
    {
        OfflineArgs result;
        result.selectionDurationSeconds
            = static_cast<double>(SampleCount) / SampleRate;
        result.audioTracks.push_back({
                    "Test input",
                    1u,
                    1u,
                    { { 2u, SampleRate }, SampleCount },
                });
        return result;
    }

    OfflineArgs labelArgs() const
    {
        OfflineArgs result;
        result.selectionDurationSeconds = 5.0;
        result.labelTracks.push_back({
                    "Original",
                    7u,
                    {
                        { 11u, 0.0, 1.0, "Bonjour" },
                        { 12u, 2.0, 3.0, "Au revoir" },
                    },
                });
        return result;
    }

    float sourceSample(uint32_t channel, uint64_t sample) const
    {
        return m_source[channel][sample];
    }

    Status convertAudio(AudioHandle, const AudioFormat&,
                        AudioHandle& converted, AudioInfo& info) override
    {
        converted = 0;
        info = {};
        return Status::InvalidArgument;
    }

    Status readAudio(AudioHandle audio, uint64_t sampleOffset,
                     uint64_t requestedSamples, AudioChunk& chunk) override
    {
        chunk = {};
        if (audio != 1u || requestedSamples == 0u
            || sampleOffset > SampleCount) {
            return Status::InvalidArgument;
        }
        if (sampleOffset == SampleCount) {
            return Status::Ok;
        }

        constexpr uint64_t HostBlockSize = 137u;
        const auto count = std::min(
            { requestedSamples, HostBlockSize, SampleCount - sampleOffset });
        chunk.channelCount = 2u;
        chunk.sampleCount = count;
        chunk.channels[0] = m_source[0].data() + sampleOffset;
        chunk.channels[1] = m_source[1].data() + sampleOffset;
        maximumReadRequest = std::max(maximumReadRequest, requestedSamples);
        ++readCalls;
        return Status::Ok;
    }

    Status createAudio(const AudioFormat& format, AudioHandle& audio) override
    {
        if (m_active) {
            return Status::InvalidState;
        }
        m_active = CapturedOutput {
            format,
            std::vector<std::vector<float> >(format.channelCount),
        };
        audio = 2u;
        return Status::Ok;
    }

    Status writeAudio(AudioHandle audio, const AudioChunk& chunk) override
    {
        if (audio != 2u || !m_active
            || chunk.channelCount != m_active->channels.size()) {
            return Status::InvalidArgument;
        }
        if (chunk.sampleCount == 0u) {
            ++emptyWriteCalls;
            return Status::Ok;
        }
        for (uint32_t channel = 0; channel < chunk.channelCount; ++channel) {
            m_active->channels[channel].insert(
                m_active->channels[channel].end(),
                chunk.channels[channel],
                chunk.channels[channel] + chunk.sampleCount);
        }
        ++writeCalls;
        return Status::Ok;
    }

    Status replaceAudioTrack(AudioHandle audio, TrackHandle track) override
    {
        if (audio != 2u || track != 1u || !m_active) {
            return Status::InvalidArgument;
        }
        replacement = std::move(*m_active);
        m_active.reset();
        replacementTrack = track;
        return Status::Ok;
    }

    Status addAudioTrack(AudioHandle, const std::string&) override
    {
        return Status::InvalidArgument;
    }

    Status releaseAudio(AudioHandle audio) override
    {
        if (audio != 2u || !m_active) {
            return Status::InvalidArgument;
        }
        m_active.reset();
        return Status::Ok;
    }

    Status createLabelTrack(const std::string& name, TrackHandle& track) override
    {
        createdLabelTrackName = name;
        track = 8u;
        return Status::Ok;
    }

    Status addLabel(TrackHandle track, double start, double end,
                    const std::string& text, LabelHandle& label) override
    {
        label = ++m_nextLabel;
        addedLabels.push_back({ track, { label, start, end, text } });
        return Status::Ok;
    }

    Status updateLabel(LabelHandle label, double start, double end,
                       const std::string& text) override
    {
        updatedLabels.push_back({ label, start, end, text });
        return Status::Ok;
    }

    Status deleteLabel(LabelHandle label) override
    {
        deletedLabels.push_back(label);
        return Status::Ok;
    }

    bool progress(double fraction, const std::string&) override
    {
        lastProgress = fraction;
        ++progressCalls;
        if (m_cancelOnProgress) {
            m_cancelled = true;
            return false;
        }
        return true;
    }

    bool cancelled() override
    {
        ++cancellationChecks;
        return m_cancelled;
    }

    Status finish() noexcept override
    {
        ++finishCalls;
        m_active.reset();
        return Status::Ok;
    }

    uint64_t maximumReadRequest = 0u;
    uint64_t readCalls = 0u;
    uint64_t writeCalls = 0u;
    uint64_t emptyWriteCalls = 0u;
    uint64_t progressCalls = 0u;
    uint64_t cancellationChecks = 0u;
    uint64_t finishCalls = 0u;
    uint64_t replacementTrack = 0u;
    double lastProgress = 0.0;
    std::optional<CapturedOutput> replacement;
    std::string createdLabelTrackName;
    std::vector<std::pair<TrackHandle, Label> > addedLabels;
    std::vector<Label> updatedLabels;
    std::vector<LabelHandle> deletedLabels;

private:
    bool m_cancelOnProgress = false;
    bool m_cancelled = false;
    std::vector<std::vector<float> > m_source;
    std::optional<CapturedOutput> m_active;
    LabelHandle m_nextLabel = 100u;
};

aup_status AUP_CALL translateLabels(
    void*, const aup_effect_offline_args_v0* args,
    const aup_effect_offline_host_v0* host)
{
    const auto& track = args->label_tracks[0];
    const auto& first = track.labels[0];
    const auto& second = track.labels[1];
    aup_label added = AUP_INVALID_LABEL;
    aup_track translatedTrack = AUP_INVALID_TRACK;

    auto status = host->update_label(
        host->context, first.label, first.start_seconds,
        first.end_seconds, AUP_STR("Hello"));
    if (status != AUP_OK) {
        return status;
    }
    status = host->add_label(
        host->context, track.track, first.start_seconds,
        first.end_seconds, AUP_STR("Bonjour / Hello"), nullptr);
    if (status != AUP_OK) {
        return status;
    }
    status = host->delete_label(host->context, second.label);
    if (status != AUP_OK) {
        return status;
    }
    status = host->create_label_track(
        host->context, AUP_STR("English"), &translatedTrack);
    if (status != AUP_OK) {
        return status;
    }
    return host->add_label(
        host->context, translatedTrack, first.start_seconds,
        first.end_seconds, AUP_STR("Hello"), &added);
}

std::shared_ptr<IEffectInstance> createGain(LoadedPlugin& plugin)
{
    auto created = plugin.createInstance("gain", { 0.25, 48000u });
    EXPECT_EQ(created.status, Status::Ok);
    EXPECT_TRUE(created.instance);
    return std::move(created.instance);
}

TEST(TestPlugin, DiscoversGainAndItsParameter)
{
    LoadedTestPlugin testPlugin("discovery");
    ASSERT_TRUE(testPlugin.plugin);

    ASSERT_EQ(testPlugin.plugin->effects().size(), 1u);
    const auto& descriptor = testPlugin.plugin->effects().front();
    EXPECT_EQ(descriptor.effectId, "gain");
    EXPECT_EQ(descriptor.group, PresentationGroup::Effect);
    EXPECT_EQ(descriptor.inputTrackTypes, InputTrackAudio);

    const auto instance = createGain(*testPlugin.plugin);
    ASSERT_TRUE(instance);
    ASSERT_EQ(instance->parameters().size(), 1u);
    const auto& parameter = instance->parameters().front();
    EXPECT_EQ(parameter.key, "gain");
    EXPECT_EQ(parameter.type, ParameterType::Double);
    EXPECT_DOUBLE_EQ(std::get<double>(parameter.defaultValue), 1.0);
    EXPECT_DOUBLE_EQ(std::get<double>(*parameter.minimum), 0.0);
    EXPECT_DOUBLE_EQ(std::get<double>(*parameter.maximum), 4.0);
    EXPECT_EQ(instance->setValue(0u, Value { 2.0 }), Status::Ok);
    EXPECT_DOUBLE_EQ(std::get<double>(instance->value(0u)), 2.0);
    EXPECT_EQ(instance->setValue(0u, Value { 5.0 }), Status::ValidationFailed);
    EXPECT_EQ(instance->validate(), Status::Ok);
}

TEST(TestPlugin, StreamsGainIntoAReplacement)
{
    LoadedTestPlugin testPlugin("streaming");
    ASSERT_TRUE(testPlugin.plugin);
    const auto instance = createGain(*testPlugin.plugin);
    ASSERT_TRUE(instance);
    ASSERT_EQ(instance->setValue(0u, Value { 2.0 }), Status::Ok);

    StreamingHost host;
    EXPECT_EQ(instance->apply(host.args(), host), Status::Ok);
    ASSERT_TRUE(host.replacement);
    EXPECT_EQ(host.replacementTrack, 1u);
    EXPECT_EQ(host.replacement->format.channelCount, 2u);
    EXPECT_EQ(host.replacement->format.sampleRate, StreamingHost::SampleRate);
    EXPECT_GT(host.readCalls, 1u);
    EXPECT_EQ(host.writeCalls, host.readCalls);
    EXPECT_EQ(host.emptyWriteCalls, 1u);
    EXPECT_LE(host.maximumReadRequest, 1024u);
    EXPECT_GT(host.progressCalls, 1u);
    EXPECT_DOUBLE_EQ(host.lastProgress, 1.0);
    EXPECT_EQ(host.finishCalls, 1u);

    for (uint32_t channel = 0; channel < 2u; ++channel) {
        ASSERT_EQ(host.replacement->channels[channel].size(),
                  StreamingHost::SampleCount);
        for (const auto sample : { 0u, 136u, 137u, 1024u, 2999u }) {
            EXPECT_FLOAT_EQ(host.replacement->channels[channel][sample],
                            2.0f * host.sourceSample(channel, sample));
        }
    }
}

TEST(TestPlugin, CancelsAndDiscardsUnfinishedOutput)
{
    LoadedTestPlugin testPlugin("cancellation");
    ASSERT_TRUE(testPlugin.plugin);
    const auto instance = createGain(*testPlugin.plugin);
    ASSERT_TRUE(instance);

    StreamingHost host(true);
    EXPECT_EQ(instance->apply(host.args(), host), Status::Cancelled);
    EXPECT_FALSE(host.replacement);
    EXPECT_EQ(host.readCalls, 1u);
    EXPECT_EQ(host.writeCalls, 1u);
    EXPECT_EQ(host.progressCalls, 1u);
    EXPECT_GT(host.cancellationChecks, 0u);
    EXPECT_EQ(host.finishCalls, 1u);
}

TEST(OfflineEffectAdapter, EditsAndCreatesLabelsByHandle)
{
    StreamingHost host;
    const aup_effect_offline_v0 offline { nullptr, translateLabels };

    EXPECT_EQ(
        internal::invokeOfflineEffect(offline, host.labelArgs(), host),
        Status::Ok);
    ASSERT_EQ(host.updatedLabels.size(), 1u);
    EXPECT_EQ(host.updatedLabels[0].label, 11u);
    EXPECT_EQ(host.updatedLabels[0].text, "Hello");
    ASSERT_EQ(host.deletedLabels.size(), 1u);
    EXPECT_EQ(host.deletedLabels[0], 12u);
    ASSERT_EQ(host.addedLabels.size(), 2u);
    EXPECT_EQ(host.addedLabels[0].first, 7u);
    EXPECT_EQ(host.addedLabels[0].second.text, "Bonjour / Hello");
    EXPECT_EQ(host.createdLabelTrackName, "English");
    EXPECT_EQ(host.addedLabels[1].first, 8u);
    EXPECT_EQ(host.addedLabels[1].second.text, "Hello");
}
} // namespace
} // namespace au::audacityplugin
