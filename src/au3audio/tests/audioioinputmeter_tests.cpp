/*
 * Audacity: A Digital Audio Editor
 */
#include <chrono>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "au3-audio-io/AudioIO.h"
#include "au3-audio-io/internal/AudioIOInputChannelSelection.h"
#include "au3-mixer/AudioIOSequences.h"

using ::testing::_;
using ::testing::NiceMock;

namespace au::au3audio {
namespace details = audacity::audio_io::details;

namespace {
class TestAudioIoCallback : public AudioIoCallback
{
public:
    void StopStream() override {}
    void StopMonitoring() override {}
};

class MeterSenderMock : public IMeterSender
{
public:
    MOCK_METHOD(void, push, (uint8_t, const InterleavedSampleData&, const std::optional<TrackId>&), (override));
    void start(double) override {}
    void stop() override {}
};

class RecordableSequenceStub final : public RecordableSequence
{
public:
    RecordableSequenceStub(size_t channels, int64_t id)
        : m_channels(channels), m_id(id) {}

    sampleFormat GetSampleFormat() const override { return floatSample; }
    double GetRate() const override { return 44100.0; }
    size_t NChannels() const override { return m_channels; }
    int64_t GetRecordableSequenceId() const override { return m_id; }
    bool Append(size_t, constSamplePtr, sampleFormat, size_t, unsigned int, sampleFormat) override { return false; }
    void Flush() override {}
    void RepairChannels() override {}
    void InsertSilence(double, double) override {}

private:
    size_t m_channels;
    int64_t m_id;
};

struct MeterSubmission {
    uint8_t channel;
    size_t stride;
    TimePoint dacTime;
    std::optional<IMeterSender::TrackId> trackId;
    std::vector<float> samples;
};
}

class AudioIOInputMeterTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        ON_CALL(*m_sender, push(_, _, _))
        .WillByDefault([this](uint8_t channel, const IMeterSender::InterleavedSampleData& data,
                              const std::optional<IMeterSender::TrackId>& trackId) {
            MeterSubmission submission { channel, data.nChannels, data.dacTime, trackId, {} };
            // The callback may submit temporary buffers, so copy samples during push().
            for (size_t frame = 0; frame < data.frames; ++frame) {
                submission.samples.push_back(data.buffer[frame * data.nChannels]);
            }
            m_submissions.push_back(std::move(submission));
        });
    }

    void setInputChannelCount(size_t channels)
    {
        m_callback.mNumCaptureChannels = channels;
        m_callback.mInputChannelSelection = details::LegacyInputChannelSelection(channels);
    }

    void setTracks(const std::vector<size_t>& channelCounts, const std::vector<std::vector<size_t> >& sourceMap)
    {
        m_callback.mCaptureSequences.clear();
        for (size_t track = 0; track < channelCounts.size(); ++track) {
            m_callback.mCaptureSequences.push_back(std::make_shared<RecordableSequenceStub>(channelCounts[track], 100 + track));
        }
        m_callback.mTrackChannelSourceMap = sourceMap;
    }

    void pushInput(const std::vector<float>& input)
    {
        ASSERT_GT(m_callback.mNumCaptureChannels, 0u);
        ASSERT_EQ(input.size() % m_callback.mNumCaptureChannels, 0u);
        m_submissions.clear();
        m_callback.PushInputMeterValues(m_sender, input.data(), input.size() / m_callback.mNumCaptureChannels, m_dacTime);
    }

    void expectMainMeter(const std::vector<std::vector<float> >& expected, size_t stride)
    {
        expectMeter(expected, stride, std::nullopt);
    }

    void expectMeter(const std::vector<std::vector<float> >& expected, size_t stride, std::optional<int64_t> trackId)
    {
        std::vector<const MeterSubmission*> submissions;
        for (const auto& submission : m_submissions) {
            const auto id = submission.trackId ? std::make_optional(submission.trackId->value) : std::nullopt;
            if (id == trackId) {
                submissions.push_back(&submission);
            }
        }
        ASSERT_EQ(submissions.size(), expected.size());
        for (size_t channel = 0; channel < expected.size(); ++channel) {
            SCOPED_TRACE(channel);
            const auto& submission = *submissions[channel];
            EXPECT_EQ(submission.channel, channel);
            EXPECT_EQ(submission.stride, stride);
            EXPECT_EQ(submission.dacTime, m_dacTime);
            ASSERT_EQ(submission.samples.size(), expected[channel].size());
            for (size_t frame = 0; frame < expected[channel].size(); ++frame) {
                SCOPED_TRACE(frame);
                EXPECT_FLOAT_EQ(submission.samples[frame], expected[channel][frame]);
            }
        }
    }

    TestAudioIoCallback m_callback;
    std::shared_ptr<NiceMock<MeterSenderMock> > m_sender = std::make_shared<NiceMock<MeterSenderMock> >();
    const TimePoint m_dacTime { std::chrono::milliseconds(123) };
    std::vector<MeterSubmission> m_submissions;
};

TEST_F(AudioIOInputMeterTests, PassesUnclampedSamplesToTheMeter)
{
    setInputChannelCount(2);

    ASSERT_NO_FATAL_FAILURE(pushInput({ 1.5f, 0.0f, -1.5f, 0.5f }));

    expectMainMeter({ { 1.5f, -1.5f }, { 0.0f, 0.5f } }, 2);
}

TEST_F(AudioIOInputMeterTests, ZeroFramesProduceNoUpdates)
{
    setInputChannelCount(2);
    const float input[] { 0.8f, 0.99f, 0.0f };
    EXPECT_CALL(*m_sender, push(_, _, _)).Times(0);

    m_callback.PushInputMeterValues(m_sender, input, 0, m_dacTime);
}

TEST_F(AudioIOInputMeterTests, EmptySelectionProducesNoUpdates)
{
    setInputChannelCount(0);
    const float input[] { 0.8f };
    EXPECT_CALL(*m_sender, push(_, _, _)).Times(0);

    m_callback.PushInputMeterValues(m_sender, input, 1, m_dacTime);
}

TEST_F(AudioIOInputMeterTests, LegacyThreeInputPresetMetersBeforeRecordingWithoutPolarityCancellation)
{
    setInputChannelCount(3);

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.75f, -0.25f, -0.75f, -0.6f, -0.4f, -0.2f, 0.2f, 0.9f, 0.4f }));

    expectMainMeter({ { 0.75f, 0.6f, 0.4f }, { 0.25f, 0.4f, 0.9f } }, 1);
}

TEST_F(AudioIOInputMeterTests, LegacyFourInputPresetUsesMaximumMagnitudeInEachBar)
{
    setInputChannelCount(4);

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.2f, -0.8f, -0.7f, -0.25f, -0.5f, -0.75f, -0.1f }));

    expectMainMeter({ { 0.8f, 0.75f }, { 0.7f, 0.5f } }, 1);
}

TEST_F(AudioIOInputMeterTests, MultichannelSummaryDoesNotClampSamples)
{
    setInputChannelCount(3);

    ASSERT_NO_FATAL_FAILURE(pushInput({ 1.5f, -1.25f, -1.75f }));

    expectMainMeter({ { 1.75f }, { 1.25f } }, 1);
}

TEST_F(AudioIOInputMeterTests, MainMeterDoesNotDependOnRecordingDestinationCount)
{
    setInputChannelCount(4);
    const std::vector<float> input { 0.8f, 0.2f, -0.8f, -0.7f, -0.25f, -0.5f, -0.75f, -0.1f };
    ASSERT_NO_FATAL_FAILURE(pushInput(input));
    expectMainMeter({ { 0.8f, 0.75f }, { 0.7f, 0.5f } }, 1);

    setTracks({ 1, 1 }, { { 0 }, { 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput(input));

    ASSERT_EQ(m_submissions.size(), 4u);
    expectMainMeter({ { 0.8f, 0.75f }, { 0.7f, 0.5f } }, 1);
    expectMeter({ { 0.8f, -0.25f } }, 4, 100);
    expectMeter({ { 0.2f, -0.5f } }, 4, 101);
}

TEST_F(AudioIOInputMeterTests, SeparateMonoTracksReceiveIndependentSelectedInputs)
{
    setInputChannelCount(2);
    setTracks({ 1, 1 }, { { 0 }, { 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.2f, -0.6f, 0.4f }));

    ASSERT_EQ(m_submissions.size(), 4u);
    expectMainMeter({ { 0.8f, -0.6f }, { 0.2f, 0.4f } }, 2);
    expectMeter({ { 0.8f, -0.6f } }, 2, 100);
    expectMeter({ { 0.2f, 0.4f } }, 2, 101);

}

TEST_F(AudioIOInputMeterTests, MonoTrackMetersTheRecordedStereoDownmix)
{
    setInputChannelCount(2);
    setTracks({ 1 }, { { 0, 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, -0.8f, -0.4f, 0.2f }));

    ASSERT_EQ(m_submissions.size(), 3u);
    expectMainMeter({ { 0.8f, -0.4f }, { -0.8f, 0.2f } }, 2);
    expectMeter({ { 0.0f, -0.1f } }, 1, 100);
}

TEST_F(AudioIOInputMeterTests, StereoTrackDuplicatesTheSelectedMonoInput)
{
    setInputChannelCount(1);
    setTracks({ 2 }, { { 0 }, { 0 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.4f, -0.6f }));

    ASSERT_EQ(m_submissions.size(), 3u);
    expectMainMeter({ { 0.4f, -0.6f } }, 1);
    expectMeter({ { 0.4f, -0.6f }, { 0.4f, -0.6f } }, 1, 100);
}

TEST_F(AudioIOInputMeterTests, MultichannelMeterLevelsDoNotChangeCancellingSoftwarePlaythrough)
{
    setInputChannelCount(3);
    const std::vector<float> input { 0.8f, -0.8f, 0.0f, -0.6f, 0.3f, 0.3f };
    ASSERT_NO_FATAL_FAILURE(pushInput(input));
    expectMainMeter({ { 0.8f, 0.6f }, { 0.8f, 0.3f } }, 1);

    m_callback.mSoftwarePlaythrough = true;
    m_callback.mNumPlaybackChannels = 2;
    float output[4] {};
    float outputMeter[4] {};
    m_callback.DoPlaythrough(input.data(), output, 2, outputMeter);

    for (size_t sample = 0; sample < 4; ++sample) {
        EXPECT_FLOAT_EQ(output[sample], 0.0f);
        EXPECT_FLOAT_EQ(outputMeter[sample], 0.0f);
    }
}
}
