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

    void setSelection(const details::InputChannelSelection& selection)
    {
        m_callback.mInputChannelSelection = selection;
        m_callback.mInputChannelIndices = details::FlattenInputChannelSelection(selection);
        m_callback.mNumCaptureChannels = m_callback.mInputChannelIndices.size();
        m_callback.mNumInputStreamChannels = details::InputChannelSelectionStreamWidth(selection);
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
        ASSERT_GT(m_callback.mNumInputStreamChannels, 0u);
        ASSERT_EQ(input.size() % m_callback.mNumInputStreamChannels, 0u);
        m_submissions.clear();
        m_callback.PushInputMeterValues(m_sender, input.data(), input.size() / m_callback.mNumInputStreamChannels, m_dacTime);
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

struct InputMeterCase {
    const char* name;
    details::InputChannelSelection selection;
    std::vector<float> input;
    std::vector<std::vector<float> > expected;
};

class AudioIOInputMeterRoutesTests : public AudioIOInputMeterTests, public ::testing::WithParamInterface<InputMeterCase>
{
};

TEST_P(AudioIOInputMeterRoutesTests, PreservesEachSelectedInputIndependently)
{
    const auto& testCase = GetParam();
    setSelection(testCase.selection);

    ASSERT_NO_FATAL_FAILURE(pushInput(testCase.input));

    expectMainMeter(testCase.expected, m_callback.mNumInputStreamChannels);
}

INSTANTIATE_TEST_SUITE_P(
    SelectedInputs,
    AudioIOInputMeterRoutesTests,
    ::testing::Values(
        InputMeterCase { "NonAdjacentMonos", { { 0 }, { 2 } },
                         { 0.8f, 0.99f, 0.0f, -0.4f, -0.99f, 0.2f, 0.6f, 0.99f, -0.6f },
                         { { 0.8f, -0.4f, 0.6f }, { 0.0f, 0.2f, -0.6f } } },
        InputMeterCase { "AdjacentMonos", { { 0 }, { 1 } },
                         { 0.8f, 0.0f, -0.4f, 0.2f, 0.6f, -0.6f },
                         { { 0.8f, -0.4f, 0.6f }, { 0.0f, 0.2f, -0.6f } } },
        InputMeterCase { "FirstStereoPair", { { 0, 1 } },
                         { 0.8f, 0.0f, -0.4f, 0.2f, 0.6f, -0.6f },
                         { { 0.8f, -0.4f, 0.6f }, { 0.0f, 0.2f, -0.6f } } },
        InputMeterCase { "HigherStereoPair", { { 2, 3 } },
                         { 0.99f, -0.99f, 0.8f, 0.0f, -0.99f, 0.99f, -0.4f, 0.2f, 0.99f, -0.99f, 0.6f, -0.6f },
                         { { 0.8f, -0.4f, 0.6f }, { 0.0f, 0.2f, -0.6f } } },
        InputMeterCase { "FirstMono", { { 0 } },
                         { 0.4f, -0.5f, 0.6f }, { { 0.4f, -0.5f, 0.6f } } },
        InputMeterCase { "HigherMono", { { 3 } },
                         { 0.99f, -0.99f, 0.75f, 0.4f, -0.99f, 0.99f, 0.2f, -0.5f, 0.99f, -0.99f, 0.3f, 0.6f },
                         { { 0.4f, -0.5f, 0.6f } } }),
    [](const ::testing::TestParamInfo<InputMeterCase>& info) {
    return info.param.name;
});

TEST_F(AudioIOInputMeterTests, PassesUnclampedSamplesToTheMeter)
{
    setSelection({ { 0, 1 } });

    ASSERT_NO_FATAL_FAILURE(pushInput({ 1.5f, 0.0f, -1.5f, 0.5f }));

    expectMainMeter({ { 1.5f, -1.5f }, { 0.0f, 0.5f } }, 2);
}

TEST_F(AudioIOInputMeterTests, ZeroFramesProduceNoUpdates)
{
    setSelection({ { 0 }, { 2 } });
    const float input[] { 0.8f, 0.99f, 0.0f };
    EXPECT_CALL(*m_sender, push(_, _, _)).Times(0);

    m_callback.PushInputMeterValues(m_sender, input, 0, m_dacTime);
}

TEST_F(AudioIOInputMeterTests, EmptySelectionProducesNoUpdates)
{
    setSelection({});
    const float input[] { 0.8f };
    EXPECT_CALL(*m_sender, push(_, _, _)).Times(0);

    m_callback.PushInputMeterValues(m_sender, input, 1, m_dacTime);
}

TEST_F(AudioIOInputMeterTests, ThreeMonoInputsUseSelectedSampleMagnitudes)
{
    setSelection({ { 0 }, { 2 }, { 4 } });

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.75f, 1.0f, -0.375f, -1.0f, 0.375f,
                                        -0.75f, 1.0f, 0.375f, -1.0f, -0.375f }));

    expectMainMeter({ { 0.75f, 0.75f }, { 0.375f, 0.375f } }, 1);
}

TEST_F(AudioIOInputMeterTests, StereoGroupAfterMonoKeepsItsLeftAndRightChannels)
{
    setSelection({ { 0 }, { 2, 3 } });

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.9f, 0.2f, -0.4f, 0.8f, -0.6f, 0.1f, 0.4f, -0.2f }));

    expectMainMeter({ { 0.9f, 0.6f }, { 0.8f, 0.2f } }, 1);
}

TEST_F(AudioIOInputMeterTests, LegacyThreeInputPresetMetersBeforeRecordingWithoutPolarityCancellation)
{
    setSelection(details::LegacyInputChannelSelection(3));

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.75f, -0.25f, -0.75f, -0.6f, -0.4f, -0.2f, 0.2f, 0.9f, 0.4f }));

    expectMainMeter({ { 0.75f, 0.6f, 0.4f }, { 0.25f, 0.4f, 0.9f } }, 1);
}

TEST_F(AudioIOInputMeterTests, LegacyFourInputPresetUsesMaximumMagnitudeInEachBar)
{
    setSelection(details::LegacyInputChannelSelection(4));

    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.2f, -0.8f, -0.7f, -0.25f, -0.5f, -0.75f, -0.1f }));

    expectMainMeter({ { 0.8f, 0.75f }, { 0.7f, 0.5f } }, 1);
}

TEST_F(AudioIOInputMeterTests, MixedGroupsKeepStereoSidesAndAlternateMonosByGroupIndex)
{
    setSelection({ { 0, 1 }, { 2 }, { 4 }, { 6, 7 } });

    ASSERT_NO_FATAL_FAILURE(pushInput({ -0.2f, 0.1f, -0.7f, 0.99f, -0.8f, -0.99f, 0.4f, -0.3f,
                                        0.9f, -0.8f, 0.1f, -0.99f, 0.2f, 0.99f, -0.3f, 0.4f }));

    expectMainMeter({ { 0.8f, 0.9f }, { 0.7f, 0.8f } }, 1);
}

TEST_F(AudioIOInputMeterTests, MultichannelSummaryDoesNotClampSamples)
{
    setSelection(details::LegacyInputChannelSelection(3));

    ASSERT_NO_FATAL_FAILURE(pushInput({ 1.5f, -1.25f, -1.75f }));

    expectMainMeter({ { 1.75f }, { 1.25f } }, 1);
}

TEST_F(AudioIOInputMeterTests, MainMeterDoesNotDependOnRecordingDestinationCount)
{
    setSelection(details::LegacyInputChannelSelection(4));
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
    setSelection(details::LegacyInputChannelSelection(2));
    setTracks({ 1, 1 }, { { 0 }, { 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.2f, -0.6f, 0.4f }));

    ASSERT_EQ(m_submissions.size(), 4u);
    expectMainMeter({ { 0.8f, -0.6f }, { 0.2f, 0.4f } }, 2);
    expectMeter({ { 0.8f, -0.6f } }, 2, 100);
    expectMeter({ { 0.2f, 0.4f } }, 2, 101);

    setSelection({ { 0 }, { 2 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.99f, 0.2f, -0.6f, -0.99f, 0.4f }));

    ASSERT_EQ(m_submissions.size(), 4u);
    expectMainMeter({ { 0.8f, -0.6f }, { 0.2f, 0.4f } }, 3);
    expectMeter({ { 0.8f, -0.6f } }, 3, 100);
    expectMeter({ { 0.2f, 0.4f } }, 3, 101);
}

TEST_F(AudioIOInputMeterTests, StereoTrackReceivesNonAdjacentInputsIndependently)
{
    setSelection({ { 0 }, { 2 } });
    setTracks({ 2 }, { { 0 }, { 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, 0.99f, 0.2f, -0.6f, -0.99f, 0.4f }));

    ASSERT_EQ(m_submissions.size(), 4u);
    expectMainMeter({ { 0.8f, -0.6f }, { 0.2f, 0.4f } }, 3);
    expectMeter({ { 0.8f, -0.6f }, { 0.2f, 0.4f } }, 3, 100);
}

TEST_F(AudioIOInputMeterTests, MonoTrackMetersTheRecordedStereoDownmix)
{
    setSelection(details::LegacyInputChannelSelection(2));
    setTracks({ 1 }, { { 0, 1 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.8f, -0.8f, -0.4f, 0.2f }));

    ASSERT_EQ(m_submissions.size(), 3u);
    expectMainMeter({ { 0.8f, -0.4f }, { -0.8f, 0.2f } }, 2);
    expectMeter({ { 0.0f, -0.1f } }, 1, 100);
}

TEST_F(AudioIOInputMeterTests, StereoTrackDuplicatesTheSelectedMonoInput)
{
    setSelection({ { 3 } });
    setTracks({ 2 }, { { 0 }, { 0 } });
    ASSERT_NO_FATAL_FAILURE(pushInput({ 0.99f, -0.99f, 0.75f, 0.4f, -0.99f, 0.99f, 0.2f, -0.6f }));

    ASSERT_EQ(m_submissions.size(), 3u);
    expectMainMeter({ { 0.4f, -0.6f } }, 4);
    expectMeter({ { 0.4f, -0.6f }, { 0.4f, -0.6f } }, 4, 100);
}

TEST_F(AudioIOInputMeterTests, MultichannelMeterLevelsDoNotChangeCancellingSoftwarePlaythrough)
{
    setSelection(details::LegacyInputChannelSelection(3));
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

TEST_F(AudioIOInputMeterTests, IndependentInputMetersDoNotChangeSoftwarePlaythrough)
{
    setSelection({ { 0 }, { 2 } });
    const std::vector<float> input { 0.8f, 0.99f, 0.0f, -0.6f, -0.99f, 0.2f };
    ASSERT_NO_FATAL_FAILURE(pushInput(input));
    expectMainMeter({ { 0.8f, -0.6f }, { 0.0f, 0.2f } }, 3);

    m_callback.mSoftwarePlaythrough = true;
    m_callback.mNumPlaybackChannels = 2;
    float output[4] {};
    float outputMeter[4] {};
    m_callback.DoPlaythrough(input.data(), output, 2, outputMeter);

    const float expected[] { 0.4f, 0.4f, -0.2f, -0.2f };
    for (size_t sample = 0; sample < 4; ++sample) {
        EXPECT_FLOAT_EQ(output[sample], expected[sample]);
        EXPECT_FLOAT_EQ(outputMeter[sample], expected[sample]);
    }
}
}
