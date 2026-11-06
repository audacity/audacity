/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <array>
#include <deque>
#include <map>
#include <memory>
#include <vector>

#include "au3-clip-processing/sequence-graph/IPlayheadMapper.h"
#include "au3-mixer/AudioIOSequences.h"

#include "../internal/juce/effectscratch.h"
#include "../internal/juce/effectsnode.h"
#include "../internal/juce/endoftracknode.h"
#include "../internal/juce/gainpannode.h"
#include "../internal/juce/ieffectscontext.h"
#include "../internal/juce/iendoftrackcontext.h"
#include "../internal/juce/imutesolocontext.h"
#include "../internal/juce/metertapnode.h"
#include "../internal/juce/monitoredinput.h"
#include "../internal/juce/mutesolonode.h"
#include "../internal/juce/sequentialmixnode.h"

using namespace au::au4juce;

namespace {
constexpr size_t TEST_MAX_FRAMES = 64;
//! Longer than TEST_MAX_FRAMES, so that reads are split
constexpr size_t TEST_READ_LEN = 3 * TEST_MAX_FRAMES + TEST_MAX_FRAMES / 2;
constexpr int TEST_LATENCY = TEST_MAX_FRAMES + TEST_MAX_FRAMES / 4;
constexpr int64_t TEST_JUMP_INDEX = 10'000;
constexpr float TEST_LEFT_VOLUME = 0.5f;
constexpr float TEST_RIGHT_VOLUME = 0.25f;
constexpr float TEST_LEVEL = 0.75f;
//! Where the playhead reads from index 0
constexpr int64_t TEST_PLAYHEAD_START = 1'000;
constexpr int64_t TEST_PLAY_END = TEST_PLAYHEAD_START + TEST_READ_LEN / 2;
constexpr int64_t TEST_TRACK_STOP = TEST_PLAYHEAD_START + TEST_READ_LEN / 4;

//! Its samples are their stream index, in the first `numChannels` buffers
class RampNode final : public ISequenceProcessingNode
{
public:
    explicit RampNode(size_t numChannels = au::au4juce::numChannels)
        : m_numChannels{numChannels} {}

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        for (size_t ch = 0; ch < m_numChannels; ++ch) {
            for (size_t i = 0; i < len; ++i) {
                buffers[ch][i] = static_cast<float>(start.as_long_long() + static_cast<int64_t>(i));
            }
        }
    }

private:
    const size_t m_numChannels;
};

//! The same level everywhere
class ConstantNode final : public ISequenceProcessingNode
{
public:
    explicit ConstantNode(float level)
        : m_level{level} {}

    void GetSamples(float* const* buffers, sampleCount, size_t len) override
    {
        for (int ch = 0; ch < au::au4juce::numChannels; ++ch) {
            std::fill_n(buffers[ch], len, m_level);
        }
    }

private:
    const float m_level;
};

//! Effects that delay what they're given by their latency, as latent effects do; per group
class DelayingEffects final : public IEffectsContext
{
public:
    explicit DelayingEffects(int latency)
        : m_latency{latency} {}

    int latency(const ChannelGroup*) const override { return m_latency; }

    void process(const ChannelGroup* group, float* const* buffers, EffectScratch&, size_t frames) override
    {
        auto& delays = m_delays[group];
        for (int ch = 0; ch < au::au4juce::numChannels; ++ch) {
            auto& delay = delays[ch];
            if (delay.empty()) {
                delay.assign(static_cast<size_t>(m_latency), 0.f);
            }
            for (size_t i = 0; i < frames; ++i) {
                delay.push_back(buffers[ch][i]);
                buffers[ch][i] = delay.front();
                delay.pop_front();
            }
            m_lastSecondChannel = std::vector<float>(buffers[1], buffers[1] + frames);
        }
    }

    std::vector<float> m_lastSecondChannel;

private:
    const int m_latency;
    std::map<const ChannelGroup*, std::array<std::deque<float>, au::au4juce::numChannels> > m_delays;
};

//! The playhead reads linearly from TEST_PLAYHEAD_START
class LinearMapper final : public IPlayheadMapper
{
public:
    Segment Map(sampleCount index, size_t maxLen) const override { return { index + TEST_PLAYHEAD_START, maxLen }; }
};

class TestEndOfTrackContext final : public IEndOfTrackContext
{
public:
    std::optional<int64_t> playEnd() const override { return end; }
    std::optional<int64_t> end;
};

class TestMuteSoloContext final : public IMuteSoloContext
{
public:
    bool anySoloing() const override { return soloing; }
    bool soloing = false;
};

class TestTrack final : public PlayableSequence
{
public:
    explicit TestTrack(size_t numChannels)
        : m_numChannels{numChannels} {}

    bool DoGet(size_t, size_t, const samplePtr[], sampleFormat, sampleCount, size_t, bool, fillFormat, bool,
               sampleCount*) const override { return true; }
    size_t NChannels() const override { return m_numChannels; }
    float GetChannelVolume(int channel) const override { return channel == 0 ? leftVolume : rightVolume; }
    double GetStartTime() const override { return 0.; }
    double GetEndTime() const override { return 0.; }
    double GetRate() const override { return 44100.; }
    sampleFormat WidestEffectiveFormat() const override { return floatSample; }
    bool HasTrivialEnvelope() const override { return true; }
    AudioGraph::ChannelType GetChannelType() const override { return AudioGraph::MonoChannel; }
    const ChannelGroup* FindChannelGroup() const override { return nullptr; }
    bool GetSolo() const override { return solo; }
    bool GetMute() const override { return mute; }

    bool solo = false;
    bool mute = false;
    float leftVolume = 1.f;
    float rightVolume = 1.f;

private:
    const size_t m_numChannels;
};

struct Stereo {
    explicit Stereo(size_t len)
        : left(len), right(len) {}
    std::vector<float> left;
    std::vector<float> right;
    float* pointers[au::au4juce::numChannels] { left.data(), right.data() };
};

Stereo read(ISequenceProcessingNode& node, int64_t start, size_t len)
{
    Stereo stereo(len);
    node.GetSamples(stereo.pointers, start, len);
    return stereo;
}

std::vector<float> ramp(int64_t start, size_t len)
{
    std::vector<float> values(len);
    for (size_t i = 0; i < len; ++i) {
        values[i] = static_cast<float>(start + static_cast<int64_t>(i));
    }
    return values;
}
}

TEST(EffectsNodeTests, WhatIsReadAtAnIndexIsHeardThere)
{
    DelayingEffects effects { TEST_LATENCY };
    EffectsNode node { std::make_shared<RampNode>(), au::au4juce::numChannels, nullptr, effects, TEST_MAX_FRAMES };

    const auto output = read(node, 0, TEST_READ_LEN);

    EXPECT_EQ(output.left, ramp(0, TEST_READ_LEN));
    EXPECT_EQ(output.right, ramp(0, TEST_READ_LEN));
}

TEST(EffectsNodeTests, PrimesTheEffectsWhereReadingJumps)
{
    DelayingEffects effects { TEST_LATENCY };
    EffectsNode node { std::make_shared<RampNode>(), au::au4juce::numChannels, nullptr, effects, TEST_MAX_FRAMES };
    read(node, 0, TEST_READ_LEN);

    const auto output = read(node, TEST_JUMP_INDEX, TEST_READ_LEN);

    EXPECT_EQ(output.left, ramp(TEST_JUMP_INDEX, TEST_READ_LEN));
}

TEST(EffectsNodeTests, GivesTheEffectsSilenceWhereAMonoInputHasNoChannel)
{
    DelayingEffects effects { 0 };
    EffectsNode node { std::make_shared<RampNode>(1), 1, nullptr, effects, TEST_MAX_FRAMES };
    Stereo stereo(TEST_MAX_FRAMES);
    std::fill(stereo.right.begin(), stereo.right.end(), TEST_LEVEL);

    node.GetSamples(stereo.pointers, 0, TEST_MAX_FRAMES);

    EXPECT_EQ(effects.m_lastSecondChannel, std::vector<float>(TEST_MAX_FRAMES, 0.f));
}

TEST(MuteSoloNodeTests, SilencesAMutedTrack)
{
    const auto track = std::make_shared<TestTrack>(au::au4juce::numChannels);
    TestMuteSoloContext context;
    MuteSoloNode node { std::make_shared<ConstantNode>(TEST_LEVEL), track, context };
    track->mute = true;

    EXPECT_EQ(read(node, 0, TEST_MAX_FRAMES).left, std::vector<float>(TEST_MAX_FRAMES, 0.f));
}

TEST(MuteSoloNodeTests, SilencesATrackWhileAnotherSolos)
{
    const auto track = std::make_shared<TestTrack>(au::au4juce::numChannels);
    TestMuteSoloContext context;
    MuteSoloNode node { std::make_shared<ConstantNode>(TEST_LEVEL), track, context };
    context.soloing = true;

    EXPECT_EQ(read(node, 0, TEST_MAX_FRAMES).right, std::vector<float>(TEST_MAX_FRAMES, 0.f));
}

TEST(MuteSoloNodeTests, PlaysASoloingTrackEvenIfMuted)
{
    const auto track = std::make_shared<TestTrack>(au::au4juce::numChannels);
    TestMuteSoloContext context;
    MuteSoloNode node { std::make_shared<ConstantNode>(TEST_LEVEL), track, context };
    track->mute = true;
    track->solo = true;
    context.soloing = true;

    EXPECT_EQ(read(node, 0, TEST_MAX_FRAMES).left, std::vector<float>(TEST_MAX_FRAMES, TEST_LEVEL));
}

TEST(GainPanNodeTests, PansAMonoTrackToStereo)
{
    const auto track = std::make_shared<TestTrack>(1);
    track->leftVolume = TEST_LEFT_VOLUME;
    track->rightVolume = TEST_RIGHT_VOLUME;
    GainPanNode node { std::make_shared<RampNode>(1), track };

    const auto output = read(node, 0, TEST_MAX_FRAMES);

    for (size_t i = 0; i < TEST_MAX_FRAMES; ++i) {
        EXPECT_FLOAT_EQ(output.left[i], i * TEST_LEFT_VOLUME);
        EXPECT_FLOAT_EQ(output.right[i], i * TEST_RIGHT_VOLUME);
    }
}

TEST(GainPanNodeTests, AppliesEachChannelsVolumeToAStereoTrack)
{
    const auto track = std::make_shared<TestTrack>(au::au4juce::numChannels);
    track->leftVolume = TEST_LEFT_VOLUME;
    track->rightVolume = TEST_RIGHT_VOLUME;
    GainPanNode node { std::make_shared<ConstantNode>(TEST_LEVEL), track };

    const auto output = read(node, 0, TEST_MAX_FRAMES);

    EXPECT_EQ(output.left, std::vector<float>(TEST_MAX_FRAMES, TEST_LEVEL * TEST_LEFT_VOLUME));
    EXPECT_EQ(output.right, std::vector<float>(TEST_MAX_FRAMES, TEST_LEVEL * TEST_RIGHT_VOLUME));
}

TEST(MeterTapNodeTests, PassesItsInputAndKeepsContiguousReads)
{
    MeterTapNode node { std::make_shared<RampNode>(), au::au4juce::numChannels, TEST_READ_LEN };

    const auto first = read(node, 0, TEST_MAX_FRAMES);
    read(node, TEST_MAX_FRAMES, TEST_MAX_FRAMES);

    EXPECT_EQ(first.left, ramp(0, TEST_MAX_FRAMES));
    const auto capture = node.capture();
    EXPECT_EQ(capture.index, 0);
    ASSERT_EQ(capture.frames, 2 * TEST_MAX_FRAMES);
    EXPECT_EQ(std::vector<float>(capture.channels[1], capture.channels[1] + capture.frames), ramp(0, 2 * TEST_MAX_FRAMES));
}

TEST(MeterTapNodeTests, StartsOverWhereReadingJumpsOrOnceReset)
{
    MeterTapNode node { std::make_shared<RampNode>(), au::au4juce::numChannels, TEST_READ_LEN };
    read(node, 0, TEST_MAX_FRAMES);

    read(node, TEST_JUMP_INDEX, TEST_MAX_FRAMES);
    EXPECT_EQ(node.capture().index, TEST_JUMP_INDEX);
    EXPECT_EQ(node.capture().frames, TEST_MAX_FRAMES);

    node.reset();
    read(node, TEST_JUMP_INDEX + static_cast<int64_t>(TEST_MAX_FRAMES), TEST_MAX_FRAMES);
    EXPECT_EQ(node.capture().index, TEST_JUMP_INDEX + static_cast<int64_t>(TEST_MAX_FRAMES));
}

TEST(MeterTapNodeTests, KeepsTheLouderSideOfAPannedMonoTrack)
{
    const auto track = std::make_shared<TestTrack>(1);
    track->leftVolume = TEST_RIGHT_VOLUME;
    track->rightVolume = TEST_LEFT_VOLUME;
    MeterTapNode node { std::make_shared<GainPanNode>(std::make_shared<RampNode>(1), track), 1, TEST_READ_LEN };

    read(node, 0, TEST_MAX_FRAMES);

    const auto capture = node.capture();
    ASSERT_EQ(capture.numChannels, 1u);
    for (size_t i = 0; i < capture.frames; ++i) {
        EXPECT_FLOAT_EQ(capture.channels[0][i], i * TEST_LEFT_VOLUME);
    }
}

TEST(SequentialMixNodeTests, SumsItsTracks)
{
    constexpr float otherLevel = TEST_LEVEL / 3;
    SequentialMixNode node { { std::make_shared<ConstantNode>(TEST_LEVEL), std::make_shared<ConstantNode>(otherLevel) },
                             TEST_MAX_FRAMES };

    const auto output = read(node, 0, TEST_READ_LEN);

    EXPECT_EQ(output.left, std::vector<float>(TEST_READ_LEN, TEST_LEVEL + otherLevel));
    EXPECT_EQ(output.right, std::vector<float>(TEST_READ_LEN, TEST_LEVEL + otherLevel));
}

namespace {
//! The ramp of RampNode, silent from `silentFrom` on
std::vector<float> rampUntil(size_t len, size_t silentFrom)
{
    auto values = ramp(0, len);
    std::fill(values.begin() + static_cast<std::ptrdiff_t>(std::min(silentFrom, len)), values.end(), 0.f);
    return values;
}
}

TEST(EndOfTrackNodeTests, PlaysOnWithoutAnEnd)
{
    TestEndOfTrackContext context;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, ramp(0, TEST_READ_LEN));
}

TEST(EndOfTrackNodeTests, SilencesPastThePlayEnd)
{
    TestEndOfTrackContext context;
    context.end = TEST_PLAY_END;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context };

    const auto output = read(node, 0, TEST_READ_LEN);

    const auto audible = static_cast<size_t>(TEST_PLAY_END - TEST_PLAYHEAD_START);
    EXPECT_EQ(output.left, rampUntil(TEST_READ_LEN, audible));
    EXPECT_EQ(output.right, rampUntil(TEST_READ_LEN, audible));
}

TEST(EndOfTrackNodeTests, SilencesWhereTheTrackStopsBeforeThePlayEnd)
{
    TestEndOfTrackContext context;
    context.end = TEST_PLAY_END;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context, TEST_TRACK_STOP };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, rampUntil(TEST_READ_LEN, static_cast<size_t>(TEST_TRACK_STOP - TEST_PLAYHEAD_START)));
}

TEST(EndOfTrackNodeTests, StopsTheTrackEvenWhileLooping)
{
    TestEndOfTrackContext context;
    EndOfTrackNode node { std::make_shared<RampNode>(), std::make_shared<LinearMapper>(), context, TEST_TRACK_STOP };

    EXPECT_EQ(read(node, 0, TEST_READ_LEN).left, rampUntil(TEST_READ_LEN, static_cast<size_t>(TEST_TRACK_STOP - TEST_PLAYHEAD_START)));
}

namespace {
constexpr int64_t TEST_BLOCK_INDEX = 500;
constexpr int64_t TEST_INPUT_LEAD = 20;
constexpr float TEST_INPUT_VOLUME = 0.5f;
}

TEST(MonitoredInputNodeTests, GivesTheBlocksInputWhereItIsReadLive)
{
    const std::atomic<float> volume { TEST_INPUT_VOLUME };
    const auto input = std::make_shared<MonitoredInput>(1, TEST_MAX_FRAMES, volume);
    const auto samples = ramp(0, TEST_MAX_FRAMES);
    const float* const device[] { samples.data() };
    input->read(device, 0, TEST_MAX_FRAMES, TEST_BLOCK_INDEX);
    MonitoredInputNode node { input, au::au4juce::numChannels, TEST_INPUT_LEAD };

    const auto output = read(node, TEST_BLOCK_INDEX + TEST_INPUT_LEAD, TEST_MAX_FRAMES);

    for (size_t i = 0; i < TEST_MAX_FRAMES; ++i) {
        // A mono input in both channels
        EXPECT_FLOAT_EQ(output.left[i], samples[i] * TEST_INPUT_VOLUME);
        EXPECT_FLOAT_EQ(output.right[i], samples[i] * TEST_INPUT_VOLUME);
    }
}

TEST(MonitoredInputNodeTests, IsSilentWhereItIsNotReadLive)
{
    const std::atomic<float> volume { 1.f };
    const auto input = std::make_shared<MonitoredInput>(1, TEST_MAX_FRAMES, volume);
    const auto samples = std::vector<float>(TEST_MAX_FRAMES, TEST_LEVEL);
    const float* const device[] { samples.data() };
    input->read(device, 0, TEST_MAX_FRAMES, TEST_BLOCK_INDEX);
    MonitoredInputNode node { input, au::au4juce::numChannels, TEST_INPUT_LEAD };

    // As where effects are primed, before what plays
    const auto output = read(node, TEST_BLOCK_INDEX, TEST_MAX_FRAMES);

    EXPECT_EQ(output.left, std::vector<float>(TEST_MAX_FRAMES, 0.f));
}
