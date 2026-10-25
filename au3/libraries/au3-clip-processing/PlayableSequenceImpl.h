/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "sequence-graph/ISequenceProcessingNode.h"

#include "au3-mixer/AudioIOSequences.h"

#include <memory>

//! A sequence as played: its samples are read from the graph processing it, the rest from the sequence itself
/*!
 * Reading is forward only. See `PlayableSequenceFactory` for what the graph does.
 */
class PlayableSequenceImpl final : public PlayableSequence
{
public:
    PlayableSequenceImpl(const PlayableSequence&, std::unique_ptr<ISequenceProcessingNode> graphEndNode);
    ~PlayableSequenceImpl() override = default;

    // WideSampleSequence
    size_t NChannels() const override;
    float GetChannelVolume(int channel) const override;
    double GetStartTime() const override;
    double GetEndTime() const override;
    double GetRate() const override;
    sampleFormat WidestEffectiveFormat() const override;
    bool HasTrivialEnvelope() const override;
    void GetEnvelopeValues(
        double* buffer, size_t bufferLen, double t0, bool backwards) const override;
    bool DoGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards,
        fillFormat fill = FillFormat::fillZero, bool mayThrow = true, sampleCount* pNumWithinClips = nullptr) const override;

    // PlayableSequence
    const ChannelGroup* FindChannelGroup() const override;
    bool GetSolo() const override;
    bool GetMute() const override;

    // AudioGraph::Channel
    AudioGraph::ChannelType GetChannelType() const override;

private:
    const PlayableSequence& mSequence;
    //! The end node of the graph processing the sequence; reading it changes its state, which a const `DoGet` does
    const std::unique_ptr<ISequenceProcessingNode> mGraphEndNode;
};
