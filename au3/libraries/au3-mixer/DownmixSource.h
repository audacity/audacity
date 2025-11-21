/**********************************************************************

  Audacity: A Digital Audio Editor

  DownmixSource.h

*******************************************************************/

#pragma once
#include "MemoryX.h"

class WideSampleSequence;

namespace AudioGraph {
class Source;
}

/** @brief Describes an input source for DownmixStage
 * Decorates an AudioGraph::Source with operations
 * that are necessary to perform down mixing, which are: number
 * of source channels, per-channel gain, channel mapping
 */
class DownmixSource
{
public:
    DownmixSource() = default;

    virtual ~DownmixSource() = default;
    //! Returns underlying `AudioGraph::Source` processed by Mixer
    virtual AudioGraph::Source& GetDownstream() const = 0;
    //! Number of output channels of the underlying Source
    virtual size_t NChannels() const = 0;
    //! Gain multiplier that should be applied to the channel
    virtual float GetChannelGain(size_t channel) const = 0;
    //! For the given `iChannel` fills the channelFlags array, that describes
    //! to which output it should go.
    virtual void FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t iChannel) = 0;
    //! Returns true if source channels could be combined into mono if needed
    virtual bool CanMakeMono() const = 0;
};

//! Downmix source that uses `Sequence` as a source for channel
//! operations.
class SequenceDownmixSource final : public DownmixSource
{
    AudioGraph::Source& mDownstream;
    const WideSampleSequence& mSequence;
    //! many-to-one mixing of channels
    //! Pointer into array of arrays
    const ArrayOf<bool>* mpMap {};
public:

    SequenceDownmixSource(AudioGraph::Source& downstream, const WideSampleSequence& sequence,
                          //! Null or else must have a lifetime enclosing this objects's
                          const ArrayOf<bool>* channelMap);

    AudioGraph::Source& GetDownstream() const override;
    size_t NChannels() const override;
    float GetChannelGain(size_t channel) const override;
    void FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t iChannel) override;
    bool CanMakeMono() const override;
};

//! No gain applied, channels are either mapped to corresponding
//! ones or combined into mono, depending on FindChannelFlags input
class SimpleDonwmixSource final : public DownmixSource
{
    AudioGraph::Source& mDownstream;
    size_t mNChannels;
public:

    SimpleDonwmixSource(AudioGraph::Source& downstream, size_t channels);

    AudioGraph::Source& GetDownstream() const override;
    size_t NChannels() const override;
    float GetChannelGain(size_t channel) const override;
    void FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t iChannel) override;
    bool CanMakeMono() const override;
};
