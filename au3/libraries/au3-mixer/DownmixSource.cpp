/**********************************************************************

  Audacity: A Digital Audio Editor

  DownmixSource.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************/

#include "DownmixSource.h"

#include "AudioGraphChannel.h"
#include "WideSampleSequence.h"

SequenceDownmixSource::SequenceDownmixSource(AudioGraph::Source& downstream,
                                             const WideSampleSequence& sequence,
                                             const ArrayOf<bool>* channelMap)
    : mDownstream(downstream)
    , mSequence(sequence)
    , mpMap(channelMap)
{
}

AudioGraph::Source& SequenceDownmixSource::GetDownstream() const
{
    return mDownstream;
}

size_t SequenceDownmixSource::NChannels() const
{
    return mSequence.NChannels();
}

float SequenceDownmixSource::GetChannelGain(size_t channel) const
{
    return mSequence.GetChannelVolume(channel);
}

void SequenceDownmixSource::FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t iChannel)
{
    const bool* map = mpMap ? mpMap[iChannel].get() : nullptr;
    const auto end = channelFlags + numChannels;
    std::fill(channelFlags, end, 0);
    if (map) {
        // ignore left and right when downmixing is customized
        std::copy(map, map + numChannels, channelFlags);
    } else if (AudioGraph::IsMono(mSequence)) {
        std::fill(channelFlags, end, 1);
    } else if (iChannel == 0) {
        channelFlags[0] = 1;
    } else if (iChannel == 1) {
        if (numChannels >= 2) {
            channelFlags[1] = 1;
        } else {
            channelFlags[0] = 1;
        }
    }
}

bool SequenceDownmixSource::CanMakeMono() const
{
    return mpMap == nullptr;
}

SimpleDonwmixSource::SimpleDonwmixSource(AudioGraph::Source& downstream, size_t channels)
    : mDownstream(downstream), mNChannels(channels)
{
}

AudioGraph::Source& SimpleDonwmixSource::GetDownstream() const
{
    return mDownstream;
}

size_t SimpleDonwmixSource::NChannels() const
{
    return mNChannels;
}

float SimpleDonwmixSource::GetChannelGain(size_t) const
{
    return 1.0f;
}

void SimpleDonwmixSource::FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t iChannel)
{
    if (mNChannels == 1) {
        for (size_t i = 0; i < numChannels; ++i) {
            channelFlags[i] = true;
        }
    } else {
        for (size_t i = 0; i < numChannels; ++i) {
            channelFlags[i] = i == iChannel;
        }
    }
}

bool SimpleDonwmixSource::CanMakeMono() const
{
    return true;
}
