/**********************************************************************

  Audacity: A Digital Audio Editor

  Channel.cpp

  Dominic Mazzoni

  Paul Licameli split from Track.h

*//*******************************************************************/
#include "Channel.h"

#include <algorithm>
#include <numeric>

ChannelGroupInterval::~ChannelGroupInterval() = default;

ChannelInterval::~ChannelInterval() = default;

WideChannelGroupInterval::~WideChannelGroupInterval() = default;

Channel::~Channel() = default;

size_t Channel::GetChannelIndex() const
{
    auto& group = DoGetChannelGroup();
    int index = -1;
    for (size_t ii = 0, nn = group.NChannels(); ii < nn; ++ii) {
        if (group.GetChannel(ii).get() == this) {
            index = ii;
            break;
        }
    }
    assert(index >= 0);
    return index;
}

const ChannelGroup& Channel::GetChannelGroup() const
{
    return DoGetChannelGroup();
}

ChannelGroup& Channel::GetChannelGroup()
{
    return DoGetChannelGroup();
}

ChannelGroup::~ChannelGroup() = default;

double ChannelGroup::GetStartTime() const
{
    const auto& range = Intervals();
    if (range.empty()) {
        return 0;
    }
    return std::accumulate(range.first, range.second,
                           std::numeric_limits<double>::max(),
                           [](double acc, const auto& pInterval){
        return std::min(acc, pInterval->Start());
    });
}

double ChannelGroup::GetEndTime() const
{
    const auto& range = Intervals();
    if (range.empty()) {
        return 0;
    }
    return std::accumulate(range.first, range.second,
                           std::numeric_limits<double>::lowest(),
                           [](double acc, const auto& pInterval){
        return std::max(acc, pInterval->End());
    });
}
