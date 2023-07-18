/**********************************************************************

  Audacity: A Digital Audio Editor

  Channel.cpp

  Dominic Mazzoni

  Paul Licameli split from Track.h

*//*******************************************************************/
#include "Channel.h"

ChannelGroupInterval::~ChannelGroupInterval() = default;

ChannelInterval::~ChannelInterval() = default;

WideChannelGroupInterval::WideChannelGroupInterval(
   const ChannelGroup &group, double start, double end
)  : ChannelGroupInterval{ start, end }
   , mNChannels{ group.NChannels() }
{
   assert(group.IsLeader());
   assert(mNChannels >= 1); // Post of ChannelGroup::NChannels
}

WideChannelGroupInterval::~WideChannelGroupInterval() = default;

Channel::~Channel() = default;

int Channel::FindChannelIndex() const
{
   auto &group = DoGetChannelGroup();
   int index = -1;
   for (size_t ii = 0, nn = group.NChannels(); ii < nn; ++ii)
      if (group.GetChannel(ii).get() == this) {
         index = ii;
         break;
      }
   // post of DoGetChannelGroup
   assert(index >= 0);

   // TODO wide wave tracks -- remove this stronger assertion
   assert(index == 0);

   return index;
}

const ChannelGroup &Channel::GetChannelGroup() const
{
   assert(FindChannelIndex() >= 0);
   return DoGetChannelGroup();
}

ChannelGroup &Channel::GetChannelGroup()
{
   assert(FindChannelIndex() >= 0);
   return DoGetChannelGroup();
}

size_t Channel::GetChannelIndex() const
{
   return FindChannelIndex();
}

ChannelGroup::~ChannelGroup() = default;
