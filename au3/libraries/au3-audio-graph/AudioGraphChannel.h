/**********************************************************************

 Audacity: A Digital Audio Editor

 @file AudioGraphChannel.h

 @brief Abstraction of a channel of a wide stream that knows whether it is mono,
 left, or right

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_AUDIO_GRAPH_CHANNEL__
#define __AUDACITY_AUDIO_GRAPH_CHANNEL__

namespace AudioGraph {
//! Mutually exclusive channel classifications
enum ChannelType : unsigned
{
    MonoChannel,
    LeftChannel,
    RightChannel,
};

struct AUDIO_GRAPH_API Channel {
    virtual ~Channel();
    //! Classify this channel
    virtual ChannelType GetChannelType() const = 0;
};

//! Whether the channel is mono
inline bool IsMono(const Channel& channel)
{
    return channel.GetChannelType() == MonoChannel;
}

//! Whether the channel may play through a left speaker
inline bool PlaysLeft(const Channel& channel)
{
    const auto type = channel.GetChannelType();
    return type == MonoChannel || type == LeftChannel;
}

//! Whether the channel may play through a right speaker
inline bool PlaysRight(const Channel& channel)
{
    const auto type = channel.GetChannelType();
    return type == MonoChannel || type == RightChannel;
}
}

#endif
