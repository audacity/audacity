/**********************************************************************

  Audacity: A Digital Audio Editor

  AVChannelLayoutWrapper.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

// Channel layout was introduced earlier, but we must use it starting from 59
#define HAS_AV_CHANNEL_LAYOUT (LIBAVUTIL_VERSION_MAJOR >= 59)

#include <cstdint>
#include <memory>

typedef struct AVChannelLayout AVChannelLayout;

class AVChannelLayoutWrapper
{
public:
    virtual ~AVChannelLayoutWrapper() = default;

    virtual uint64_t GetLegacyChannelLayout() const noexcept = 0;
    virtual int GetChannelsCount() const noexcept = 0;

    virtual const AVChannelLayout* GetChannelLayout() const noexcept = 0;

    virtual std::unique_ptr<AVChannelLayoutWrapper> Clone() const = 0;
}; // class AVChannelLayoutWrapper
