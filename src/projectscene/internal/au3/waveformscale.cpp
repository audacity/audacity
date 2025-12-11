/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveformScale.cpp

**********************************************************************/

#include "au3-wave-track/WaveTrack.h"
#include "au3wrap/au3types.h"

#include "waveformscale.h"

using namespace au::au3;

static const ChannelGroup::Attachments::RegisteredFactory
    key2{ [](auto&) { return std::make_unique<WaveformScale>(); } };

WaveformScale& WaveformScale::Get(const Au3WaveTrack& track)
{
    auto& mutTrack = const_cast<WaveTrack&>(track);
    return mutTrack.Attachments::Get<WaveformScale>(key2);
}

WaveformScale& WaveformScale::Get(const WaveChannel& channel)
{
    return Get(channel.GetTrack());
}

auto WaveformScale::Clone() const -> PointerType
{
    return std::make_unique<WaveformScale>(*this);
}
