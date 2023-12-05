/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveformScale.cpp

**********************************************************************/

#include "WaveformScale.h"

#include "Channel.h"
#include "WaveTrack.h"

static const ChannelGroup::Attachments::RegisteredFactory
key2{ [](auto &) { return std::make_unique<WaveformScale>(); } };

WaveformScale &WaveformScale::Get(const WaveTrack &track)
{
   auto &mutTrack = const_cast<WaveTrack&>(track);
   return mutTrack.GetGroupData().Attachments
      ::Get<WaveformScale>(key2);
}

WaveformScale::~WaveformScale() = default;

auto WaveformScale::Clone() const -> PointerType
{
   return std::make_unique<WaveformScale>(*this);
}

int WaveformScale::ZeroLevelYCoordinate(wxRect rect) const
{
   return rect.GetTop() +
      (int)((mDisplayMax / (mDisplayMax - mDisplayMin)) * rect.height);
}
