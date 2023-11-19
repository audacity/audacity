/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file WaveformApperance.cpp
 
 Paul Licameli
 
 **********************************************************************/

#include "WaveformAppearance.h"
static const AttachedTrackObjects::RegisteredFactory keyWA{
   [](Track &track) -> std::shared_ptr<WaveformAppearance> {
      if (const auto pTrack = dynamic_cast<WaveTrack *>(&track))
         return std::make_shared<WaveformAppearance>(*pTrack);
      else
         return nullptr;
   }
};

WaveformAppearance &WaveformAppearance::Get(WaveChannel &channel)
{
   return channel.GetTrack().AttachedObjects::Get<WaveformAppearance>(keyWA);
}

const WaveformAppearance &WaveformAppearance::Get(const WaveChannel &channel)
{
   return Get(const_cast<WaveChannel &>(channel));
}

WaveformAppearance::WaveformAppearance(WaveTrack &track)
   : mwTrack{
      std::static_pointer_cast<WaveTrack>(track.shared_from_this()) }
{
}

WaveformAppearance::~WaveformAppearance() = default;

void WaveformAppearance::Reparent(const std::shared_ptr<Track> &parent)
{
   mwTrack = std::dynamic_pointer_cast<WaveTrack>(parent);
}
