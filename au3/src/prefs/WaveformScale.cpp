/**********************************************************************

Audacity: A Digital Audio Editor

WaveformScale.cpp

Paul Licameli

***********************************************************************/

#include "WaveformScale.h"
#include "WaveTrack.h"
#include <wx/gdicmn.h>

static const ChannelGroup::Attachments::RegisteredFactory
    key2{ [](auto&) { return std::make_unique<WaveformScale>(); } };

WaveformScale& WaveformScale::Get(const WaveTrack& track)
{
    auto& mutTrack = const_cast<WaveTrack&>(track);
    return mutTrack.Attachments::Get<WaveformScale>(key2);
}

WaveformScale& WaveformScale::Get(const WaveChannel& channel)
{
    return Get(channel.GetTrack());
}

WaveformScale::~WaveformScale() = default;

auto WaveformScale::Clone() const -> PointerType
{
    return std::make_unique<WaveformScale>(*this);
}

int WaveformScale::ZeroLevelYCoordinate(wxRect rect) const
{
    return rect.GetTop()
           + (int)((mDisplayMax / (mDisplayMax - mDisplayMin)) * rect.height);
}
