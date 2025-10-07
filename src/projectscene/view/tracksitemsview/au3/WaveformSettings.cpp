#include "WaveformSettings.h"
#include "WaveTrack.h"
#include "Channel.h"

namespace au::projectscene {
static const ChannelGroup::Attachments::RegisteredFactory
    key1{ [](auto&) {
        return std::make_unique<WaveformSettings>(WaveformSettings::defaults());
    } };

WaveformSettings& WaveformSettings::Get(const WaveTrack& track)
{
    auto& mutTrack = const_cast<WaveTrack&>(track);
    return mutTrack.Attachments::Get<WaveformSettings>(key1);
}

WaveformSettings& WaveformSettings::defaults()
{
    static WaveformSettings instance;
    return instance;
}

auto WaveformSettings::Clone() const -> PointerType
{
    return std::make_unique<WaveformSettings>(*this);
}
}
