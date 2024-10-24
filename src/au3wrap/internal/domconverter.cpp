#include "domconverter.h"

#include "au3types.h"
#include "libraries/lib-track/Track.h"

#include "wxtypes_convert.h"
#include "domau3types.h"

#include "../au3types.h"

using namespace au::au3;

namespace {
static au::trackedit::TrackType trackType(const Au3Track* track)
{
    switch (track->NChannels()) {
    case 0:
        return au::trackedit::TrackType::Label;
    case 1:
        return au::trackedit::TrackType::Mono;
    case 2:
        return au::trackedit::TrackType::Stereo;
    default:
        break;
    }

    return au::trackedit::TrackType::Undefined;
}

static muse::draw::Color trackColor(const au::trackedit::TrackId& trackId)
{
    //! TODO AU4 Just for tests
    static std::vector<muse::draw::Color> colors = {
        muse::draw::Color::fromString("#66A3FF"),
        muse::draw::Color::fromString("#9996FC"),
        muse::draw::Color::fromString("#DA8CCC"),
        muse::draw::Color::fromString("#F08080"),
        muse::draw::Color::fromString("#FF9E65"),
        muse::draw::Color::fromString("#E8C050"),
        muse::draw::Color::fromString("#74BE59"),
        muse::draw::Color::fromString("#34B494"),
        muse::draw::Color::fromString("#48BECF")
    };
    size_t colorIdx = std::llabs(trackId);
    if (colorIdx >= colors.size()) {
        colorIdx = colorIdx % colors.size();
    }

    return colors.at(colorIdx);
}
}

au::trackedit::TrackId DomConverter::trackId(const Au3TrackId& au3trackId)
{
    return *(reinterpret_cast<const long*>(&au3trackId));
}

au::trackedit::Clip DomConverter::clip(const Au3WaveTrack* waveTrack, const WaveClip* au3clip)
{
    au::trackedit::Clip clip;
    clip.key.trackId = trackId(waveTrack->GetId());
    clip.key.clipId = WaveClipID(au3clip).id;
    clip.title = wxToString(au3clip->GetName());
    clip.startTime = au3clip->GetPlayStartTime();
    clip.endTime = au3clip->GetPlayEndTime();
    clip.color = trackColor(clip.key.trackId);
    clip.stereo = au3clip->NChannels() > 1;

    return clip;
}

au::trackedit::Track DomConverter::track(const Au3Track* waveTrack)
{
    trackedit::Track au4t;
    au4t.id = DomConverter::trackId(waveTrack->GetId());
    au4t.title = wxToString(waveTrack->GetName());
    au4t.type = trackType(waveTrack);
    au4t.color = trackColor(au4t.id);

    return au4t;
}
