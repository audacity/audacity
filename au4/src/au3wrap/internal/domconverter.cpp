#include "domconverter.h"

#include "libraries/lib-track/Track.h"

#include "wxtypes_convert.h"
#include "domau3types.h"

using namespace au::au3;

namespace {
static au::trackedit::TrackType trackType(const ::Track* track)
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
}

au::trackedit::TrackId DomConverter::trackId(const TrackId& au3trackId)
{
    return *(reinterpret_cast<const long*>(&au3trackId));
}

au::trackedit::Clip DomConverter::clip(const WaveTrack* waveTrack, const WaveClip* au3clip)
{
    au::trackedit::Clip clip;
    clip.key.trackId = trackId(waveTrack->GetId());
    clip.key.clipId = WaveClipID(au3clip).id;
    clip.title = wxToString(au3clip->GetName());
    clip.startTime = au3clip->GetPlayStartTime();
    clip.endTime = au3clip->GetPlayEndTime();

    //! TODO AU4 Just for tests
    static std::vector<muse::draw::Color> colors = {
        muse::draw::Color::fromString("#677CE4"),
        muse::draw::Color::fromString("#2C79B0"),
        muse::draw::Color::fromString("#2D9696")
    };
    size_t colorIdx = std::llabs(clip.key.trackId + clip.key.clipId);
    if (colorIdx >= colors.size()) {
        colorIdx = colorIdx % colors.size();
    }

    clip.color = colors.at(colorIdx);

    return clip;
}

au::trackedit::Track DomConverter::track(const ::Track *waveTrack)
{
    trackedit::Track au4t;
    au4t.id = DomConverter::trackId(waveTrack->GetId());
    au4t.title = wxToString(waveTrack->GetName());
    au4t.type = trackType(waveTrack);

    return au4t;
}
