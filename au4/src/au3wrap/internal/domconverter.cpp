#include "domconverter.h"

#include "libraries/lib-track/Track.h"

#include "../wxtypes_convert.h"

using namespace au::au3;

au::processing::TrackId DomConverter::trackId(const TrackId& au3trackId)
{
    return *(reinterpret_cast<const long*>(&au3trackId));
}

au::processing::Clip DomConverter::clip(const WaveTrack* waveTrack, const WaveClip* au3clip, int index)
{
    //! TODO AU4 Just for tests
    static std::vector<muse::draw::Color> colors = {
        muse::draw::Color::fromString("#677CE4"),
        muse::draw::Color::fromString("#2C79B0"),
        muse::draw::Color::fromString("#2D9696")
    };

    au::processing::Clip clip;
    clip.key.trackId = trackId(waveTrack->GetId());
    clip.key.index = index;
    clip.title = wxToSting(au3clip->GetName());
    clip.startTime = au3clip->GetPlayStartTime();
    clip.endTime = au3clip->GetPlayEndTime();

    size_t colorIdx = clip.key.trackId + clip.key.index;
    if (colorIdx >= colors.size()) {
        colorIdx = colorIdx % colors.size();
    }

    clip.color = colors.at(colorIdx);

    return clip;
}
