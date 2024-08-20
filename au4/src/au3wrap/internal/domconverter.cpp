#include "domconverter.h"

#include "libraries/lib-track/Track.h"

#include "wxtypes_convert.h"
#include "domau3types.h"

using namespace au::au3;

au::trackedit::TrackId DomConverter::trackId(const TrackId& au3trackId)
{
    return *(reinterpret_cast<const long*>(&au3trackId));
}

au::trackedit::Clip DomConverter::clip(const WaveTrack* waveTrack, const WaveClip* au3clip)
{
    au::trackedit::Clip clip;
    clip.key.trackId = trackId(waveTrack->GetId());
    clip.key.clipId = WaveClipID(au3clip).id;
    clip.title = wxToSting(au3clip->GetName());
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
