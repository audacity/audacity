/*
 * Audacity: A Digital Audio Editor
 */
#include "trackhalfwaveattachment.h"

#include "au3-wave-track/WaveTrack.h"

#include "waveformscale.h"

namespace au::au3 {
static const AttachedTrackObjects::RegisteredFactory keyTrackHalfWave{
    [](Au3Track& track) -> std::shared_ptr<TrackHalfWaveAttachment> { return std::make_shared<TrackHalfWaveAttachment>(track); }
};

static constexpr auto HalfWaveAttr = "halfWave";

TrackHalfWaveAttachment& TrackHalfWaveAttachment::Get(Au3Track* track)
{
    return track->AttachedTrackObjects::Get<TrackHalfWaveAttachment>(keyTrackHalfWave);
}

TrackHalfWaveAttachment& TrackHalfWaveAttachment::Get(const Au3Track* track)
{
    return Get(const_cast<Au3Track*>(track));
}

void TrackHalfWaveAttachment::CopyTo(Au3Track& track) const
{
    auto& attachment = Get(&track);
    attachment.SetHalfWave(mIsHalfWave);
}

TrackHalfWaveAttachment::TrackHalfWaveAttachment(Au3Track& track)
    : mTrack{track.shared_from_this()}
{
}

void TrackHalfWaveAttachment::Reparent(const std::shared_ptr<Au3Track>& parent)
{
    mTrack = parent;
}

void TrackHalfWaveAttachment::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(HalfWaveAttr, mIsHalfWave ? 1 : 0);
}

bool TrackHalfWaveAttachment::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    int nValue;
    if (attr == HalfWaveAttr && valueView.TryGet(nValue)) {
        mIsHalfWave = (nValue != 0);
        if (mIsHalfWave) {
            applyToWaveformScale();
        }
        return true;
    }
    return false;
}

bool TrackHalfWaveAttachment::IsHalfWave() const
{
    return mIsHalfWave;
}

void TrackHalfWaveAttachment::SetHalfWave(bool halfWave)
{
    mIsHalfWave = halfWave;
}

void TrackHalfWaveAttachment::applyToWaveformScale()
{
    auto track = mTrack.lock();
    if (!track) {
        return;
    }
    auto* waveTrack = dynamic_cast<WaveTrack*>(track.get());
    if (!waveTrack) {
        return;
    }

    auto& scale = WaveformScale::Get(*waveTrack);
    float min = 0.0f;
    float max = 0.0f;
    scale.GetDisplayBounds(min, max);
    scale.SetDisplayBounds(0.0f, max);
}
}
