/*
 * Audacity: A Digital Audio Editor
 */
#include "trackviewtypeattachment.h"

#include "au3-wave-track/WaveTrack.h"

namespace au::au3 {
static const AttachedTrackObjects::RegisteredFactory keyTrackViewType{
    [](Au3Track& track) -> std::shared_ptr<TrackViewTypeAttachment> { return std::make_shared<TrackViewTypeAttachment>(track); }
};

static constexpr auto TrackViewTypeAttr = "trackViewType";

TrackViewTypeAttachment& TrackViewTypeAttachment::Get(Au3Track* track)
{
    return track->AttachedTrackObjects::Get<TrackViewTypeAttachment>(keyTrackViewType);
}

TrackViewTypeAttachment& TrackViewTypeAttachment::Get(const Au3Track* track)
{
    return Get(const_cast<Au3Track*>(track));
}

void TrackViewTypeAttachment::CopyTo(Au3Track& track) const
{
    auto& attachment = Get(&track);
    attachment.SetTrackViewType(mTrackViewType);
}

TrackViewTypeAttachment::TrackViewTypeAttachment(Au3Track& track)
    : mTrack{track.shared_from_this()}
{
}

void TrackViewTypeAttachment::Reparent(const std::shared_ptr<Au3Track>& parent)
{
    mTrack = parent;
}

void TrackViewTypeAttachment::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(TrackViewTypeAttr, static_cast<int>(mTrackViewType));
}

bool TrackViewTypeAttachment::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    int nValue;
    if (attr == TrackViewTypeAttr && valueView.TryGet(nValue)) {
        switch (nValue) {
        case static_cast<int>(projectscene::TrackViewType::Undefined):
        case static_cast<int>(projectscene::TrackViewType::Waveform):
        case static_cast<int>(projectscene::TrackViewType::Spectrogram):
        case static_cast<int>(projectscene::TrackViewType::WaveformAndSpectrogram):
            mTrackViewType = static_cast<projectscene::TrackViewType>(nValue);
            break;
        default:
            mTrackViewType = projectscene::TrackViewType::Undefined;
            break;
        }
        return true;
    }
    return false;
}

projectscene::TrackViewType TrackViewTypeAttachment::GetTrackViewType() const
{
    return mTrackViewType;
}

void TrackViewTypeAttachment::SetTrackViewType(const projectscene::TrackViewType& type)
{
    mTrackViewType = type;
}
}
