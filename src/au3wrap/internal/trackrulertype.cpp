/*
* Audacity: A Digital Audio Editor
*/
#include "trackrulertype.h"

using namespace au::au3;
using namespace au::trackedit;

static const AttachedTrackObjects::RegisteredFactory keyTrackRulerType{
    [](Au3Track& track) -> std::shared_ptr<TrackRulerTypeAttachment> { return std::make_shared<TrackRulerTypeAttachment>(track); }
};

static constexpr auto RulerTypeAttr = "rulerType";

TrackRulerTypeAttachment& TrackRulerTypeAttachment::Get(Au3Track* track)
{
    return track->AttachedTrackObjects::Get<TrackRulerTypeAttachment>(keyTrackRulerType);
}

TrackRulerTypeAttachment& TrackRulerTypeAttachment::Get(const Au3Track* track)
{
    return Get(const_cast<Au3Track*>(track));
}

void TrackRulerTypeAttachment::CopyTo(Au3Track& track) const
{
    auto& rulerType = Get(&track);
    rulerType.SetRulerType(mRulerType);
}

TrackRulerTypeAttachment::TrackRulerTypeAttachment(Au3Track& track)
    : mTrack{track.shared_from_this()}
    , mRulerType{TrackRulerType::Linear}
{
}

void TrackRulerTypeAttachment::Reparent(const std::shared_ptr<Au3Track>& parent)
{
    mTrack = parent;
}

void TrackRulerTypeAttachment::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(RulerTypeAttr, static_cast<int>(mRulerType));
}

bool TrackRulerTypeAttachment::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    int nValue;
    if (attr == RulerTypeAttr && valueView.TryGet(nValue)) {
        mRulerType = static_cast<TrackRulerType>(nValue);
        return true;
    }
    return false;
}

TrackRulerType TrackRulerTypeAttachment::GetRulerType() const
{
    return mRulerType;
}

void TrackRulerTypeAttachment::SetRulerType(const TrackRulerType& rulerType)
{
    mRulerType = rulerType;
}
