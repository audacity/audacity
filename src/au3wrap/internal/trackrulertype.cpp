/*
* Audacity: A Digital Audio Editor
*/
#include "trackrulertype.h"

using namespace au::au3;

static const AttachedTrackObjects::RegisteredFactory keyTrackRulerType{
    [](Track& track) -> std::shared_ptr<TrackRulerType> { return std::make_shared<TrackRulerType>(track); }
};

static constexpr auto RulerTypeAttr = "rulerType";

TrackRulerType& TrackRulerType::Get(Track* track)
{
    return track->AttachedTrackObjects::Get<TrackRulerType>(keyTrackRulerType);
}

TrackRulerType& TrackRulerType::Get(const Track* track)
{
    return Get(const_cast<Track*>(track));
}

void TrackRulerType::CopyTo(Track& track) const
{
    auto& rulerType = Get(&track);
    rulerType.SetRulerType(mRulerType);
}

TrackRulerType::TrackRulerType(Track& track)
    : mTrack{track.shared_from_this()}
    , mRulerType{RulerType::Linear}
{
}

void TrackRulerType::Reparent(const std::shared_ptr<Track>& parent)
{
    mTrack = parent;
}

void TrackRulerType::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(RulerTypeAttr, static_cast<int>(mRulerType));
}

bool TrackRulerType::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    int nValue;
    if (attr == RulerTypeAttr && valueView.TryGet(nValue)) {
        mRulerType = static_cast<RulerType>(nValue);
        return true;
    }
    return false;
}

RulerType TrackRulerType::GetRulerType() const
{
    return mRulerType;
}

void TrackRulerType::SetRulerType(const RulerType& rulerType)
{
    mRulerType = rulerType;
}
