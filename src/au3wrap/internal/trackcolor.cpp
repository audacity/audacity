#include "trackcolor.h"

using namespace au::au3;

static const AttachedTrackObjects::RegisteredFactory keyTrackColor{
    [](Track& track) -> std::shared_ptr<TrackColor> { return std::make_shared<TrackColor>(track); }
};

static constexpr auto ColorAttr = "color";

TrackColor& TrackColor::Get(Track* track)
{
    return track->AttachedTrackObjects::Get<TrackColor>(keyTrackColor);
}

TrackColor& TrackColor::Get(const Track* track)
{
    return Get(const_cast<Track*>(track));
}

void TrackColor::CopyTo(Track& track) const
{
    auto& color = Get(&track);
    color.SetColor(mColor);
}

TrackColor::TrackColor(Track& track)
    : mTrack{track.shared_from_this()}
{
    auto ntrack = mTrack.lock();
    if (!ntrack) {
        return;
    }

    if (ntrack->GetId() == -1) {
        //Only assign color to newly created tracks
        mColor = muse::draw::Color::fromString(projectSceneConfiguration()->nextTrackColor());
    }
}

void TrackColor::Reparent(const std::shared_ptr<Track>& parent)
{
    mTrack = parent;
}

void TrackColor::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(ColorAttr, mColor.toString());
}

bool TrackColor::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    if (attr == ColorAttr) {
        mColor = muse::draw::Color::fromString(valueView.ToWString());
        return true;
    }

    return false;
}

muse::draw::Color TrackColor::GetColor() const
{
    return mColor;
}

void TrackColor::SetColor(const muse::draw::Color& color)
{
    mColor = color;
}
