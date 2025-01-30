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

TrackColor::TrackColor(Track& track)
    : mTrack{track.shared_from_this()}
{
    auto clipColors = projectSceneConfiguration()->clipColors();
    std::vector<muse::draw::Color> colors;
    for (const auto& color : clipColors) {
        colors.push_back(muse::draw::Color::fromString(color.second));
    }

    size_t colorIdx = std::llabs(track.GetId());
    if (colorIdx >= colors.size()) {
        colorIdx = colorIdx % colors.size();
    }
    mColor = colors.at(colorIdx);
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

muse::draw::Color TrackColor::GetColor() const { return mColor; }
