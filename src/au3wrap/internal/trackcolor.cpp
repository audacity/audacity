#include "trackcolor.h"

using namespace au::au3;

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
