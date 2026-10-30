/*
* Audacity: A Digital Audio Editor
*/
#include "trackclipitem.h"

#include "framework/global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr FadeTypes::Type fadeType(au::trackedit::FadeType type)
{
    switch (type) {
    case au::trackedit::FadeType::Linear:
        return FadeTypes::Type::LINEAR;
    case au::trackedit::FadeType::Logarithmic:
        return FadeTypes::Type::LOGARITHMIC;
    default:
        assert(false);
        return FadeTypes::Type::LINEAR;
    }
}
}

TrackClipItem::TrackClipItem(QObject* parent)
    : ViewTrackItem(parent)
{
}

void TrackClipItem::setClip(const trackedit::Clip& clip)
{
    m_key = TrackItemKey(clip.key);
    m_title = clip.title;
    m_color = configuration()->clipColor(clip.colorIndex).toQColor();
    m_selectedColor = configuration()->clipSelectedColor(clip.colorIndex).toQColor();
    m_groupId = clip.groupId;
    m_pitch = clip.pitch;
    m_speed = clip.speed;
    m_fadeInDuration = clip.fadeInDuration;
    m_fadeOutDuration = clip.fadeOutDuration;
    m_fadeInType = fadeType(clip.fadeInType);
    m_fadeOutType = fadeType(clip.fadeOutType);

    emit titleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit fadesChanged();
    emit colorChanged();
    emit groupIdChanged();
    emit contentChanged();
    emit timeChanged();
}

bool TrackClipItem::isGrouped() const
{
    return m_groupId != -1;
}

int TrackClipItem::pitch() const
{
    return m_pitch;
}

int TrackClipItem::speedPercentage() const
{
    return qRound(100.0 / m_speed);
}

bool TrackClipItem::isPitchModified() const
{
    return m_pitch != 0;
}

bool TrackClipItem::isSpeedModified() const
{
    return !muse::RealIsEqual(m_speed, 1.0);
}

double TrackClipItem::speed() const
{
    return m_speed;
}

double TrackClipItem::fadeInDuration() const
{
    return m_fadeInDuration;
}

double TrackClipItem::fadeOutDuration() const
{
    return m_fadeOutDuration;
}

FadeTypes::Type TrackClipItem::fadeInType() const
{
    return m_fadeInType;
}

FadeTypes::Type TrackClipItem::fadeOutType() const
{
    return m_fadeOutType;
}
