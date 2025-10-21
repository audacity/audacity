/*
* Audacity: A Digital Audio Editor
*/
#include "trackclipitem.h"

using namespace au::projectscene;

TrackClipItem::TrackClipItem(QObject* parent)
    : ViewTrackItem(parent)
{
}

void TrackClipItem::setClip(const trackedit::Clip& clip)
{
    m_key = TrackObjectKey(clip.key);
    m_title = clip.title;
    m_color = clip.color.toQColor();
    m_groupId = clip.groupId;
    m_pitch = clip.pitch;
    m_speed = clip.speed;

    emit titleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit colorChanged();
    emit groupIdChanged();
    emit waveChanged();
    emit timeChanged();
}

int TrackClipItem::groupId() const
{
    return m_groupId;
}

int TrackClipItem::pitch() const
{
    return m_pitch;
}

int TrackClipItem::speedPercentage() const
{
    return qRound(100.0 / m_speed);
}
