/*
* Audacity: A Digital Audio Editor
*/
#include "trackclipitem.h"

#include "framework/global/realfn.h"

using namespace au::projectscene;

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

    emit titleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit colorChanged();
    emit groupIdChanged();
    emit contentChanged();
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
