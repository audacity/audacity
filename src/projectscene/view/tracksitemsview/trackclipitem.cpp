/*
* Audacity: A Digital Audio Editor
*/
#include "trackclipitem.h"

using namespace au::projectscene;

TrackClipItem::TrackClipItem(QObject* parent)
    : QObject(parent)
{
}

void TrackClipItem::setClip(const trackedit::Clip& clip)
{
    m_clip = clip;

    emit titleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit colorChanged();
    emit groupIdChanged();
    emit waveChanged();
}

const au::trackedit::Clip& TrackClipItem::clip() const
{
    return m_clip;
}

ClipKey TrackClipItem::key() const
{
    return ClipKey(m_clip.key);
}

QString TrackClipItem::title() const
{
    return m_clip.title;
}

void TrackClipItem::setTitle(const QString& newTitle)
{
    m_clip.title = newTitle;
    emit titleChanged();
}

QColor TrackClipItem::color() const
{
    return m_clip.color.toQColor();
}

int TrackClipItem::groupId() const
{
    return m_clip.groupId;
}

double TrackClipItem::x() const
{
    return m_x;
}

void TrackClipItem::setX(double newX)
{
    if (qFuzzyCompare(m_x, newX)) {
        return;
    }
    m_x = newX;
    emit xChanged();
}

double TrackClipItem::width() const
{
    return m_width;
}

void TrackClipItem::setWidth(double newWidth)
{
    if (qFuzzyCompare(m_width, newWidth)) {
        return;
    }
    m_width = newWidth;
    emit widthChanged();
}

bool TrackClipItem::selected() const
{
    return m_selected;
}

void TrackClipItem::setSelected(bool newSelected)
{
    if (m_selected == newSelected) {
        return;
    }
    m_selected = newSelected;
    emit selectedChanged();
}

ClipTime TrackClipItem::time() const
{
    return m_time;
}

void TrackClipItem::setTime(const ClipTime& newTime)
{
    if (m_time == newTime) {
        return;
    }
    m_time = newTime;
    emit timeChanged();
}

double TrackClipItem::leftVisibleMargin() const
{
    return m_leftVisibleMargin;
}

void TrackClipItem::setLeftVisibleMargin(double newLeftVisibleMargin)
{
    if (qFuzzyCompare(m_leftVisibleMargin, newLeftVisibleMargin)) {
        return;
    }
    m_leftVisibleMargin = newLeftVisibleMargin;
    emit leftVisibleMarginChanged();
}

double TrackClipItem::rightVisibleMargin() const
{
    return m_rightVisibleMargin;
}

void TrackClipItem::setRightVisibleMargin(double newRightVisibleMargin)
{
    if (qFuzzyCompare(m_rightVisibleMargin, newRightVisibleMargin)) {
        return;
    }
    m_rightVisibleMargin = newRightVisibleMargin;
    emit rightVisibleMarginChanged();
}

int TrackClipItem::pitch() const
{
    return m_clip.pitch;
}

int TrackClipItem::speedPercentage() const
{
    return qRound(100.0 / m_clip.speed);
}
