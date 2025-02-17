/*
* Audacity: A Digital Audio Editor
*/
#include "cliplistitem.h"

using namespace au::projectscene;

ClipListItem::ClipListItem(QObject* parent)
    : QObject(parent)
{
}

void ClipListItem::setClip(const trackedit::Clip& clip)
{
    m_clip = clip;

    emit titleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit colorChanged();
    emit groupIdChanged();
}

const au::trackedit::Clip& ClipListItem::clip() const
{
    return m_clip;
}

ClipKey ClipListItem::key() const
{
    return ClipKey(m_clip.key);
}

QString ClipListItem::title() const
{
    return m_clip.title;
}

void ClipListItem::setTitle(const QString& newTitle)
{
    m_clip.title = newTitle;
    emit titleChanged();
}

QColor ClipListItem::color() const
{
    return m_clip.color.toQColor();
}

int ClipListItem::groupId() const
{
    return m_clip.groupId;
}

double ClipListItem::x() const
{
    return m_x;
}

void ClipListItem::setX(double newX)
{
    if (qFuzzyCompare(m_x, newX)) {
        return;
    }
    m_x = newX;
    emit xChanged();
}

double ClipListItem::width() const
{
    return m_width;
}

void ClipListItem::setWidth(double newWidth)
{
    if (qFuzzyCompare(m_width, newWidth)) {
        return;
    }
    m_width = newWidth;
    emit widthChanged();
}

bool ClipListItem::selected() const
{
    return m_selected;
}

void ClipListItem::setSelected(bool newSelected)
{
    if (m_selected == newSelected) {
        return;
    }
    m_selected = newSelected;
    emit selectedChanged();
}

ClipTime ClipListItem::time() const
{
    return m_time;
}

void ClipListItem::setTime(const ClipTime& newTime)
{
    if (m_time == newTime) {
        return;
    }
    m_time = newTime;
    emit timeChanged();
}

double ClipListItem::leftVisibleMargin() const
{
    return m_leftVisibleMargin;
}

void ClipListItem::setLeftVisibleMargin(double newLeftVisibleMargin)
{
    if (qFuzzyCompare(m_leftVisibleMargin, newLeftVisibleMargin)) {
        return;
    }
    m_leftVisibleMargin = newLeftVisibleMargin;
    emit leftVisibleMarginChanged();
}

double ClipListItem::rightVisibleMargin() const
{
    return m_rightVisibleMargin;
}

void ClipListItem::setRightVisibleMargin(double newRightVisibleMargin)
{
    if (qFuzzyCompare(m_rightVisibleMargin, newRightVisibleMargin)) {
        return;
    }
    m_rightVisibleMargin = newRightVisibleMargin;
    emit rightVisibleMarginChanged();
}

int ClipListItem::pitch() const
{
    return m_clip.pitch;
}

int ClipListItem::speedPercentage() const
{
    return qRound(100.0 / m_clip.speed);
}
