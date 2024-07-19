/*
* Audacity: A Digital Audio Editor
*/
#include "cliplistitem.h"

using namespace au::projectscene;

ClipListItem::ClipListItem(const processing::Clip& clip)
    : m_clip(clip)
{
}

void ClipListItem::setClip(const processing::Clip& clip)
{
    m_clip = clip;

    emit titleChanged();
}

const au::processing::Clip& ClipListItem::clip() const
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

double ClipListItem::moveMaximumX() const
{
    return m_moveMaximumX;
}

void ClipListItem::setMoveMaximumX(double newMoveMaximumX)
{
    if (qFuzzyCompare(m_moveMaximumX, newMoveMaximumX)) {
        return;
    }
    m_moveMaximumX = newMoveMaximumX;
    emit moveMaximumXChanged();
}

double ClipListItem::moveMinimumX() const
{
    return m_moveMinimumX;
}

void ClipListItem::setMoveMinimumX(double newMoveMinimumX)
{
    if (qFuzzyCompare(m_moveMinimumX, newMoveMinimumX)) {
        return;
    }
    m_moveMinimumX = newMoveMinimumX;
    emit moveMinimumXChanged();
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
