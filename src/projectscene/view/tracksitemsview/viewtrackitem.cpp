/*
* Audacity: A Digital Audio Editor
*/
#include "viewtrackitem.h"

using namespace au::projectscene;

ViewTrackItem::ViewTrackItem(QObject* parent)
    : QObject(parent)
{
}

ViewTrackItem::~ViewTrackItem()
{
}

TrackItemKey ViewTrackItem::key() const
{
    return m_key;
}

QString ViewTrackItem::title() const
{
    return m_title;
}

void ViewTrackItem::setTitle(const QString& newTitle)
{
    if (m_title == newTitle) {
        return;
    }
    m_title = newTitle;
    emit titleChanged();
}

QColor ViewTrackItem::color() const
{
    return m_color;
}

double ViewTrackItem::x() const
{
    return m_x;
}

void ViewTrackItem::setX(double newX)
{
    if (qFuzzyCompare(m_x, newX)) {
        return;
    }
    m_x = newX;
    emit xChanged();
}

double ViewTrackItem::width() const
{
    return m_width;
}

void ViewTrackItem::setWidth(double newWidth)
{
    if (qFuzzyCompare(m_width, newWidth)) {
        return;
    }
    m_width = newWidth;
    emit widthChanged();
}

bool ViewTrackItem::selected() const
{
    return m_selected;
}

void ViewTrackItem::setSelected(bool newSelected)
{
    if (m_selected == newSelected) {
        return;
    }
    m_selected = newSelected;
    emit selectedChanged();
}

bool ViewTrackItem::intersectsSelection() const
{
    return m_intersectsSelection;
}

void ViewTrackItem::setIntersectsSelection(bool newState)
{
    if (m_intersectsSelection == newState) {
        return;
    }
    m_intersectsSelection = newState;
    emit intersectsSelectionChanged();
}

double ViewTrackItem::leftVisibleMargin() const
{
    return m_leftVisibleMargin;
}

void ViewTrackItem::setLeftVisibleMargin(double newLeftVisibleMargin)
{
    if (qFuzzyCompare(m_leftVisibleMargin, newLeftVisibleMargin)) {
        return;
    }
    m_leftVisibleMargin = newLeftVisibleMargin;
    emit leftVisibleMarginChanged();
}

double ViewTrackItem::rightVisibleMargin() const
{
    return m_rightVisibleMargin;
}

void ViewTrackItem::setRightVisibleMargin(double newRightVisibleMargin)
{
    if (qFuzzyCompare(m_rightVisibleMargin, newRightVisibleMargin)) {
        return;
    }
    m_rightVisibleMargin = newRightVisibleMargin;
    emit rightVisibleMarginChanged();
}

TrackItemTime ViewTrackItem::time() const
{
    return m_time;
}

void ViewTrackItem::setTime(const TrackItemTime& newTime)
{
    if (m_time == newTime) {
        return;
    }
    m_time = newTime;
    emit timeChanged();
}
