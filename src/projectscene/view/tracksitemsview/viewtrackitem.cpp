/*
* Audacity: A Digital Audio Editor
*/
#include <realfn.h>
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

    bool isStartTimeChanged = !muse::RealIsEqual(m_time.startTime, newTime.startTime);
    bool isEndTimeChanged = !muse::RealIsEqual(m_time.endTime, newTime.endTime);

    m_time = newTime;

    emit timeChanged();

    if (isStartTimeChanged) {
        emit startTimeChanged();
    }

    if (isEndTimeChanged) {
        emit endTimeChanged();
    }
}

bool ViewTrackItem::focused() const
{
    return m_focused;
}

void ViewTrackItem::setFocused(bool focused)
{
    if (m_focused == focused) {
        return;
    }

    m_focused = focused;
    emit focusedChanged();
}
