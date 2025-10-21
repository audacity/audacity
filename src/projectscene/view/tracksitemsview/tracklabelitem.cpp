/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelitem.h"

using namespace au::projectscene;

TrackLabelItem::TrackLabelItem(QObject* parent)
    : QObject(parent)
{
}

void TrackLabelItem::setLabel(const trackedit::Label& label)
{
    m_label = label;

    emit titleChanged();
    emit colorChanged();
}

const au::trackedit::Label& TrackLabelItem::label() const
{
    return m_label;
}

LabelKey TrackLabelItem::key() const
{
    return LabelKey(m_label.key);
}

QString TrackLabelItem::title() const
{
    return m_label.title;
}

void TrackLabelItem::setTitle(const QString& newTitle)
{
    m_label.title = newTitle;
    emit titleChanged();
}

QColor TrackLabelItem::color() const
{
    return m_label.color.toQColor();
}

double TrackLabelItem::x() const
{
    return m_x;
}

void TrackLabelItem::setX(double newX)
{
    if (qFuzzyCompare(m_x, newX)) {
        return;
    }

    m_x = newX;
    emit xChanged();
}

double TrackLabelItem::width() const
{
    return m_width;
}

void TrackLabelItem::setWidth(double newWidth)
{
    if (qFuzzyCompare(m_width, newWidth)) {
        return;
    }

    m_width = newWidth;
    emit widthChanged();
}

bool TrackLabelItem::selected() const
{
    return m_selected;
}

void TrackLabelItem::setSelected(bool newSelected)
{
    if (m_selected == newSelected) {
        return;
    }

    m_selected = newSelected;
    emit selectedChanged();
}

LabelTime TrackLabelItem::time() const
{
    return m_time;
}

void TrackLabelItem::setTime(const LabelTime& newTime)
{
    if (m_time == newTime) {
        return;
    }

    m_time = newTime;
    emit timeChanged();
}

double TrackLabelItem::leftVisibleMargin() const
{
    return m_leftVisibleMargin;
}

void TrackLabelItem::setLeftVisibleMargin(double newLeftVisibleMargin)
{
    if (qFuzzyCompare(m_leftVisibleMargin, newLeftVisibleMargin)) {
        return;
    }

    m_leftVisibleMargin = newLeftVisibleMargin;
    emit leftVisibleMarginChanged();
}

double TrackLabelItem::rightVisibleMargin() const
{
    return m_rightVisibleMargin;
}

void TrackLabelItem::setRightVisibleMargin(double newRightVisibleMargin)
{
    if (qFuzzyCompare(m_rightVisibleMargin, newRightVisibleMargin)) {
        return;
    }

    m_rightVisibleMargin = newRightVisibleMargin;
    emit rightVisibleMarginChanged();
}
