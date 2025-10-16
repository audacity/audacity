/*
* Audacity: A Digital Audio Editor
*/
#include "labellistitem.h"

using namespace au::projectscene;

LabelListItem::LabelListItem(QObject* parent)
    : QObject(parent)
{
}

void LabelListItem::setLabel(const trackedit::Label& label)
{
    m_label = label;

    emit titleChanged();
    emit colorChanged();
}

const au::trackedit::Label& LabelListItem::label() const
{
    return m_label;
}

LabelKey LabelListItem::key() const
{
    return LabelKey(m_label.key);
}

QString LabelListItem::title() const
{
    return m_label.title;
}

void LabelListItem::setTitle(const QString& newTitle)
{
    m_label.title = newTitle;
    emit titleChanged();
}

QColor LabelListItem::color() const
{
    return m_label.color.toQColor();
}

double LabelListItem::x() const
{
    return m_x;
}

void LabelListItem::setX(double newX)
{
    if (qFuzzyCompare(m_x, newX)) {
        return;
    }

    m_x = newX;
    emit xChanged();
}

double LabelListItem::width() const
{
    return m_width;
}

void LabelListItem::setWidth(double newWidth)
{
    if (qFuzzyCompare(m_width, newWidth)) {
        return;
    }

    m_width = newWidth;
    emit widthChanged();
}

bool LabelListItem::selected() const
{
    return m_selected;
}

void LabelListItem::setSelected(bool newSelected)
{
    if (m_selected == newSelected) {
        return;
    }

    m_selected = newSelected;
    emit selectedChanged();
}

LabelTime LabelListItem::time() const
{
    return m_time;
}

void LabelListItem::setTime(const LabelTime& newTime)
{
    if (m_time == newTime) {
        return;
    }

    m_time = newTime;
    emit timeChanged();
}

double LabelListItem::leftVisibleMargin() const
{
    return m_leftVisibleMargin;
}

void LabelListItem::setLeftVisibleMargin(double newLeftVisibleMargin)
{
    if (qFuzzyCompare(m_leftVisibleMargin, newLeftVisibleMargin)) {
        return;
    }

    m_leftVisibleMargin = newLeftVisibleMargin;
    emit leftVisibleMarginChanged();
}

double LabelListItem::rightVisibleMargin() const
{
    return m_rightVisibleMargin;
}

void LabelListItem::setRightVisibleMargin(double newRightVisibleMargin)
{
    if (qFuzzyCompare(m_rightVisibleMargin, newRightVisibleMargin)) {
        return;
    }

    m_rightVisibleMargin = newRightVisibleMargin;
    emit rightVisibleMarginChanged();
}
