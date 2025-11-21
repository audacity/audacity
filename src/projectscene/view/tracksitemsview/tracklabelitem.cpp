/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelitem.h"

using namespace au::projectscene;

TrackLabelItem::TrackLabelItem(QObject* parent)
    : ViewTrackItem(parent)
{
}

void TrackLabelItem::setLabel(const trackedit::Label& label)
{
    m_key = TrackItemKey(label.key);
    m_title = label.title;
    m_color = label.color.toQColor();

    emit titleChanged();
    emit colorChanged();
    emit timeChanged();
}

int TrackLabelItem::level() const
{
    return m_level;
}

void TrackLabelItem::setLevel(int level)
{
    if (m_level == level) {
        return;
    }

    m_level = level;
    emit levelChanged();
}

int TrackLabelItem::visualWidth() const
{
    return m_visualWidth;
}

void TrackLabelItem::setVisualWidth(int width)
{
    if (m_visualWidth == width) {
        return;
    }

    m_visualWidth = width;
    emit visualWidthChanged();
}

int TrackLabelItem::visualHeight() const
{
    return m_visualHeight;
}

void TrackLabelItem::setVisualHeight(int height)
{
    if (m_visualHeight == height) {
        return;
    }

    m_visualHeight = height;
    emit visualHeightChanged();
}

bool TrackLabelItem::isEditing() const
{
    return m_isEditing;
}

void TrackLabelItem::setIsEditing(bool editing)
{
    if (m_isEditing == editing) {
        return;
    }

    m_isEditing = editing;
    emit isEditingChanged();
}

bool TrackLabelItem::isLeftLinked() const
{
    return m_isLeftLinked;
}

void TrackLabelItem::setIsLeftLinked(bool linked)
{
    if (m_isLeftLinked == linked) {
        return;
    }

    m_isLeftLinked = linked;
    emit isLeftLinkedChanged();
}

bool TrackLabelItem::isRightLinked() const
{
    return m_isRightLinked;
}

void TrackLabelItem::setIsRightLinked(bool linked)
{
    if (m_isRightLinked == linked) {
        return;
    }

    m_isRightLinked = linked;
    emit isRightLinkedChanged();
}

bool TrackLabelItem::isLinkedActive() const
{
    return m_isLinkedActive;
}

void TrackLabelItem::setIsLinkedActive(bool active)
{
    if (m_isLinkedActive == active) {
        return;
    }

    m_isLinkedActive = active;
    emit isLinkedActiveChanged();
}
