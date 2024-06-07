/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarcontrolitem.h"

using namespace au::playback;

PlaybackToolBarControlItem::PlaybackToolBarControlItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                                       QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
}

QColor PlaybackToolBarControlItem::iconColor() const
{
    return m_iconColor;
}

void PlaybackToolBarControlItem::setIconColor(const QColor& color)
{
    if (m_iconColor == color) {
        return;
    }

    m_iconColor = color;
    emit iconColorChanged();
}

QColor PlaybackToolBarControlItem::backgroundColor() const
{
    return m_backgroundColor;
}

void PlaybackToolBarControlItem::setBackgroundColor(const QColor& color)
{
    if (m_backgroundColor == color) {
        return;
    }

    m_backgroundColor = color;
    emit backgroundColorChanged();
}
