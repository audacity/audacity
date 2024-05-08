/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarabstractitem.h"

using namespace au::playback;
using namespace muse::uicomponents;

PlaybackToolBarAbstractItem::PlaybackToolBarAbstractItem(const muse::ui::UiAction& action, const ItemType& type, QObject* parent)
    : muse::uicomponents::MenuItem(action, parent), m_type(type)
{
}

PlaybackToolBarAbstractItem::ItemType PlaybackToolBarAbstractItem::type() const
{
    return m_type;
}

void PlaybackToolBarAbstractItem::setType(ItemType type)
{
    if (m_type == type) {
        return;
    }

    m_type = type;
    emit typeChanged();
}

int PlaybackToolBarAbstractItem::type_property() const
{
    return static_cast<int>(m_type);
}
