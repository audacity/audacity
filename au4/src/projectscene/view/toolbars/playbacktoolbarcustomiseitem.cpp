/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarcustomiseitem.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

PlaybackToolBarCustomiseItem::PlaybackToolBarCustomiseItem(const ItemType& type, QObject* parent)
    : SelectableItemListModel::Item(parent), m_type(type)
{
}

QString PlaybackToolBarCustomiseItem::id() const
{
    return m_id;
}

QString PlaybackToolBarCustomiseItem::title() const
{
    return m_title;
}

PlaybackToolBarCustomiseItem::ItemType PlaybackToolBarCustomiseItem::type() const
{
    return m_type;
}

void PlaybackToolBarCustomiseItem::setTitle(QString title)
{
    if (m_title == title) {
        return;
    }

    m_title = title;
    emit titleChanged();
}

void PlaybackToolBarCustomiseItem::setId(const QString& id)
{
    m_id = id;
}

int PlaybackToolBarCustomiseItem::icon() const
{
    return static_cast<int>(m_icon);
}

bool PlaybackToolBarCustomiseItem::checked() const
{
    return m_checked;
}

void PlaybackToolBarCustomiseItem::setIcon(muse::ui::IconCode::Code icon)
{
    if (m_icon == icon) {
        return;
    }

    m_icon = icon;
    emit iconChanged();
}

void PlaybackToolBarCustomiseItem::setChecked(bool checked)
{
    if (m_checked == checked) {
        return;
    }

    m_checked = checked;
    emit checkedChanged(m_checked);
}

QColor PlaybackToolBarCustomiseItem::iconColor() const
{
    return m_iconColor;
}

void PlaybackToolBarCustomiseItem::setIconColor(const QColor& color)
{
    if (m_iconColor == color) {
        return;
    }

    m_iconColor = color;
    emit iconColorChanged();
}
