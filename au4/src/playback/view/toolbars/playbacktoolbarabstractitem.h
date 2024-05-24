/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKTOOLBARABSTRACTITEM_H
#define AU_PLAYBACK_PLAYBACKTOOLBARABSTRACTITEM_H

#include <QString>

#include "uicomponents/view/menuitem.h"

namespace au::playback {
class PlaybackToolBarAbstractItem : public muse::uicomponents::MenuItem
{
    Q_OBJECT

    Q_PROPERTY(int type READ type_property NOTIFY typeChanged FINAL)

public:
    enum ItemType {
        UNDEFINED = -1,
        SECTION,
        ACTION,
        PLAYBACK_LEVEL,
        RECORD_LEVEL
    };
    Q_ENUM(ItemType)

    explicit PlaybackToolBarAbstractItem(const muse::ui::UiAction& action, const ItemType& type, QObject* parent = nullptr);

    ItemType type() const;
    void setType(ItemType type);

signals:
    void typeChanged();

private:
    int type_property() const;

    ItemType m_type = ItemType::UNDEFINED;
};
}

#endif // AU_PLAYBACK_PLAYBACKTOOLBARABSTRACTITEM_H
