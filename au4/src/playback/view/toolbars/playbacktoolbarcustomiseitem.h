/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PLAYBACKTOOLBARCUSTOMISEITEM_H
#define AU_PROJECTSCENE_PLAYBACKTOOLBARCUSTOMISEITEM_H

#include <QString>

#include "ui/view/iconcodes.h"
#include "uicomponents/view/selectableitemlistmodel.h"

namespace au::playback {
class PlaybackToolBarCustomiseItem : public muse::uicomponents::SelectableItemListModel::Item
{
    Q_OBJECT

    Q_PROPERTY(ItemType type READ type CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(int icon READ icon NOTIFY iconChanged)
    Q_PROPERTY(bool checked READ checked WRITE setChecked NOTIFY checkedChanged)

public:
    enum ItemType {
        UNDEFINED = -1,
        ACTION,
        SEPARATOR
    };
    Q_ENUM(ItemType)

    explicit PlaybackToolBarCustomiseItem(const ItemType& type, QObject* parent = nullptr);

    ItemType type() const;
    QString title() const;
    int icon() const;
    bool checked() const;

    Q_INVOKABLE QString id() const;
    void setId(const QString& id);

public slots:
    void setTitle(QString title);
    void setIcon(muse::ui::IconCode::Code icon);
    void setChecked(bool checked);

signals:
    void titleChanged();
    void iconChanged();
    void checkedChanged(bool checked);

private:

    QString m_id;
    ItemType m_type = ItemType::UNDEFINED;
    QString m_title;
    muse::ui::IconCode::Code m_icon = muse::ui::IconCode::Code::NONE;
    bool m_checked = false;
};
}

#endif // AU_PROJECTSCENE_PLAYBACKTOOLBARCUSTOMISEITEM_H
