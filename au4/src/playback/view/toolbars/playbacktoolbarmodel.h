/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
#define AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H

#include <QHash>
#include <QAbstractListModel>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"
#include "ui/iuiconfiguration.h"
#include "actions/iactionsdispatcher.h"
#include "playback/iplaybackcontroller.h"

namespace au::playback {
class PlaybackToolBarAbstractItem;
class PlaybackToolBarModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::ui::IUiActionsRegister> actionsRegister;
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> context;
    muse::Inject<au::playback::IPlaybackController> controller;

    Q_PROPERTY(int length READ rowCount NOTIFY itemsChanged)

public:
    explicit PlaybackToolBarModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void handleMenuItem(const QString& itemId);
    Q_INVOKABLE QVariantMap get(int index);

    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

signals:
    void itemsChanged();

private:
    enum InputRoles {
        ItemRole = Qt::UserRole + 1,
        IsMenuSecondaryRole,
        OrderRole,
        SectionRole,
    };

    PlaybackToolBarAbstractItem* findItem(const muse::actions::ActionCode& actionCode);

    void onActionsStateChanges(const muse::actions::ActionCodeList& codes);

    void setupConnections();
    void onProjectChanged();

    void updateActions();

    bool isMenuSecondary(const muse::actions::ActionCode& actionCode) const;

    PlaybackToolBarAbstractItem* makeItem(const muse::ui::UiAction& action, const QString& section);

    muse::ui::UiAction playAction() const;

    QList<PlaybackToolBarAbstractItem*> m_items;
};
}

#endif // AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
