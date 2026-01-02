/*
* Audacity: A Digital Audio Editor
*/
#pragma once
#include <QAbstractListModel>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "framework/actions/iactionsdispatcher.h"

namespace {
constexpr int DEFAULT_MAX_ITEMS = 5;
}

namespace au::toastnotification {
class ToastNotificationListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

    Q_PROPERTY(int maxItems READ maxItems WRITE setMaxItems)

public:
    explicit ToastNotificationListModel(QObject* parent = nullptr);
    ~ToastNotificationListModel() override = default;

    Q_INVOKABLE void init();
    Q_INVOKABLE void dismissNotification(int id);
    Q_INVOKABLE void executeAction(int notificationId, QString actionStr);

    int maxItems() const;
    void setMaxItems(int maxItems);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

private:
    enum RoleNames {
        IdRole = Qt::UserRole + 1,
        IconCodeRole,
        TitleRole,
        MessageRole,
        ActionsRole,
        DismissableRole,
        AutoDismissTimeoutRole
    };

    std::vector<ToastNotificationItem> m_notifications;
    int m_maxItems = DEFAULT_MAX_ITEMS;
};
}
