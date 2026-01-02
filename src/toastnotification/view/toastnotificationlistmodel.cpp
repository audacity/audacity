/*
* Audacity: A Digital Audio Editor
*/
#include <cstddef>
#include <iterator>
#include <algorithm>

#include <QModelIndex>
#include <QString>

#include "toastnotificationlistmodel.h"

using namespace au::toastnotification;

ToastNotificationListModel::ToastNotificationListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ToastNotificationListModel::init()
{
    const auto currentToastNotification = globalContext()->currentToastNotification();
    IF_ASSERT_FAILED(currentToastNotification) {
        return;
    }

    currentToastNotification->notificationAdded().onReceive(this, [this](const ToastNotificationItem& notification) {
        if (m_notifications.size() >= m_maxItems) {
            beginRemoveRows(QModelIndex(), 0, 0);
            m_notifications.erase(m_notifications.begin());
            endRemoveRows();
        }

        beginInsertRows(QModelIndex(), static_cast<int>(m_notifications.size()), static_cast<int>(m_notifications.size()));
        m_notifications.emplace_back(notification);
        endInsertRows();
    }, muse::async::Asyncable::Mode::SetReplace);

    currentToastNotification->notificationDismissed().onReceive(this, [this](int id) {
        int index = -1;
        for (int i = 0; i < static_cast<int>(m_notifications.size()); ++i) {
            if (m_notifications.at(i).id == id) {
                index = i;
                break;
            }
        }

        if (index == -1) {
            return;
        }

        beginRemoveRows(QModelIndex(), index, index);
        m_notifications.erase(m_notifications.begin() + index);
        endRemoveRows();
    }, muse::async::Asyncable::Mode::SetReplace);
}

void ToastNotificationListModel::dismissNotification(int id)
{
    muse::actions::ActionQuery q("action://toastnotification/dismiss-item");
    q.addParam("id", muse::Val(id));
    dispatcher()->dispatch(q);
}

void ToastNotificationListModel::executeAction(int notificationId, QString actionStr)
{
    int index = -1;
    for (int i = 0; i < static_cast<int>(m_notifications.size()); ++i) {
        if (m_notifications.at(i).id == notificationId) {
            index = i;
            break;
        }
    }

    if (index == -1) {
        return;
    }

    if (m_notifications.at(index).actions.empty()) {
        return;
    }

    m_notifications.at(index).actions
    .at(std::distance(m_notifications.at(index).actions.cbegin(),
                      std::find_if(m_notifications.at(index).actions.cbegin(),
                                   m_notifications.at(index).actions.cend(),
                                   [actionStr](const ToastNotificationAction& action) {
        return QString::fromStdString(action.text) == actionStr;
    })))
    .callback();
}

int ToastNotificationListModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }
    return static_cast<int>(m_notifications.size());
}

QVariant ToastNotificationListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= static_cast<int>(m_notifications.size())) {
        return QVariant();
    }

    const ToastNotificationItem& notification = m_notifications.at(index.row());

    switch (role) {
    case IdRole:
        return notification.id;
    case IconCodeRole:
        return notification.iconCode;
    case TitleRole:
        return QString::fromStdString(notification.title);
    case MessageRole:
        return QString::fromStdString(notification.message);
    case ActionsRole:
    {
        QVariantList actionsList;
        for (const auto& action : notification.actions) {
            QVariantMap actionMap;
            actionMap.insert("text", QString::fromStdString(action.text));
            actionsList.append(actionMap);
        }
        return actionsList;
    }
    case DismissableRole:
        return notification.dismissable;
    case AutoDismissTimeoutRole:
        return notification.autoDismissTimeout.has_value() ? notification.autoDismissTimeout.value() : 0;
    default:
        return QVariant();
    }
}

QHash<int, QByteArray> ToastNotificationListModel::roleNames() const
{
    static QHash<int, QByteArray> roles {
        { IdRole, "id" },
        { IconCodeRole, "iconCode" },
        { TitleRole, "title" },
        { MessageRole, "message" },
        { ActionsRole, "actions" },
        { DismissableRole, "dismissable" },
        { AutoDismissTimeoutRole, "autoDismissTimeout" }
    };
    return roles;
}

int ToastNotificationListModel::maxItems() const
{
    return m_maxItems;
}

void ToastNotificationListModel::setMaxItems(int maxItems)
{
    if (maxItems < 0) {
        return;
    }

    m_maxItems = maxItems;
}
