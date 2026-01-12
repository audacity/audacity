/*
* Audacity: A Digital Audio Editor
*/
#include <iterator>

#include <QModelIndex>
#include <QString>

#include "toastlistmodel.h"
#include "toast/internal/toastitem.h"

using namespace au::toast;

namespace {
constexpr int MAX_VISIBLE_TOASTS = 5;
}

ToastListModel::ToastListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ToastListModel::init()
{
    toastProvider()->toastAdded().onReceive(this, [this](std::shared_ptr<ToastItem> toast) {
        if (m_toasts.size() >= MAX_VISIBLE_TOASTS) {
            toastProvider()->dismissToast(m_toasts.front()->id());
        }

        beginInsertRows(QModelIndex(), static_cast<int>(m_toasts.size()), static_cast<int>(m_toasts.size()));
        m_toasts.emplace_back(toast);
        endInsertRows();

        int id = toast->id();
        toast->progressChanged().onNotify(this, [this, id](){
            int toastIndex = -1;
            for (int i = 0; i < static_cast<int>(m_toasts.size()); ++i) {
                if (m_toasts.at(i)->id() == id) {
                    toastIndex = i;
                    break;
                }
            }

            if (toastIndex == -1) {
                return;
            }

            emit dataChanged(index(toastIndex), this->index(toastIndex), { ProgressRole });
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    toastProvider()->toastDismissed().onReceive(this, [this](int id) {
        int index = -1;
        for (int i = 0; i < static_cast<int>(m_toasts.size()); ++i) {
            if (m_toasts.at(i)->id() == id) {
                index = i;
                break;
            }
        }

        if (index == -1) {
            return;
        }

        beginRemoveRows(QModelIndex(), index, index);
        m_toasts.erase(m_toasts.begin() + index);
        endRemoveRows();
    }, muse::async::Asyncable::Mode::SetReplace);
}

void ToastListModel::dismissToast(int id)
{
    toastProvider()->dismissToast(id);
}

void ToastListModel::executeAction(int id, QString actionStr)
{
    for (int i = 0; i < static_cast<int>(m_toasts.size()); ++i) {
        if (m_toasts.at(i)->id() == id) {
            if (m_toasts.at(i)->actions().empty()) {
                return;
            }

            const auto action = m_toasts.at(i)->actions()
                                .at(std::distance(m_toasts.at(i)->actions().cbegin(),
                                                  std::find_if(m_toasts.at(i)->actions().cbegin(),
                                                               m_toasts.at(i)->actions().cend(),
                                                               [actionStr](const ToastAction& action) {
                return action.text == actionStr.toStdString();
            })));

            toastProvider()->executeAction(id, action.code);
            return;
        }
    }
}

int ToastListModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }

    return static_cast<int>(m_toasts.size());
}

QVariant ToastListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= static_cast<int>(m_toasts.size())) {
        return QVariant();
    }

    const auto toast = m_toasts.at(index.row());
    switch (role) {
    case IdRole:
        return toast->id();
    case IconCodeRole:
        return static_cast<int>(toast->iconCode());
    case TitleRole:
        return QString::fromStdString(toast->title());
    case MessageRole:
        return QString::fromStdString(toast->message());
    case DismissableRole:
        return toast->isDismissible();
    case ActionRole:
    {
        QVariantList actionsList;
        for (auto& action : toast->actions()) {
            QVariantMap actionMap;
            actionMap.insert("text", QString::fromStdString(action.text));
            actionsList.append(actionMap);
        }
        return actionsList;
    }
    case ProgressRole:
        return static_cast<int>(toast->currentProgress());
    default:
        return QVariant();
    }
}

QHash<int, QByteArray> ToastListModel::roleNames() const
{
    static QHash<int, QByteArray> roles {
        { IdRole, "id" },
        { IconCodeRole, "iconCode" },
        { TitleRole, "title" },
        { MessageRole, "message" },
        { DismissableRole, "dismissable" },
        { ActionRole, "actions" },
        { ProgressRole, "progress" }
    };
    return roles;
}
