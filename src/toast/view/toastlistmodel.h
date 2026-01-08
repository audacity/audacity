/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QVariant>
#include <QByteArray>
#include <QHash>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "toast/itoastprovider.h"

namespace au::toast {
class ToastListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::toast::IToastProvider> toastProvider;

public:
    explicit ToastListModel(QObject* parent = nullptr);
    ~ToastListModel() override = default;

    Q_INVOKABLE void init();
    Q_INVOKABLE void dismissToast(int id);
    Q_INVOKABLE void executeAction(int id, QString actionStr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

private:
    enum RoleNames {
        IdRole,
        IconCodeRole,
        TitleRole,
        MessageRole,
        ActionRole,
        DismissableRole,
        ProgressRole
    };

    std::vector<std::shared_ptr<ToastItem> > m_toasts;
};
}
