/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "toast/itoastservice.h"

namespace au::toast {
class ToastTestsModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IToastService> toastService = { this };
    muse::Inject<muse::IInteractive> interactive = { this };

public:
    explicit ToastTestsModel(QObject* parent = nullptr);

    Q_INVOKABLE void showSuccess(const QString& title, const QString& message);
    Q_INVOKABLE void showError(const QString& title, const QString& message);
    Q_INVOKABLE void showInfo(const QString& title, const QString& message);
    Q_INVOKABLE void showWarning(const QString& title, const QString& message);

    Q_INVOKABLE void showToastWithAction(const QString& title, const QString& message, int iconCode);
    Q_INVOKABLE void showToastWithTimeout(const QString& title, const QString& message, int iconCode, int timeout, bool dismissible);
    Q_INVOKABLE void showWithProgress(const QString& title, const QString& message, int iconCode, bool dismissible, bool showProgressInfo);
    Q_INVOKABLE void updateProgress(int progressValue);

private:
    std::shared_ptr<muse::Progress> m_progress;
};
}
