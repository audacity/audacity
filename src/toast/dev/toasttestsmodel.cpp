/*
* Audacity: A Digital Audio Editor
*/
#include "toasttestsmodel.h"

#include "log.h"
#include "toast/toasttypes.h"

using namespace au::toast;
using namespace muse;

ToastTestsModel::ToastTestsModel(QObject* parent)
    : QObject(parent), Injectable(muse::iocCtxForQmlObject(this))
{
}

void ToastTestsModel::showToastWithTimeout(const QString& title, const QString& message, int iconCode, int timeoutSeconds, bool dismissible)
{
    toastService()->showWithTimeout(
        title.toStdString(),
        message.toStdString(),
        std::chrono::seconds(timeoutSeconds),
        static_cast<muse::ui::IconCode::Code>(iconCode),
        dismissible
        );
}

void ToastTestsModel::showSuccess(const QString& title, const QString& message)
{
    toastService()->showSuccess(title.toStdString(), message.toStdString());
}

void ToastTestsModel::showError(const QString& title, const QString& message)
{
    toastService()->showError(title.toStdString(), message.toStdString());
}

void ToastTestsModel::showInfo(const QString& title, const QString& message)
{
    toastService()->showInfo(title.toStdString(), message.toStdString());
}

void ToastTestsModel::showWarning(const QString& title, const QString& message)
{
    toastService()->showWarning(title.toStdString(), message.toStdString());
}

void ToastTestsModel::showToastWithAction(const QString& title, const QString& message, int iconCode)
{
    toastService()->show(
        title.toStdString(),
        message.toStdString(),
        static_cast<muse::ui::IconCode::Code>(iconCode),
        true,
    {
        { "Open info dialog", ToastActionCode::Action1 },
    }).onResolve(this, [this](ToastActionCode actionCode) {
        if (actionCode == ToastActionCode::Action1) {
            interactive()->info("Info Dialog", "Info dialog action triggered from toast");
        }
    });
}

void ToastTestsModel::showWithProgress(const QString& title, const QString& message, int iconCode, bool dismissible)
{
    if (m_progress) {
        //Just to make things easy on testing
        m_progress->progress(100, 100, "Completed old process");
    }

    m_progress = std::make_shared<muse::Progress>();

    toastService()->showWithProgress(
        title.toStdString(),
        message.toStdString(),
        m_progress,
        static_cast<muse::ui::IconCode::Code>(iconCode),
        dismissible
        );

    m_progress->start();
}

void ToastTestsModel::updateProgress(int progress)
{
    if (m_progress) {
        m_progress->progress(progress, 100, "Processing...");
    }
}
