/*
* Audacity: A Digital Audio Editor
*/

#include <QCoreApplication>

#include "progressdialog.h"
#include "wxtypes_convert.h"

ProgressDialog::ProgressDialog(const muse::modularity::ContextPtr& ctx, const std::string& title)
    : muse::Injectable(ctx), m_progressTitle{title}
{
    // Of course, the least number of increments to yield a smooth animation depends on the width of the progress bar,
    // yet 300 increments should be enough to provide a smooth animation in most cases.
    m_progress.setMaxNumIncrements(200);
}

ProgressDialog::ProgressDialog(const muse::modularity::ContextPtr& ctx, const TranslatableString& title)
    : ProgressDialog{ctx, au::au3::wxToStdString(title.Translation())}
{
}

ProgressDialog::~ProgressDialog()
{
    m_progress.finish(muse::make_ok());
}

void ProgressDialog::Reinit()
{
}

void ProgressDialog::SetDialogTitle(const TranslatableString& title)
{
    m_progressTitle = au::au3::wxToStdString(title.Translation());
}

ProgressResult ProgressDialog::Poll(unsigned long long numerator, unsigned long long denominator, const TranslatableString& message)
{
    if (!m_progress.isStarted()) {
        interactive()->showProgress(m_progressTitle, m_progress);

        m_progress.canceled().onNotify(this, [this]() {
            m_cancelled = true;
        });

        m_progress.start();
    }

    if (!message.empty()) {
        m_progressMessage = au::au3::wxToStdString(message.Translation());
    }

    if (m_progress.progress(numerator, denominator, m_progressMessage)) {
        QCoreApplication::processEvents();
    }

    if (m_cancelled) {
        return ProgressResult::Cancelled;
    }
    return ProgressResult::Success;
}

void ProgressDialog::SetMessage(const TranslatableString& message)
{
    m_progressMessage = au::au3::wxToStdString(message.Translation());
}
