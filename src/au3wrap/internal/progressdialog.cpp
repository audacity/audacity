/*
* Audacity: A Digital Audio Editor
*/

#include <QCoreApplication>

#include "progressdialog.h"

ProgressDialog::ProgressDialog()
{
    muse::Ret ret = interactive()->showProgress(std::string(), &m_progress);
    m_progress.started();

    m_progress.canceled().onNotify(this, [this]() {
        m_cancelled = true;
    });
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
    Q_UNUSED(title);
}

ProgressResult ProgressDialog::Poll(unsigned long long numerator, unsigned long long denominator, const TranslatableString& message)
{
    m_progress.progress(numerator, denominator, message.Translation().ToStdString());
    QCoreApplication::processEvents();

    if (m_cancelled) {
        return ProgressResult::Cancelled;
    }
    return ProgressResult::Success;
}

void ProgressDialog::SetMessage(const TranslatableString& message)
{
    Q_UNUSED(message);
}
