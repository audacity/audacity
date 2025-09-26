/*
* Audacity: A Digital Audio Editor
*/

#include <QCoreApplication>

#include "progressdialog.h"

ProgressDialog::ProgressDialog()
{
    // Of course, the least number of increments to yield a smooth animation depends on the width of the progress bar,
    // yet 300 increments should be enough to provide a smooth animation in most cases.
    m_progress.setMaxNumIncrements(200);
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
    if (!m_progress.isStarted()) {
        interactive()->showProgress(std::string(), m_progress);

        m_progress.canceled().onNotify(this, [this]() {
            m_cancelled = true;
        });

        m_progress.start();
    }

    if (m_progress.progress(numerator, denominator, message.Translation().ToStdString())) {
        QCoreApplication::processEvents();
    }

    if (m_cancelled) {
        return ProgressResult::Cancelled;
    }
    return ProgressResult::Success;
}

void ProgressDialog::SetMessage(const TranslatableString& message)
{
    Q_UNUSED(message);
}
