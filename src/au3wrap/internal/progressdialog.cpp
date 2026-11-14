/*
* Audacity: A Digital Audio Editor
*/

#include <QCoreApplication>

#include "progressdialog.h"

namespace au::au3 {
ProgressDialog::ProgressDialog(const muse::modularity::ContextPtr& ctx, const std::string& title)
    : muse::Contextable(ctx), m_progressTitle{title}
{
    // Of course, the least number of increments to yield a smooth animation depends on the width of the progress bar,
    // yet 300 increments should be enough to provide a smooth animation in most cases.
    m_progress.setMaxNumIncrements(200);
}

ProgressDialog::ProgressDialog(const muse::modularity::ContextPtr& ctx, const ::TranslatableString& title)
    : ProgressDialog{ctx, title.translated().toStdString()}
{
}

ProgressDialog::~ProgressDialog()
{
    m_progress.finish(muse::make_ok());
}

void ProgressDialog::Reinit()
{
    m_cancelled = false;
    m_canceledHooked = false;
    m_lastEventPump = {};
}

void ProgressDialog::SetDialogTitle(const ::TranslatableString& title)
{
    m_progressTitle = title.translated().toStdString();
}

void ProgressDialog::start()
{
    if (m_progress.isStarted()) {
        return;
    }
    interactive()->showProgress(m_progressTitle, m_progress);

    if (!m_canceledHooked) {
        m_progress.canceled().onNotify(this, [this]() {
            m_cancelled = true;
        });
        m_canceledHooked = true;
    }

    m_progress.start();
}

ProgressResult ProgressDialog::Poll(unsigned long long numerator, unsigned long long denominator, const ::TranslatableString& message)
{
    if (m_cancelled) {
        return ProgressResult::Cancelled;
    }

    start();

    if (!message.empty()) {
        m_progressMessage = message.translated().toStdString();
    }

    // Push the new fraction/message into muse::Progress unconditionally.
    // The framework throttles the *visual* update internally;
    m_progress.progress(numerator, denominator, m_progressMessage);

    // Make sure that the user can press Cancel even if the progress bar
    // doesn't update itself
    using clock = std::chrono::steady_clock;
    constexpr auto pumpInterval = std::chrono::milliseconds(100);
    const auto now = clock::now();
    if (now - m_lastEventPump >= pumpInterval) {
        m_lastEventPump = now;
        QCoreApplication::processEvents(QEventLoop::AllEvents);
    }

    if (m_cancelled) {
        return ProgressResult::Cancelled;
    }

    return ProgressResult::Success;
}

void ProgressDialog::SetMessage(const ::TranslatableString& message)
{
    m_progressMessage = message.translated().toStdString();
}
}
