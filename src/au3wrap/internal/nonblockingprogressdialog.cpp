/*
 * Audacity: A Digital Audio Editor
 */
#include "nonblockingprogressdialog.h"

#include "iinteractive.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

#include <QCoreApplication>

#include <thread>
#include <condition_variable>

namespace au::effects {
namespace {
class DialogImpl : public muse::async::Asyncable
{
    muse::Inject<muse::IInteractive> interactive;

public:
    DialogImpl()
        : m_au3dialog{m_mutex, m_progressUpdate, m_canceled} {}

    void execute(std::function<void(BasicUI::ProgressDialog&, const bool& canceled)>);

private:
    class Au3Dialog : public BasicUI::ProgressDialog
    {
    public:
        Au3Dialog(std::mutex& mutex, std::condition_variable& progressUpdate, bool& canceled);
        ~Au3Dialog() override = default;

        void Reinit() override;

        void SetDialogTitle(const TranslatableString& title) override;

        BasicUI::ProgressResult Poll(unsigned long long numerator, unsigned long long denominator,
                                     const TranslatableString& message) override;

        void SetMessage(const TranslatableString& message) override;

        unsigned long long numerator() const;
        unsigned long long denominator() const;
        std::string message() const;

    private:
        std::mutex& m_mutex;
        std::condition_variable& m_progressUpdate;
        bool& m_canceled;

        unsigned long long m_numerator = 0;
        unsigned long long m_denominator = 0;
        std::string m_message;
    };

    std::mutex m_mutex;
    std::condition_variable m_progressUpdate;
    bool m_canceled = false;
    Au3Dialog m_au3dialog;
    std::thread m_thread;
    muse::Progress m_progress;
};

void DialogImpl::execute(std::function<void(BasicUI::ProgressDialog&, const bool& canceled)> fun)
{
    interactive()->showProgress(std::string(), &m_progress);

    m_progress.canceled().onNotify(this, [&]() {
        std::lock_guard<std::mutex> lock { m_mutex };
        m_canceled = true;
    });

    auto finished = false;
    auto wrapper = [&] {
        fun(m_au3dialog, m_canceled);
        finished = true;
    };

    m_progress.start();

    m_thread = std::thread{ std::move(wrapper) };
    while (!finished) {
        unsigned long long numerator = 0;
        unsigned long long denominator = 0;
        std::string message;
        {
            std::unique_lock<std::mutex> lock{ m_mutex };
            const auto timedOut = m_progressUpdate.wait_for(lock, std::chrono::milliseconds(100)) == std::cv_status::timeout;
            if (finished || m_canceled) {
                break;
            } else if (timedOut) {
                continue;
            }
            numerator = m_au3dialog.numerator();
            denominator = m_au3dialog.denominator();
            message = m_au3dialog.message();
        }
        m_progress.progress(numerator, denominator, message);
        QCoreApplication::processEvents();
    }
    m_thread.join();
    m_progress.finish(muse::make_ok());
}

DialogImpl::Au3Dialog::Au3Dialog(std::mutex& mutex, std::condition_variable& progressUpdate, bool& canceled)
    : m_mutex{mutex}, m_progressUpdate{progressUpdate}, m_canceled{canceled}
{
}

void DialogImpl::Au3Dialog::Reinit()
{
}

void DialogImpl::Au3Dialog::SetDialogTitle(const TranslatableString& title)
{
    Q_UNUSED(title);
}

BasicUI::ProgressResult DialogImpl::Au3Dialog::Poll(unsigned long long numerator, unsigned long long denominator,
                                                    const TranslatableString& message)
{
    auto result = BasicUI::ProgressResult::Success;
    {
        std::lock_guard<std::mutex> lock{ m_mutex };
        m_numerator = numerator;
        m_denominator = denominator;
        m_message = message.Translation().ToStdString();
        if (m_canceled) {
            result = BasicUI::ProgressResult::Cancelled;
        }
    }
    m_progressUpdate.notify_one();
    return result;
}

void DialogImpl::Au3Dialog::SetMessage(const TranslatableString& message)
{
    Q_UNUSED(message);
}

unsigned long long DialogImpl::Au3Dialog::numerator() const
{
    return m_numerator;
}

unsigned long long DialogImpl::Au3Dialog::denominator() const
{
    return m_denominator;
}

std::string DialogImpl::Au3Dialog::message() const
{
    return m_message;
}
} // namespace

void NonBlockingProgressDialog::execute(std::function<void(BasicUI::ProgressDialog&, const bool& canceled)> fun)
{
    DialogImpl progress;
    progress.execute(fun);
}
}
