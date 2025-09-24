/*
 * Audacity: A Digital Audio Editor
 */
#include "au3wraputils.h"

#include "iinteractive.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

#include "libraries/lib-exceptions/AudacityException.h"

#include <QEventLoop>

#include <thread>

namespace au::au3 {
namespace {
class DialogImpl : public muse::async::Asyncable
{
    muse::Inject<muse::IInteractive> interactive;

public:
    muse::Ret executeInBackground(Task task, std::string title, muse::Ret::Code errorCode)
    {
        QEventLoop loop;
        muse::Ret ret;

        m_museProgress.finished().onReceive(this, [&loop, &ret](const muse::RetVal<muse::Val>& retval) {
            ret = retval.ret;
            loop.quit();
        });

        // Make sure that the progress is started before the thread starts being executed.
        interactive()->showProgress(title, &m_museProgress);

        std::thread thread([&] {
            DialogImpl::Au3Progress au3Progress { m_museProgress };

            try {
                if (!task(au3Progress)) {
                    m_museProgress.finished().send(muse::make_ret(errorCode));
                } else if (!m_museProgress.isCanceled()) {
                    m_museProgress.finished().send(muse::make_ok());
                }
            } catch (::AudacityException& e) {
                std::string message;
                if (const auto box = dynamic_cast<::MessageBoxException*>(&e)) {
                    message = box->ErrorMessage().Translation().ToStdString();
                }
                m_museProgress.finished().send(muse::make_ret(errorCode, message));
            } catch (const std::exception& e) {
                m_museProgress.finished().send(muse::make_ret(errorCode, std::string { e.what() }));
            } catch (...) {
                m_museProgress.finished().send(muse::make_ret(errorCode));
            }
        });

        loop.exec();
        thread.join();

        return ret;
    }

private:
    class Au3Progress : public BasicUI::ProgressDialog
    {
    public:
        Au3Progress(muse::Progress progress)
            : m_progress{progress} {}
        ~Au3Progress() override = default;

        void Reinit() override {}
        void SetDialogTitle(const TranslatableString&) override {}
        void SetMessage(const TranslatableString&) override {}

        BasicUI::ProgressResult Poll(unsigned long long numerator, unsigned long long denominator,
                                     const TranslatableString& message) override
        {
            if (m_progress.isCanceled()) {
                return BasicUI::ProgressResult::Cancelled;
            }
            m_progress.progress(numerator, denominator, message.Translation().ToStdString());
            return BasicUI::ProgressResult::Success;
        }

    private:
        muse::Progress m_progress;
    };

private:
    muse::Progress m_museProgress;
};
} // namespace
}

muse::Ret au::au3::executeInBackground(Task task, std::string title, muse::Ret::Code errorCode)
{
    DialogImpl impl;
    return impl.executeInBackground(task, std::move(title), errorCode);
}
