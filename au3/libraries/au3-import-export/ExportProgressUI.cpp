/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressUI.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "ExportProgressUI.h"

#include "Export.h"
#include "ExportPlugin.h"
#include "au3-strings/Internat.h"
#include "au3-basic-ui/BasicUI.h"
#include "au3-files/FileException.h"
#include "au3-utility/MemoryX.h"

#include <mutex>
#include <thread>

namespace {
class DialogExportProgressDelegate : public ExportProcessorDelegate
{
    std::atomic<bool> mCancelled { false };
    std::atomic<bool> mStopped { false };
    std::atomic<double> mProgress {};

    std::mutex mStatusMutex;
    TranslatableString mStatus;

    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
public:

    bool IsCancelled() const override
    {
        return mCancelled;
    }

    bool IsStopped() const override
    {
        return mStopped;
    }

    void SetStatusString(const TranslatableString& str) override
    {
        const std::lock_guard lock(mStatusMutex);
        mStatus = str;
    }

    void OnProgress(double progress) override
    {
        mProgress = progress;
    }

    void UpdateUI()
    {
        constexpr long long ProgressSteps = 1000ul;

        TranslatableString status;
        {
            const std::lock_guard lock(mStatusMutex);
            status = mStatus;
        }
        if (!mProgressDialog) {
            mProgressDialog = BasicUI::MakeProgress(TranslatableString("import-export", "Export"), status);
        } else {
            mProgressDialog->SetMessage(status);
        }

        const auto result = mProgressDialog->Poll(mProgress * ProgressSteps, ProgressSteps);

        if (result == BasicUI::ProgressResult::Cancelled) {
            if (!mStopped) {
                mCancelled = true;
            }
        } else if (result == BasicUI::ProgressResult::Stopped) {
            if (!mCancelled) {
                mStopped = true;
            }
        }
    }
};
}

ExportResult ExportProgressUI::Show(ExportTask exportTask, bool honorLateCancellation)
{
    assert(exportTask.valid());

    auto f = exportTask.get_future();
    DialogExportProgressDelegate delegate;
    std::thread worker(std::move(exportTask), std::ref(delegate));
    const auto joinWorker = finally([&] { worker.join(); });
    auto result = ExportResult::Error;
    while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
        delegate.UpdateUI();
    }

    bool resultReceived = false;
    ExceptionWrappedCall([&] {
        result = f.get();
        resultReceived = true;
    });
    if (honorLateCancellation && result == ExportResult::Success) {
        if (delegate.IsCancelled()) {
            return ExportResult::Cancelled;
        }
        if (delegate.IsStopped()) {
            return ExportResult::Stopped;
        }
    }

    if (resultReceived && result == ExportResult::Error) {
        BasicUI::ShowErrorDialog(
            {}, TranslatableString("import-export", "Export error"),
            TranslatableString("import-export", "Export completed with error."), {},
            BasicUI::ErrorDialogOptions { BasicUI::ErrorDialogType::ModalError });
    }

    return result;
}
