/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressUI.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "ExportProgressUI.h"

#include "Export.h"
#include "ExportPlugin.h"
#include "Internat.h"
#include "BasicUI.h"
#include "FileException.h"

namespace {
class DialogExportProgressDelegate : public ExportProcessorDelegate
{
    std::atomic<bool> mCancelled { false };
    std::atomic<bool> mStopped { false };
    std::atomic<double> mProgress {};

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
        mStatus = str;
    }

    void OnProgress(double progress) override
    {
        mProgress = progress;
    }

    void UpdateUI()
    {
        constexpr long long ProgressSteps = 1000ul;

        if (!mProgressDialog) {
            mProgressDialog = BasicUI::MakeProgress(XO("Export"), mStatus);
        } else {
            mProgressDialog->SetMessage(mStatus);
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

ExportResult ExportProgressUI::Show(ExportTask exportTask)
{
    assert(exportTask.valid());

    auto f = exportTask.get_future();
    DialogExportProgressDelegate delegate;
    std::thread(std::move(exportTask), std::ref(delegate)).detach();
    auto result = ExportResult::Error;
    while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
        delegate.UpdateUI();
    }

    ExceptionWrappedCall([&] { result = f.get(); });

    if (result == ExportResult::Error) {
        BasicUI::ShowErrorDialog(
            {}, XO("Export error"),
            XO("Export completed with error."), {},
            BasicUI::ErrorDialogOptions { BasicUI::ErrorDialogType::ModalError });
    }

    return result;
}
