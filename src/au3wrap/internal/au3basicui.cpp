/*
* Audacity: A Digital Audio Editor
*/

#include "au3basicui.h"
#include "progressdialog.h"

#include "global/async/async.h"

void Au3BasicUI::DoCallAfter(const BasicUI::Action& action)
{
    muse::async::Async::call(this, action);
}

void Au3BasicUI::DoYield()
{
}

void Au3BasicUI::DoShowErrorDialog(const BasicUI::WindowPlacement& placement, const TranslatableString& dlogTitle,
                                   const TranslatableString& message, const ManualPageID& helpPage,
                                   const BasicUI::ErrorDialogOptions& options)
{
    Q_UNUSED(placement);
    Q_UNUSED(dlogTitle);
    Q_UNUSED(message);
    Q_UNUSED(helpPage);
    Q_UNUSED(options);
}

BasicUI::MessageBoxResult Au3BasicUI::DoMessageBox(const TranslatableString& message, BasicUI::MessageBoxOptions options)
{
    Q_UNUSED(message);
    Q_UNUSED(options);
    return BasicUI::MessageBoxResult::None;
}

std::unique_ptr<BasicUI::ProgressDialog> Au3BasicUI::DoMakeProgress(const TranslatableString& title, const TranslatableString& message,
                                                                    unsigned int flags, const TranslatableString& remainingLabelText)
{
    Q_UNUSED(title);
    Q_UNUSED(message);
    Q_UNUSED(flags);
    Q_UNUSED(remainingLabelText);
    return std::make_unique<ProgressDialog>();
}

namespace {
struct MyGenericProgress : BasicUI::GenericProgressDialog {
    MyGenericProgress()
    {}
    ~MyGenericProgress() override = default;
    BasicUI::ProgressResult Pulse() override
    {
        return BasicUI::ProgressResult::Stopped;
    }
};
}

std::unique_ptr<BasicUI::GenericProgressDialog> Au3BasicUI::DoMakeGenericProgress(const BasicUI::WindowPlacement& placement,
                                                                                  const TranslatableString& title,
                                                                                  const TranslatableString& message, int style)
{
    Q_UNUSED(placement);
    Q_UNUSED(title);
    Q_UNUSED(message);
    Q_UNUSED(style);
    return std::make_unique<MyGenericProgress>();
}

int Au3BasicUI::DoMultiDialog(const TranslatableString& message, const TranslatableString& title, const TranslatableStrings& buttons,
                              const ManualPageID& helpPage, const TranslatableString& boxMsg, bool log)
{
    Q_UNUSED(message);
    Q_UNUSED(buttons);
    Q_UNUSED(helpPage);
    Q_UNUSED(boxMsg);
    Q_UNUSED(log);
    Q_UNUSED(title);
    return -1;
}

bool Au3BasicUI::DoOpenInDefaultBrowser(const wxString& url)
{
    Q_UNUSED(url);
    return false;
}

std::unique_ptr<BasicUI::WindowPlacement> Au3BasicUI::DoFindFocus()
{
    return nullptr;
}

void Au3BasicUI::DoSetFocus(const BasicUI::WindowPlacement& focus)
{
    Q_UNUSED(focus);
}

bool Au3BasicUI::IsUsingRtlLayout() const
{
    return false;
}

bool Au3BasicUI::IsUiThread() const
{
    return false;
}
