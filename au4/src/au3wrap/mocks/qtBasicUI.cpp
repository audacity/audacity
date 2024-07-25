/*
* Audacity: A Digital Audio Editor
*/

#include "mocks/qtBasicUI.h"
#include "mocks/progressdialog.h"

QtBasicUI::~QtBasicUI()
{
}

void QtBasicUI::DoCallAfter(const BasicUI::Action& action)
{
    Q_UNUSED(action);
}

void QtBasicUI::DoYield()
{
}

void QtBasicUI::DoShowErrorDialog(const BasicUI::WindowPlacement& placement, const TranslatableString& dlogTitle,
                                  const TranslatableString& message, const ManualPageID& helpPage,
                                  const BasicUI::ErrorDialogOptions& options)
{
    Q_UNUSED(placement);
    Q_UNUSED(dlogTitle);
    Q_UNUSED(message);
    Q_UNUSED(helpPage);
    Q_UNUSED(options);
}

BasicUI::MessageBoxResult QtBasicUI::DoMessageBox(const TranslatableString& message, BasicUI::MessageBoxOptions options)
{
    Q_UNUSED(message);
    Q_UNUSED(options);
    return BasicUI::MessageBoxResult::None;
}

std::unique_ptr<BasicUI::ProgressDialog> QtBasicUI::DoMakeProgress(const TranslatableString& title, const TranslatableString& message,
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

std::unique_ptr<BasicUI::GenericProgressDialog> QtBasicUI::DoMakeGenericProgress(const BasicUI::WindowPlacement& placement,
                                                                                 const TranslatableString& title,
                                                                                 const TranslatableString& message, int style)
{
    Q_UNUSED(placement);
    Q_UNUSED(title);
    Q_UNUSED(message);
    Q_UNUSED(style);
    return std::make_unique<MyGenericProgress>();
}

int QtBasicUI::DoMultiDialog(const TranslatableString& message, const TranslatableString& title, const TranslatableStrings& buttons,
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

bool QtBasicUI::DoOpenInDefaultBrowser(const wxString& url)
{
    Q_UNUSED(url);
    return false;
}

std::unique_ptr<BasicUI::WindowPlacement> QtBasicUI::DoFindFocus()
{
    return nullptr;
}

void QtBasicUI::DoSetFocus(const BasicUI::WindowPlacement& focus)
{
    Q_UNUSED(focus);
}

bool QtBasicUI::IsUsingRtlLayout() const
{
    return false;
}

bool QtBasicUI::IsUiThread() const
{
    return false;
}
