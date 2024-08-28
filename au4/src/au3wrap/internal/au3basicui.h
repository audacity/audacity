/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "BasicUI.h"

class Au3BasicUI final : public BasicUI::Services, public muse::async::Asyncable
{
public:
    ~Au3BasicUI() override;

protected:
    void DoCallAfter(const BasicUI::Action& action) override;
    void DoYield() override;
    void DoShowErrorDialog(const BasicUI::WindowPlacement& placement, const TranslatableString& dlogTitle,
                           const TranslatableString& message, const ManualPageID& helpPage,
                           const BasicUI::ErrorDialogOptions& options) override;
    BasicUI::MessageBoxResult DoMessageBox(
        const TranslatableString& message, BasicUI::MessageBoxOptions options) override;
    std::unique_ptr<BasicUI::ProgressDialog>
    DoMakeProgress(const TranslatableString& title, const TranslatableString& message, unsigned flags,
                   const TranslatableString& remainingLabelText) override;
    std::unique_ptr<BasicUI::GenericProgressDialog>
    DoMakeGenericProgress(const BasicUI::WindowPlacement& placement, const TranslatableString& title, const TranslatableString& message,
                          int style) override;
    int DoMultiDialog(const TranslatableString& message, const TranslatableString& title, const TranslatableStrings& buttons,
                      const ManualPageID& helpPage, const TranslatableString& boxMsg, bool log) override;

    bool DoOpenInDefaultBrowser(const wxString& url) override;

    std::unique_ptr<BasicUI::WindowPlacement> DoFindFocus() override;
    void DoSetFocus(const BasicUI::WindowPlacement& focus) override;

    bool IsUsingRtlLayout() const override;

    bool IsUiThread() const override;
};
