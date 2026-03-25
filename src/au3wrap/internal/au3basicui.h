/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/global/iapplication.h"

#include "au3-basic-ui/BasicUI.h"

// WindowPlacement subclass that carries a muse context.
// Created by WindowPlacementFactory when au3 code calls
// ProjectFramePlacement(&project).
class Au3WindowPlacement : public BasicUI::WindowPlacement, public kors::modularity::Contextable
{
public:
    Au3WindowPlacement(muse::modularity::ContextPtr ctx)
        : Contextable(std::move(ctx)) {}
};

class Au3BasicUI final : public BasicUI::Services, public muse::async::Asyncable
{
public:
    Au3BasicUI(std::shared_ptr<muse::IApplication> app)
        : m_app(std::move(app)) {}

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

private:
    muse::modularity::ContextPtr contextFromPlacement(const BasicUI::WindowPlacement& placement) const;
    muse::modularity::ContextPtr contextFromPlacement(const BasicUI::WindowPlacement* placement) const;
    muse::modularity::ContextPtr activeContext() const;
    std::shared_ptr<muse::IInteractive> interactiveForContext(const muse::modularity::ContextPtr& ctx) const;

    std::shared_ptr<muse::IApplication> m_app;
};
