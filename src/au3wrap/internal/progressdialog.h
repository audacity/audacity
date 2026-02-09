/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-basic-ui/BasicUI.h" // For ProgressResult

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/interactive/iinteractive.h"

using ProgressResult = BasicUI::ProgressResult;

class ProgressDialog : public BasicUI::ProgressDialog, public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<muse::IInteractive> interactive { this };

public:
    ProgressDialog(const muse::modularity::ContextPtr& ctx, const TranslatableString& title = {});
    ProgressDialog(const muse::modularity::ContextPtr& ctx, const std::string& title);

public:
    virtual ~ProgressDialog();

    void Reinit() override;

    void SetDialogTitle(const TranslatableString& title) override;

public:
    ProgressResult Poll(
        unsigned long long numerator, unsigned long long denominator, const TranslatableString& message = {}) override;

    void SetMessage(const TranslatableString& message) override;

    bool cancelled() const
    {
        return m_cancelled;
    }

private:
    mutable muse::Progress m_progress;
    std::string m_progressTitle;
    std::string m_progressMessage;
    bool m_cancelled = false;
};
