/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "BasicUI.h" // For ProgressResult

#include "iinteractive.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

using ProgressResult = BasicUI::ProgressResult;

class ProgressDialog : public BasicUI::ProgressDialog, public muse::async::Asyncable
{
    muse::Inject<muse::IInteractive> interactive;

public:
    ProgressDialog(const TranslatableString& title = {});
    ProgressDialog(const std::string& title);

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
