/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <chrono>

#include "au3-basic-ui/BasicUI.h" // For ProgressResult

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/interactive/iinteractive.h"

using ProgressResult = BasicUI::ProgressResult;

namespace au::au3 {
class ProgressDialog : public BasicUI::ProgressDialog, public muse::async::Asyncable, public muse::Contextable
{
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    ProgressDialog(const muse::modularity::ContextPtr& ctx, const ::TranslatableString& title = {});
    ProgressDialog(const muse::modularity::ContextPtr& ctx, const std::string& title);

public:
    virtual ~ProgressDialog();

    void Reinit() override;

    void SetDialogTitle(const ::TranslatableString& title) override;

public:
    ProgressResult Poll(
        unsigned long long numerator, unsigned long long denominator, const ::TranslatableString& message = {}) override;

    void SetMessage(const ::TranslatableString& message) override;

    bool cancelled() const
    {
        return m_cancelled;
    }

    //! Open the QML dialog and start the underlying `muse::Progress`.
    //! Callers that publish through `muse::Progress` directly (e.g. plugin
    //! scanners that drive `muse::Progress::progress(...)` instead of
    //! `Poll()`) must call this explicitly before reading `museProgress()`;
    //! the no-Poll path otherwise leaves the dialog unmounted and nothing
    //! appears on screen.
    void start();

    //! Underlying progress channel. Pure getter — does not mount the QML
    //! dialog; call `start()` first if no `Poll()`-based driver is going
    //! to do that on your behalf.
    muse::Progress& museProgress() { return m_progress; }

private:
    mutable muse::Progress m_progress;
    std::string m_progressTitle;
    std::string m_progressMessage;
    bool m_cancelled = false;
    bool m_canceledHooked = false;
    std::chrono::steady_clock::time_point m_lastEventPump {};
};
}
