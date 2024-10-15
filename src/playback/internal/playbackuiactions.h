/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKUIACTIONS_H
#define AU_PLAYBACK_PLAYBACKUIACTIONS_H

#include "ui/iuiactionsmodule.h"
#include "playbackcontroller.h"
#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "async/asyncable.h"

namespace au::playback {
class PlaybackUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    INJECT(context::IUiContextResolver, uicontextResolver)

public:
    PlaybackUiActions(std::shared_ptr<PlaybackController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    static const muse::ui::UiActionList& settingsActions();
    static const muse::ui::UiActionList& loopBoundaryActions();

private:
    static const muse::ui::UiActionList m_mainActions;
    static const muse::ui::UiActionList m_settingsActions;
    static const muse::ui::UiActionList m_loopBoundaryActions;

    std::shared_ptr<PlaybackController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PLAYBACK_PLAYBACKUIACTIONS_H
