/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKUIACTIONS_H
#define AU_PLAYBACK_PLAYBACKUIACTIONS_H

#include "async/asyncable.h"
#include "context/iuicontextresolver.h"
#include "ui/iuiactionsmodule.h"
#include "ui/iuiactionsregister.h"

#include "iaudiodevicesprovider.h"
#include "internal/playbackcontroller.h"

namespace au::playback {
class PlaybackUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    INJECT(context::IUiContextResolver, uicontextResolver)
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

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
    void registerActions();

private:
    muse::ui::UiActionList m_actions;
    static const muse::ui::UiActionList m_mainActions;
    static const muse::ui::UiActionList m_settingsActions;
    static const muse::ui::UiActionList m_loopBoundaryActions;
    static const muse::ui::UiActionList m_meterDbRangeActions;

    std::shared_ptr<PlaybackController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PLAYBACK_PLAYBACKUIACTIONS_H
