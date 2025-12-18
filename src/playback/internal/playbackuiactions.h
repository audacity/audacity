/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKUIACTIONS_H
#define AU_PLAYBACK_PLAYBACKUIACTIONS_H

#include "framework/global/async/asyncable.h"
#include "framework/ui/iuiactionsmodule.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iuicontextresolver.h"
#include "internal/playbackcontroller.h"

namespace au::playback {
class PlaybackUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    muse::Inject<context::IUiContextResolver> uiContextResolver;
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider;

public:
    PlaybackUiActions(std::shared_ptr<PlaybackController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    static const muse::ui::UiActionList& settingsActions();
    void registerActions();

private:
    muse::ui::UiActionList m_actions;
    static const muse::ui::UiActionList m_mainActions;
    static const muse::ui::UiActionList m_settingsActions;
    static const muse::ui::UiActionList m_meterDbRangeActions;

    std::shared_ptr<PlaybackController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PLAYBACK_PLAYBACKUIACTIONS_H
