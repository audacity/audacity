/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/ui/iuiactionsmodule.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "trackeditactionscontroller.h"
#include "context/iuicontextresolver.h"
#include "audio/iaudiodevicesprovider.h"

namespace au::trackedit {
class TrackeditUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<context::IUiContextResolver> uicontextResolver;

public:
    TrackeditUiActions(std::shared_ptr<TrackeditActionsController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    muse::ui::UiActionList m_actions;

    std::shared_ptr<TrackeditActionsController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}
