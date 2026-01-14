/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDUIACTIONS_H
#define AU_RECORD_RECORDUIACTIONS_H

#include "framework/global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "framework/ui/iuiactionsmodule.h"

#include "recordcontroller.h"

namespace au::record {
class RecordUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<context::IUiContextResolver> uicontextResolver{ this };

public:
    RecordUiActions(std::shared_ptr<RecordController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const muse::ui::UiActionList m_mainActions;

    std::shared_ptr<RecordController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_RECORD_RECORDUIACTIONS_H
