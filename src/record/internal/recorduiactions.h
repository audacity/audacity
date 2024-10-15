/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDUIACTIONS_H
#define AU_RECORD_RECORDUIACTIONS_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"

#include "ui/iuiactionsmodule.h"

#include "recordcontroller.h"

namespace au::record {
class RecordUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    INJECT(context::IUiContextResolver, uicontextResolver)

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
};
}

#endif // AU_RECORD_RECORDUIACTIONS_H
