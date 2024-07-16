/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROCESSING_PROCESSINGUIACTIONS_H
#define AU_PROCESSING_PROCESSINGUIACTIONS_H

#include "ui/iuiactionsmodule.h"
#include "processingactionscontroller.h"
#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "async/asyncable.h"

namespace au::processing {
class ProcessingUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    INJECT(context::IUiContextResolver, uicontextResolver)

public:
    ProcessingUiActions(std::shared_ptr<ProcessingActionsController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const muse::ui::UiActionList m_actions;

    std::shared_ptr<ProcessingActionsController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PROCESSING_PROCESSINGUIACTIONS_H
