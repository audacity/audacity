#ifndef AU_PROJECT_PROJECTUIACTIONS_H
#define AU_PROJECT_PROJECTUIACTIONS_H

#include "ui/iuiactionsmodule.h"
#include "context/iuicontextresolver.h"
#include "projectactionscontroller.h"
#include "record/irecordcontroller.h"
#include "async/asyncable.h"

namespace au::project {
class ProjectUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    muse::Inject<context::IUiContextResolver> uicontextResolver;
    muse::Inject<record::IRecordController> recordController;

public:

    ProjectUiActions(std::shared_ptr<ProjectActionsController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const muse::ui::UiActionList m_actions;
    std::shared_ptr<ProjectActionsController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PROJECT_PROJECTUIACTIONS_H
