#ifndef AU_PROJECT_PROJECTUIACTIONS_H
#define AU_PROJECT_PROJECTUIACTIONS_H

#include "context/iuicontextresolver.h"
#include "projectactionscontroller.h"
#include "ui/iuiactionsmodule.h"

namespace au::project {
class ProjectUiActions : public muse::ui::IUiActionsModule
{
    muse::Inject<context::IUiContextResolver> uicontextResolver;

public:
    ProjectUiActions(std::shared_ptr<ProjectActionsController> controller);

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
}  // namespace au::project

#endif  // AU_PROJECT_PROJECTUIACTIONS_H
