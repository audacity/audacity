#ifndef AU_PROJECT_PROJECTUIACTIONS_H
#define AU_PROJECT_PROJECTUIACTIONS_H

#include "ui/iuiactionsmodule.h"
#include "context/iuicontextresolver.h"
#include "projectactionscontroller.h"

namespace au::project {
class ProjectUiActions : public mu::ui::IUiActionsModule
{
    mu::Inject<mu::context::IUiContextResolver> uicontextResolver;

public:

    ProjectUiActions(std::shared_ptr<ProjectActionsController> controller);

    const mu::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    static const mu::ui::UiActionList m_actions;
    std::shared_ptr<ProjectActionsController> m_controller;
    mu::async::Channel<mu::actions::ActionCodeList> m_actionEnabledChanged;
    mu::async::Channel<mu::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_PROJECT_PROJECTUIACTIONS_H
