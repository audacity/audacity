#ifndef AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H
#define AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H

#include "ui/iuiactionsmodule.h"

namespace au::projectscene {
class ProjectSceneUiActions : public mu::ui::IUiActionsModule
{
public:

    const mu::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionCheckedChanged() const override;

private:
    const mu::ui::UiActionList& mainActions() const;
};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H
