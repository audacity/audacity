#ifndef AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H
#define AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H

#include "ui/iuiactionsmodule.h"

namespace au::projectscene {
class ProjectSceneUiActions : public muse::ui::IUiActionsModule
{
public:

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    static const muse::ui::ToolConfig& defaultPlaybackToolBarConfig();

private:
    const muse::ui::UiActionList& mainActions() const;
};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEUIACTIONS_H
