/*
* Audacity: A Digital Audio Editor
*/
#ifndef MU_PROJECTSCENE_PLAYTOOLBARMODEL_H
#define MU_PROJECTSCENE_PLAYTOOLBARMODEL_H

#include "uicomponents/view/abstractmenumodel.h"

#include "modularity/ioc.h"
#include "ui/iuiactionsregister.h"

namespace au::projectscene {
class PlayToolBarModel : public mu::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    mu::Inject<mu::ui::IUiActionsRegister> actionsRegister;

public:
    Q_INVOKABLE void load() override;

private:
    mu::uicomponents::MenuItem* makeItem(const mu::actions::ActionCode& actionCode);
};
}

#endif // MU_PROJECTSCENE_PLAYTOOLBARMODEL_H
