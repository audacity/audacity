/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PROJECTTOOLBARMODEL_H
#define AU_PROJECTSCENE_PROJECTTOOLBARMODEL_H

#include "uicomponents/view/abstractmenumodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"

namespace au::projectscene {
class ProjectToolBarModel : public mu::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    mu::Inject<mu::context::IGlobalContext> context;
    mu::Inject<mu::ui::IUiActionsRegister> actionsRegister;

public:
    Q_INVOKABLE void load() override;

private:
    mu::uicomponents::MenuItem* makeItem(const mu::actions::ActionCode& actionCode);
};
}

#endif // AU_PROJECTSCENE_PROJECTTOOLBARMODEL_H
