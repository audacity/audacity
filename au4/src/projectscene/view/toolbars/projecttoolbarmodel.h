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
class ProjectToolBarModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<mu::context::IGlobalContext> context;
    muse::Inject<muse::ui::IUiActionsRegister> actionsRegister;

public:
    Q_INVOKABLE void load() override;

private:
    muse::uicomponents::MenuItem* makeItem(const muse::actions::ActionCode& actionCode);
};
}

#endif // AU_PROJECTSCENE_PROJECTTOOLBARMODEL_H
