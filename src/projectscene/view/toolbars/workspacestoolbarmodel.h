/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "uicomponents/view/abstractmenumodel.h"
#include "uicomponents/view/abstracttoolbarmodel.h"

namespace au::projectscene {
class WorkspacesToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> context;

public:
    WorkspacesToolBarModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;

    Q_INVOKABLE void handleWorkspacesMenuItem(const QString& itemId);

private:
    void loadWorkspacesModel();

    void updateState();

    std::shared_ptr<muse::uicomponents::AbstractMenuModel> m_workspacesMenuModel;
};
}
