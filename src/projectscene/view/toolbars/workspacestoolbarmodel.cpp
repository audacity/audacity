/*
* Audacity: A Digital Audio Editor
*/
#include "workspacestoolbarmodel.h"

#include "muse_framework_config.h"

#ifdef MUSE_MODULE_WORKSPACE
#include "workspace/view/workspacesmenumodel.h"
#endif

#include "uicomponents/view/toolbaritem.h"

#include "translation.h"

static const muse::actions::ActionCode WORKSPACES_ACTION_CODE = "workspaces";

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;
using namespace muse::ui;

WorkspacesToolBarModel::WorkspacesToolBarModel(QObject* parent)
    : muse::uicomponents::AbstractToolBarModel(parent)
{
#ifdef MUSE_MODULE_WORKSPACE
    m_workspacesMenuModel = std::make_shared<muse::workspace::WorkspacesMenuModel>(this);
#endif
}

void WorkspacesToolBarModel::load()
{
    AbstractToolBarModel::load();
    loadWorkspacesModel();

    ToolBarItemList items;

    UiAction workspacesAction;
    workspacesAction.code = WORKSPACES_ACTION_CODE;

    ToolBarItem* item = new ToolBarItem(workspacesAction, ToolBarItemType::ACTION, this);

    items << item;

    setItems(items);

    updateState();
}

void WorkspacesToolBarModel::handleWorkspacesMenuItem(const QString& itemId)
{
#ifdef MUSE_MODULE_WORKSPACE
    m_workspacesMenuModel->handleMenuItem(itemId);
#endif
}

void WorkspacesToolBarModel::loadWorkspacesModel()
{
#ifdef MUSE_MODULE_WORKSPACE
    m_workspacesMenuModel.get()->disconnect();

    m_workspacesMenuModel->load();

    connect(m_workspacesMenuModel.get(), &muse::workspace::WorkspacesMenuModel::itemsChanged, this, [this](){
        updateState();
    });
#endif
}

void WorkspacesToolBarModel::updateState()
{
    ToolBarItem& item = findItem(WORKSPACES_ACTION_CODE);
    if (!item.isValid()) {
        return;
    }

    muse::TranslatableString currentWorkspaceName;

    for (const MenuItem* menuItem : m_workspacesMenuModel->items()) {
        if (menuItem->selected()) {
            currentWorkspaceName = menuItem->action().title.raw();
        }
    }

    item.setTitle(currentWorkspaceName);
    item.setMenuItems(m_workspacesMenuModel->items());
}
