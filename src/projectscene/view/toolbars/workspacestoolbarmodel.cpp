/*
* Audacity: A Digital Audio Editor
*/
#include "workspacestoolbarmodel.h"

#include "shared/types/workspacetitles.h"

#include "muse_framework_config.h"

#ifdef MUSE_MODULE_WORKSPACE
#include "workspace/qml/Muse/Workspace/workspacesmenumodel.h"
#endif

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

#include "translation.h"

static const QString WORKSPACES_ITEM_ID = "workspaces";

using namespace au::projectscene;
using namespace muse::uicomponents;

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

    ToolBarItem* item = new ToolBarItem(ToolBarItemType::ACTION, this);
    item->setId(WORKSPACES_ITEM_ID);

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
    ToolBarItem& item = findItem(WORKSPACES_ITEM_ID);
    if (!item.isValid()) {
        return;
    }

    muse::TranslatableString currentWorkspaceName;

    au::shared::translateWorkspaceTitles(m_workspacesMenuModel->items());

    for (const MenuItem* menuItem : m_workspacesMenuModel->items()) {
        if (menuItem->selected()) {
            currentWorkspaceName = muse::TranslatableString::untranslatable(menuItem->translatedTitle());
        }
    }

    item.setTitle(currentWorkspaceName);
    item.setMenuItems(m_workspacesMenuModel->items());
}
