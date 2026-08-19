/*
* Audacity: A Digital Audio Editor
*/
#include "workspacestoolbarmodel.h"

#include "muse_framework_config.h"

#ifdef MUSE_MODULE_WORKSPACE
#include "workspace/qml/Muse/Workspace/workspacesmenumodel.h"
#endif

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

#include "translation.h"

static const muse::actions::ActionCode WORKSPACES_ACTION_CODE = "workspaces";

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;
using namespace muse::ui;

namespace {
//! NOTE Workspace names are file names, so they exist only as data and cannot
//! be extracted for translation. Give the workspaces we ship a translated title;
muse::TranslatableString workspaceTitle(const std::string& name)
{
    if (name == "Classic") {
        return muse::TranslatableString("workspace", "Classic");
    } else if (name == "Modern") {
        return muse::TranslatableString("workspace", "Modern");
    } else if (name == "Music") {
        return muse::TranslatableString("workspace", "Music");
    }

    return muse::TranslatableString::untranslatable(muse::String::fromStdString(name));
}

void translateWorkspaceTitles(const muse::uicomponents::MenuItemList& items)
{
    for (muse::uicomponents::MenuItem* item : items) {
        const muse::actions::ActionData args = item->args();
        if (args.empty()) {
            continue;
        }

        muse::ui::UiAction action = item->action();
        action.title = workspaceTitle(args.arg<std::string>(0));
        item->setAction(action);
    }
}
}

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

    translateWorkspaceTitles(m_workspacesMenuModel->items());

    for (const MenuItem* menuItem : m_workspacesMenuModel->items()) {
        if (menuItem->selected()) {
            currentWorkspaceName = menuItem->action().title.raw();
        }
    }

    item.setTitle(currentWorkspaceName);
    item.setMenuItems(m_workspacesMenuModel->items());
}
