/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectcontextmenumodel.h"

#include "framework/global/log.h"

#include "au3cloud/cloudcommands.h"
#include "../projectcommands.h"

using namespace au::project;
using namespace muse::rcommand;

namespace {
const QString SHOW_IN_FOLDER_ITEM_ID("project-show-in-folder");

muse::TranslatableString showInFolderTitle()
{
#if defined(Q_OS_MAC)
    return muse::TranslatableString("action", "Show in Finder");
#elif defined(Q_OS_WIN)
    return muse::TranslatableString("action", "Show in Explorer");
#else
    return muse::TranslatableString("action", "Show in File Manager");
#endif
}
}

CloudProjectContextMenuModel::CloudProjectContextMenuModel(QString projectId, muse::io::path_t localPath, QObject* parent)
    : AbstractMenuModel(parent), m_projectId(std::move(projectId)), m_localPath(std::move(localPath))
{
}

void CloudProjectContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    const Params projectIdParams { { PROJECT_ID_PARAM, muse::Val(m_projectId) } };

    muse::uicomponents::MenuItemList items = {
        makeMenuItem(make_query(PROJECT_OPEN_CLOUD_COMMAND, projectIdParams)),
        makeMenuItem(make_query(au3cloud::CLOUD_OPEN_PROJECT_PAGE_COMMAND, {
            { au3cloud::CLOUD_OPEN_PROJECT_PAGE_ID_PARAM, muse::Val(m_projectId) },
        })),
        makeMenuItem(make_query(PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND, projectIdParams)),
    };

    for (muse::uicomponents::MenuItem* item : items) {
        item->setEnabled(item->enabled() && !m_projectId.isEmpty());
    }

    if (!m_localPath.empty()) {
        muse::uicomponents::MenuItem* showInFolderItem = new muse::uicomponents::MenuItem(this);
        showInFolderItem->setId(SHOW_IN_FOLDER_ITEM_ID);
        showInFolderItem->setTitle(showInFolderTitle());
        showInFolderItem->setEnabled(true);
        items.append(showInFolderItem);
    }

    setItems(items);
}

void CloudProjectContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == SHOW_IN_FOLDER_ITEM_ID) {
        IF_ASSERT_FAILED(!m_localPath.empty()) {
            return;
        }

        platformInteractive()->revealInFileBrowser(m_localPath);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}
