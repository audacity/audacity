/*
* Audacity: A Digital Audio Editor
*/
#include "recentprojectcontextmenumodel.h"

#include "framework/global/types/translatablestring.h"

#include <QUrl>

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

RecentProjectContextMenuModel::RecentProjectContextMenuModel(bool isCloudProject, QString path, QString displayNameOverride,
                                                             QString cloudProjectId, QObject* parent)
    : AbstractMenuModel(parent), isCloudProject(isCloudProject), m_path(std::move(path)),
    m_displayNameOverride(std::move(displayNameOverride)), m_cloudProjectId(std::move(cloudProjectId))
{
}

void RecentProjectContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = nullptr;
    if (isCloudProject && !m_cloudProjectId.isEmpty()) {
        openItem = makeMenuItem(make_query(PROJECT_OPEN_CLOUD_COMMAND, { { PROJECT_ID_PARAM, muse::Val(m_cloudProjectId) } }));
    } else {
        openItem = makeMenuItem(make_query(PROJECT_OPEN_COMMAND, {
            { PROJECT_URL_PARAM, muse::Val(QUrl::fromLocalFile(m_path).toString()) },
            { PROJECT_DISPLAY_NAME_PARAM, muse::Val(m_displayNameOverride) },
        }));
    }
    openItem->setTitle(muse::TranslatableString("action", "Open"));

    muse::uicomponents::MenuItemList items = { openItem };
    if (isCloudProject) {
        muse::uicomponents::MenuItem* pageItem = makeMenuItem(make_query(au3cloud::CLOUD_OPEN_PROJECT_PAGE_COMMAND, {
            { au3cloud::CLOUD_OPEN_PROJECT_PAGE_ID_PARAM, muse::Val(m_cloudProjectId) },
        }));
        pageItem->setEnabled(pageItem->enabled() && !m_cloudProjectId.isEmpty());
        items.append(pageItem);

        muse::uicomponents::MenuItem* previewItem = makeMenuItem(make_query(PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND, {
            { PROJECT_ID_PARAM, muse::Val(m_cloudProjectId) },
        }));
        previewItem->setEnabled(previewItem->enabled() && !m_cloudProjectId.isEmpty());
        items.append(previewItem);
    }

    if (!m_path.isEmpty()) {
        muse::uicomponents::MenuItem* showInFolderItem = new muse::uicomponents::MenuItem(this);
        showInFolderItem->setId(SHOW_IN_FOLDER_ITEM_ID);
        showInFolderItem->setTitle(showInFolderTitle());
        showInFolderItem->setEnabled(true);
        items.append(showInFolderItem);
    }

    setItems(items);
}

void RecentProjectContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == SHOW_IN_FOLDER_ITEM_ID) {
        if (m_path.isEmpty()) {
            return;
        }

        platformInteractive()->revealInFileBrowser(m_path);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}
