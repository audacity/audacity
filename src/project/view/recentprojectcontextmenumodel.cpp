/*
* Audacity: A Digital Audio Editor
*/
#include "recentprojectcontextmenumodel.h"

#include "framework/actions/actiontypes.h"
#include "framework/global/io/path.h"
#include "framework/global/types/translatablestring.h"

#include <QUrl>

using namespace au::project;

namespace {
constexpr const char* OPEN_PROJECT_ACTION = "file-open";
constexpr const char* OPEN_PROJECT_PAGE_ACTION = "audacity://cloud/open-project-page";
constexpr const char* SHOW_IN_FOLDER_ACTION = "project-show-in-folder";
}

RecentProjectContextMenuModel::RecentProjectContextMenuModel(bool isCloudProject, QString path, QString displayNameOverride,
                                                             QObject* parent)
    : AbstractMenuModel(parent), isCloudProject(isCloudProject), m_path(std::move(path)),
    m_displayNameOverride(std::move(displayNameOverride))
{
}

void RecentProjectContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem(OPEN_PROJECT_ACTION);
    openItem->setTitle(muse::TranslatableString("action", "Open"));

    muse::uicomponents::MenuItemList items = { openItem };
    if (isCloudProject) {
        items.append(makeMenuItem(OPEN_PROJECT_PAGE_ACTION));
    }

    if (!m_path.isEmpty()) {
        items.append(makeMenuItem(SHOW_IN_FOLDER_ACTION));
    }

    setItems(items);

    openItem->setShortcuts({});
}

void RecentProjectContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == OPEN_PROJECT_ACTION) {
        dispatcher()->dispatch("file-open",
                               muse::actions::ActionData::make_arg2<QUrl, QString>(
                                   QUrl::fromLocalFile(m_path), m_displayNameOverride));
        return;
    }

    if (itemId == OPEN_PROJECT_PAGE_ACTION) {
        if (m_path.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(OPEN_PROJECT_PAGE_ACTION);
        query.addParam("path", muse::Val(muse::io::path_t(m_path.toStdString())));
        dispatch(query);
        return;
    }

    if (itemId == SHOW_IN_FOLDER_ACTION) {
        if (m_path.isEmpty()) {
            return;
        }

        platformInteractive()->revealInFileBrowser(m_path);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}
