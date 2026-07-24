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
constexpr const char* UPDATE_AUDIO_PREVIEW_ACTION = "audacity://cloud/update-audio-preview-for-project";
constexpr const char* SHOW_IN_FOLDER_ACTION = "project-show-in-folder";
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

    muse::uicomponents::MenuItem* openItem = makeMenuItem(OPEN_PROJECT_ACTION);
    openItem->setTitle(muse::TranslatableString("action", "Open"));

    muse::uicomponents::MenuItemList items = { openItem };
    if (isCloudProject) {
        items.append(makeMenuItem(OPEN_PROJECT_PAGE_ACTION));
        items.append(makeMenuItem(UPDATE_AUDIO_PREVIEW_ACTION));
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
        if (isCloudProject && !m_cloudProjectId.isEmpty()) {
            dispatcher()->dispatch("cloud-file-open",
                                   muse::actions::ActionData::make_arg1<QString>(m_cloudProjectId));
            return;
        }

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

    if (itemId == UPDATE_AUDIO_PREVIEW_ACTION) {
        if (m_cloudProjectId.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(UPDATE_AUDIO_PREVIEW_ACTION);
        query.addParam("id", muse::Val(m_cloudProjectId));
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
