/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectcontextmenumodel.h"

#include "framework/actions/actiontypes.h"
#include "framework/global/log.h"

using namespace au::project;

namespace {
constexpr const char* OPEN_PROJECT_ACTION = "cloud-file-open";
constexpr const char* OPEN_PROJECT_PAGE_ACTION = "audacity://cloud/open-project-page";
constexpr const char* UPDATE_AUDIO_PREVIEW_ACTION = "audacity://cloud/update-audio-preview-for-project";
constexpr const char* SHOW_IN_FOLDER_ACTION = "project-show-in-folder";
}

CloudProjectContextMenuModel::CloudProjectContextMenuModel(QString projectId, muse::io::path_t localPath, QObject* parent)
    : AbstractMenuModel(parent), m_projectId(std::move(projectId)), m_localPath(std::move(localPath))
{
}

void CloudProjectContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItemList items = {
        makeMenuItem(OPEN_PROJECT_ACTION),
        makeMenuItem(OPEN_PROJECT_PAGE_ACTION),
        makeMenuItem(UPDATE_AUDIO_PREVIEW_ACTION)
    };

    if (!m_localPath.empty()) {
        items.append(makeMenuItem(SHOW_IN_FOLDER_ACTION));
    }

    setItems(items);
}

void CloudProjectContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == OPEN_PROJECT_ACTION) {
        if (m_projectId.isEmpty()) {
            return;
        }

        dispatcher()->dispatch("cloud-file-open", muse::actions::ActionData::make_arg1<QString>(m_projectId));
        return;
    }

    if (itemId == OPEN_PROJECT_PAGE_ACTION) {
        if (m_projectId.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(OPEN_PROJECT_PAGE_ACTION);
        query.addParam("id", muse::Val(m_projectId));
        dispatch(query);
        return;
    }

    if (itemId == SHOW_IN_FOLDER_ACTION) {
        IF_ASSERT_FAILED(!m_localPath.empty()) {
            return;
        }

        platformInteractive()->revealInFileBrowser(m_localPath);
        return;
    }

    if (itemId == UPDATE_AUDIO_PREVIEW_ACTION) {
        if (m_projectId.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(UPDATE_AUDIO_PREVIEW_ACTION);
        query.addParam("id", muse::Val(m_projectId));
        dispatch(query);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}
