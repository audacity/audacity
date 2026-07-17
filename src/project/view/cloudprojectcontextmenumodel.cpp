/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectcontextmenumodel.h"

#include "framework/actions/actiontypes.h"

using namespace au::project;

namespace {
constexpr const char* OPEN_PROJECT_ACTION = "cloud-file-open";
constexpr const char* OPEN_PROJECT_PAGE_ACTION = "audacity://cloud/open-project-page";
}

CloudProjectContextMenuModel::CloudProjectContextMenuModel(QString projectId, QObject* parent)
    : AbstractMenuModel(parent), m_projectId(std::move(projectId))
{
}

void CloudProjectContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem(OPEN_PROJECT_ACTION);
    muse::uicomponents::MenuItem* viewProjectPage = makeMenuItem(OPEN_PROJECT_PAGE_ACTION);

    setItems({ openItem, viewProjectPage });
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

    AbstractMenuModel::handleMenuItem(itemId);
}
