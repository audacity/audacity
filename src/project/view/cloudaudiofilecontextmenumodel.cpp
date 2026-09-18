/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "framework/actions/actiontypes.h"

using namespace au::project;

namespace {
constexpr const char* OPEN_AUDIO_FILE_ACTION = "audacity://cloud/open-audio-file";
constexpr const char* OPEN_AUDIO_PAGE_ACTION = "audacity://cloud/open-audio-page";
}

CloudAudioFileContextMenuModel::CloudAudioFileContextMenuModel(QString audioId, QString slug, QObject* parent)
    : AbstractMenuModel(parent), m_audioId(std::move(audioId)), m_slug(std::move(slug))
{
}

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem(OPEN_AUDIO_FILE_ACTION);
    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem(OPEN_AUDIO_PAGE_ACTION);

    setItems({ openItem, viewAudiocom });
}

void CloudAudioFileContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == OPEN_AUDIO_PAGE_ACTION) {
        if (m_slug.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(OPEN_AUDIO_PAGE_ACTION);
        query.addParam("slug", muse::Val(m_slug));
        dispatchAction(query);
        return;
    }

    if (itemId == OPEN_AUDIO_FILE_ACTION) {
        if (m_audioId.isEmpty()) {
            return;
        }

        muse::actions::ActionQuery query(OPEN_AUDIO_FILE_ACTION);
        query.addParam("audioId", muse::Val(m_audioId));
        dispatchAction(query);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}
