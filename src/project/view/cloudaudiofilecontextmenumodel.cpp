/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "framework/actions/actiontypes.h"

#include "au3cloud/cloudcommands.h"

using namespace au::project;

namespace {
constexpr const char* OPEN_AUDIO_FILE_ACTION = "action://cloud/open-audio-file";
}

CloudAudioFileContextMenuModel::CloudAudioFileContextMenuModel(QString audioId, QString slug, QObject* parent)
    : AbstractMenuModel(parent), m_audioId(std::move(audioId)), m_slug(std::move(slug))
{
}

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem(OPEN_AUDIO_FILE_ACTION);
    muse::uicomponents::MenuItem* viewAudiocom
        = makeMenuItem(au3cloud::OpenAudioPageCommand { .slug = m_slug.toStdString() });

    setItems({ openItem, viewAudiocom });
}

void CloudAudioFileContextMenuModel::handleMenuItem(const QString& itemId)
{
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
