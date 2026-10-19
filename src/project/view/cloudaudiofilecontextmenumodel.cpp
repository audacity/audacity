/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "au3cloud/cloudcommands.h"
#include "project/projectcommands.h"

using namespace au::project;
using namespace muse::rcommand;

CloudAudioFileContextMenuModel::CloudAudioFileContextMenuModel(QString audioId, QString slug, QObject* parent)
    : AbstractMenuModel(parent), m_audioId(std::move(audioId)), m_slug(std::move(slug))
{
}

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem(make_query(PROJECT_OPEN_CLOUD_AUDIO_FILE_COMMAND, {
        { PROJECT_AUDIO_ID_PARAM, muse::Val(m_audioId) },
    }));
    openItem->setEnabled(openItem->enabled() && !m_audioId.isEmpty());

    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem(make_query(au3cloud::CLOUD_OPEN_AUDIO_PAGE_COMMAND, {
        { au3cloud::CLOUD_OPEN_AUDIO_PAGE_SLUG_PARAM, muse::Val(m_slug) },
    }));
    viewAudiocom->setEnabled(viewAudiocom->enabled() && !m_slug.isEmpty());

    setItems({ openItem, viewAudiocom });
}
