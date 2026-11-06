/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "framework/global/log.h"

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
    IF_ASSERT_FAILED(openItem) {
        return;
    }
    openItem->setEnabled(openItem->enabled() && !m_audioId.isEmpty());

    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem(au3cloud::OpenAudioPageCommand { .slug = m_slug.toStdString() });
    IF_ASSERT_FAILED(viewAudiocom) {
        return;
    }
    viewAudiocom->setEnabled(viewAudiocom->enabled() && !m_slug.isEmpty());

    setItems({ openItem, viewAudiocom });
}
