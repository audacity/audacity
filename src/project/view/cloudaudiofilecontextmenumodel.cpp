/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

using namespace au::project;

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem("cloud-audio-open");
    openItem->setState({ false, false });

    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem("audio-view-on-audiocom");
    viewAudiocom->setState({ false, false });

    setItems({ openItem, viewAudiocom });
}

void CloudAudioFileContextMenuModel::handleMenuItem(const QString&)
{
}

QString CloudAudioFileContextMenuModel::cloudItemId() const
{
    return m_cloudItemId;
}

void CloudAudioFileContextMenuModel::setCloudItemId(const QString& cloudItemId)
{
    if (m_cloudItemId == cloudItemId) {
        return;
    }
    m_cloudItemId = cloudItemId;
    emit cloudItemIdChanged();
}
