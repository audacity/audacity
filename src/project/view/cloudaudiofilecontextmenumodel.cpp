/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "framework/actions/actiontypes.h"

using namespace au::project;

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem("audacity://cloud/open-audio-file");
    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem("audacity://cloud/open-audio-page");

    setItems({ openItem, viewAudiocom });
}

void CloudAudioFileContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == "audacity://cloud/open-audio-page") {
        muse::actions::ActionQuery query("audacity://cloud/open-audio-page");
        query.addParam("slug", muse::Val(m_slug.toStdString()));
        dispatch(query);
        return;
    }

    if (itemId == "audacity://cloud/open-audio-file") {
        muse::actions::ActionQuery query("audacity://cloud/open-audio-file");
        query.addParam("audioId", muse::Val(m_audioId.toStdString()));
        dispatch(query);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
}

QString CloudAudioFileContextMenuModel::audioId() const
{
    return m_audioId;
}

void CloudAudioFileContextMenuModel::setAudioId(const QString& audioId)
{
    if (m_audioId == audioId) {
        return;
    }
    m_audioId = audioId;
    emit audioIdChanged();
}

QString CloudAudioFileContextMenuModel::slug() const
{
    return m_slug;
}

void CloudAudioFileContextMenuModel::setSlug(const QString& slug)
{
    if (m_slug == slug) {
        return;
    }
    m_slug = slug;
    emit slugChanged();
}
