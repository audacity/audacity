/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilecontextmenumodel.h"

#include "framework/actions/actiontypes.h"

using namespace au::project;

void CloudAudioFileContextMenuModel::load()
{
    muse::uicomponents::AbstractMenuModel::load();

    muse::uicomponents::MenuItem* openItem = makeMenuItem("cloud-audio-open");
    openItem->setState({ false, false });

    muse::uicomponents::MenuItem* viewAudiocom = makeMenuItem("cloud-open-audio-page");

    setItems({ openItem, viewAudiocom });
}

void CloudAudioFileContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == "cloud-open-audio-page") {
        muse::actions::ActionQuery query("audacity://cloud/open-audio-page");
        query.addParam("slug", muse::Val(m_slug.toStdString()));
        dispatch(query);
        return;
    }

    AbstractMenuModel::handleMenuItem(itemId);
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
