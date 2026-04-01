/*
* Audacity: A Digital Audio Editor
*/
#include "projecttoolbarmodel.h"

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void ProjectToolBarModel::load()
{
    if (m_loaded) {
        return;
    }

    AbstractToolBarModel::load();

    muse::actions::ActionCodeList itemsCodes = {
        "audio-setup",
    };

    if (au3CloudService()->enabled()) {
        itemsCodes.push_back("file-share-audio");
    }

    itemsCodes.push_back("get-effects");

    ToolBarItemList items;
    for (const ActionCode& code : itemsCodes) {
        ToolBarItem* item = makeItem(code);
        item->setShowTitle(true);

        items << item;
    }

    setItems(items);

    context()->currentProjectChanged().onNotify(this, [this]() {
        load();
    });

    dispatcher()->reg(this, "audio-setup", [this]() { emit openAudioSetupContextMenu(); });
    dispatcher()->reg(this, "get-effects", [this]() { emit openGetEffectsDialog(); });

    m_loaded = true;
}

bool ProjectToolBarModel::isCompactMode() const
{
    return m_isCompactMode;
}

void ProjectToolBarModel::setIsCompactMode(bool isCompactMode)
{
    if (m_isCompactMode == isCompactMode) {
        return;
    }

    for (int i = 0; i < rowCount(); ++i) {
        ToolBarItem& item = this->item(i);
        item.setShowTitle(!isCompactMode);
    }

    m_isCompactMode = isCompactMode;
    emit isCompactModeChanged();
}
