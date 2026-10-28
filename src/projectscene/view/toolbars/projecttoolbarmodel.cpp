/*
* Audacity: A Digital Audio Editor
*/
#include "projecttoolbarmodel.h"

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

#include "effects/effects_base/effectscommands.h"
#include "project/projectcommands.h"
#include "projectscene/projectscenecommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::rcommand;

void ProjectToolBarModel::load()
{
    if (m_loaded) {
        return;
    }

    AbstractToolBarModel::load();

    std::vector<Command> commands = {
        PROJECTSCENE_AUDIO_SETUP_COMMAND,
    };

    if (au3CloudService()->enabled()) {
        commands.push_back(project::PROJECT_SHARE_AUDIO_COMMAND);
    }

#ifndef Q_OS_LINUX
    commands.push_back(effects::EFFECTS_GET_EFFECTS_COMMAND);
#endif

    ToolBarItemList items;
    for (const Command& command : commands) {
        ToolBarItem* item = makeItem(command);
        if (!item) {
            continue;
        }
        item->setShowTitle(true);

        items << item;
    }

    setItems(items);

    context()->currentProjectChanged().onNotify(this, [this]() {
        load();
    });

    projectSceneActionsController()->audioSetupContextMenuRequested().onNotify(this, [this]() {
        emit openAudioSetupContextMenu();
    });

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

void ProjectToolBarModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    if (uicontextResolver()->currentUiContext() == context::UiCtxDialogOpened) {
        return;
    }

    AbstractToolBarModel::onCommandStateChanged(command, state);
}
