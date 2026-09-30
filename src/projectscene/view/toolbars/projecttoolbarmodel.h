/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/qml/Muse/UiComponents/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "context/iuicontextresolver.h"
#include "au3cloud/iau3audiocomservice.h"
#include "projectscene/iprojectsceneactionscontroller.h"

namespace au::projectscene {
class ProjectToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    Q_PROPERTY(bool isCompactMode READ isCompactMode WRITE setIsCompactMode NOTIFY isCompactModeChanged)

    muse::ContextInject<IProjectSceneActionsController> projectSceneActionsController { this };
    muse::ContextInject<context::IGlobalContext> context { this };
    muse::ContextInject<context::IUiContextResolver> uicontextResolver { this };
    muse::ContextInject<au3cloud::IAu3AudioComService> au3CloudService { this };

public:
    Q_INVOKABLE void load() override;

    bool isCompactMode() const;
    void setIsCompactMode(bool isCompactMode);

signals:
    void openAudioSetupContextMenu();
    void isCompactModeChanged();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    bool m_loaded = false;
    bool m_isCompactMode = false;
};
}
