/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "actions/actionable.h"
#include "uicomponents/qml/Muse/UiComponents/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::projectscene {
class ProjectToolBarModel : public muse::uicomponents::AbstractToolBarModel, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isCompactMode READ isCompactMode WRITE setIsCompactMode NOTIFY isCompactModeChanged)

    muse::ContextInject<au::context::IGlobalContext> context { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<au::au3cloud::IAu3AudioComService> au3CloudService { this };

public:
    Q_INVOKABLE void load() override;

    bool isCompactMode() const;
    void setIsCompactMode(bool isCompactMode);

signals:
    void openAudioSetupContextMenu();
    void openGetEffectsDialog();
    void isCompactModeChanged();

private:
    bool m_loaded = false;
    bool m_isCompactMode = false;
};
}
