/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "actions/actionable.h"
#include "uicomponents/view/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class ProjectToolBarModel : public muse::uicomponents::AbstractToolBarModel, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isCompactMode READ isCompactMode WRITE setIsCompactMode NOTIFY isCompactModeChanged)

    muse::Inject<au::context::IGlobalContext> context;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    Q_INVOKABLE void load() override;

    bool isCompactMode() const;
    void setIsCompactMode(bool isCompactMode);

signals:
    void openAudioSetupContextMenu();
    void isCompactModeChanged();

private:
    bool m_isCompactMode = false;
};
}
