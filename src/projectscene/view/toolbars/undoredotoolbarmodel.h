/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/qml/Muse/UiComponents/abstracttoolbarmodel.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"

namespace au::projectscene {
class UndoRedoToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    muse::ContextInject<context::IGlobalContext> context { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };

public:

    Q_INVOKABLE void load() override;

private:
    void onCommandStateChanged(const muse::rcommand::Command& command, const muse::rcommand::CommandState& state) override;

    void updateTitle(const muse::rcommand::Command& command);
    void updateItems();
    void subscribeOnHistoryChanges();

    bool m_loaded = false;
};
}
