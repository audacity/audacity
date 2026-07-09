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
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    void updateItems();
    void subscribeOnHistoryChanges();

    bool m_loaded = false;
};
}
