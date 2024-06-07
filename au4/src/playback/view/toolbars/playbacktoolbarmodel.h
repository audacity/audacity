/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
#define AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"
#include "ui/iuiconfiguration.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordcontroller.h"

#include "uicomponents/view/abstracttoolbarmodel.h"

namespace au::playback {
class PlaybackToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister;
    muse::Inject<au::context::IGlobalContext> context;
    muse::Inject<au::playback::IPlaybackController> controller;
    muse::Inject<au::record::IRecordController> recordController;

public:
    explicit PlaybackToolBarModel(QObject* parent = nullptr);

    enum ItemType
    {
        UNDEFINED,
        PLAYBACK_LEVEL = muse::uicomponents::ToolBarItemType::USER_TYPE + 1,
        RECORD_LEVEL
    };
    Q_ENUM(ItemType)

    Q_INVOKABLE void load() override;

private:

    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    void setupConnections();
    void onProjectChanged();

    void updateActions();

    muse::uicomponents::ToolBarItem* makeLocalItem(const muse::actions::ActionCode& actionCode);

    muse::ui::UiAction playAction() const;
    muse::ui::UiAction recordAction() const;
};
}

#endif // AU_PROJECTSCENE_PLAYBACKTOOLBARMODEL_H
