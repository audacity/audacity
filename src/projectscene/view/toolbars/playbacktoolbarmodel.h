/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "ui/iuiactionsregister.h"
#include "ui/iuiconfiguration.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordcontroller.h"
#include "record/irecordconfiguration.h"

#include "uicomponents/qml/Muse/UiComponents/abstracttoolbarmodel.h"

namespace au::project {
class IAudacityProject;
}

namespace au::projectscene {
class PlaybackToolBarModel : public muse::uicomponents::AbstractToolBarModel
{
    Q_OBJECT

    Q_PROPERTY(bool isEnabled READ isEnabled NOTIFY isEnabledChanged)

    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::GlobalInject<playback::IPlaybackConfiguration> configuration;
    muse::GlobalInject<record::IRecordConfiguration> recordConfiguration;

    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister{ this };
    muse::Inject<context::IGlobalContext> context{ this };
    muse::Inject<playback::IPlaybackController> playbackController{ this };
    muse::Inject<record::IRecordController> recordController{ this };

public:
    explicit PlaybackToolBarModel(QObject* parent = nullptr);

    enum ItemType
    {
        UNDEFINED,
        PLAYBACK_LEVEL = muse::uicomponents::ToolBarItemType::USER_TYPE + 1,
        RECORD_LEVEL,
        PLAYBACK_TIME,
        PLAYBACK_BPM,
        PLAYBACK_TIME_SIGNATURE,
        PLAYBACK_CONTROL,
        PROJECT_CONTROL,
        SNAP
    };
    Q_ENUM(ItemType)

    Q_INVOKABLE void load() override;

    bool isEnabled() const;

signals:
    void isEnabledChanged();

private:
    void reload();
    void setupProjectConnections(project::IAudacityProject& project);

    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    void updateStates();
    void updatePlayState();
    void updateStopState();
    void updateRecordState();
    void updateLoopState();
    void updateSplitState();
    void updateGlobalSpectrogramViewState();

    void setupConnections();

    void updateActions();

    muse::uicomponents::ToolBarItem* makeLocalItem(const muse::actions::ActionCode& actionCode);

    bool m_inited = false;
};
}
