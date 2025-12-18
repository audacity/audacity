/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "iprojectsceneconfiguration.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"

namespace au::projectscene {
class TrackContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<trackedit::ISelectionController> selectionController;

    Q_PROPERTY(trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

public:
    TrackContextMenuModel() = default;

    Q_INVOKABLE void load() override;

    trackedit::TrackId trackId() const;
    void setTrackId(const trackedit::TrackId& newTrackId);
    void handleMenuItem(const QString& itemId) override;

signals:
    void trackIdChanged();
    void trackRenameRequested();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    muse::uicomponents::MenuItemList makeMonoTrackItems();
    muse::uicomponents::MenuItemList makeStereoTrackItems();
    muse::uicomponents::MenuItemList makeLabelTrackItems();

    muse::uicomponents::MenuItemList makeTrackColorItems();
    muse::uicomponents::MenuItemList makeTrackFormatItems();
    muse::uicomponents::MenuItemList makeTrackRateItems();
    muse::uicomponents::MenuItemList makeTrackMoveItems();
    muse::uicomponents::MenuItemList makeTrackViewItems();
    muse::uicomponents::MenuItemList makeMeterMonitoringItems();

    muse::uicomponents::MenuItem* makeItemWithArg(const muse::actions::ActionCode& actionCode);

    void updateColorCheckedState();
    void updateTrackFormatState();
    void updateTrackRateState();
    void updateTrackMonoState();
    void updateTrackViewCheckedState();

    trackedit::TrackId m_trackId;
    muse::actions::ActionCodeList m_colorChangeActionCodeList;
    muse::actions::ActionCodeList m_trackViewTypeChangeActionCodeList;
};
}
