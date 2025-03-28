/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "context/iglobalcontext.h"
#include "uicomponents/view/abstractmenumodel.h"

#include "playback/iaudiodevicesprovider.h"

namespace au::projectscene {
class AudioSetupContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    AudioSetupContextMenuModel() = default;

    Q_INVOKABLE void load() override;

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    void makeMenuItems();

    muse::uicomponents::MenuItemList makeHostItems();
    muse::uicomponents::MenuItemList makePlaybackDevicesItems();
    muse::uicomponents::MenuItemList makeRecordingDevicesItems();
    muse::uicomponents::MenuItemList makeInputChannelsItems();
};
}
