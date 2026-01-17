/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::projectscene {
class AudioSetupContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider{ this };
    muse::Inject<context::IGlobalContext> globalContext{ this };

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
