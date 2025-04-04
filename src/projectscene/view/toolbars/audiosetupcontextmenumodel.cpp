/*
* Audacity: A Digital Audio Editor
*/
#include "audiosetupcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

namespace {
bool containsAny(const ActionCodeList& list, const ActionCodeList& actionCodes)
{
    return std::any_of(actionCodes.begin(), actionCodes.end(), [&](ActionCode code) {
        return std::find(list.begin(), list.end(), code) != list.end();
    });
}
}

void AudioSetupContextMenuModel::load()
{
    AbstractMenuModel::load();

    makeMenuItems();
}

void AudioSetupContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);

    auto audioSetupCodeList = { ActionCode("action://playback/change-api"), ActionCode("action://playback/change-playback-device"),
                                ActionCode("action://playback/change-recording-device"), ActionCode(
                                    "action://playback/change-input-channels") };

    if (containsAny(codes, audioSetupCodeList)) {
        //! NOTE: changing audio setup may change available items within context menu
        makeMenuItems();
    }
}

void AudioSetupContextMenuModel::makeMenuItems()
{
    MenuItemList items {
        makeMenu(muse::TranslatableString("audio setup", "Host"), makeHostItems(), "hostMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Playback device"), makePlaybackDevicesItems(), "playbackDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording device"), makeRecordingDevicesItems(), "recordingDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording channels"), makeInputChannelsItems(), "inputChannelsMenu"),
        makeMenuItem("rescan-devices"),
        makeMenuItem("audio-settings")
    };

    setItems(items);
}

MenuItemList AudioSetupContextMenuModel::makeHostItems()
{
    MenuItemList items;
    auto currentApi = audioDevicesProvider()->currentAudioApi();

    auto makeChangeApiAction = [](int index) {
        ActionQuery q("action://playback/change-api");
        q.addParam("api_index", muse::Val(index));
        return q;
    };

    const auto& apiList = audioDevicesProvider()->audioApiList();
    for (int i = 0; i < apiList.size(); ++i) {
        MenuItem* item = makeMenuItem(makeChangeApiAction(i).toString(),
                                      muse::TranslatableString::untranslatable(muse::String::fromStdString(apiList.at(i))));
        item->setId(QString::fromStdString(item->query().toString()));
        if (currentApi == apiList.at(i)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}

MenuItemList AudioSetupContextMenuModel::makePlaybackDevicesItems()
{
    MenuItemList items;
    auto currentOutputDevice = audioDevicesProvider()->currentAudioOutputDevice();

    auto makeChangePlaybackDeviceAction = [](int index) {
        ActionQuery q("action://playback/change-playback-device");
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    const auto& outputDevicesList = audioDevicesProvider()->audioOutputDevices();
    for (int i = 0; i < outputDevicesList.size(); ++i) {
        MenuItem* item = makeMenuItem(makeChangePlaybackDeviceAction(i).toString(),
                                      muse::TranslatableString::untranslatable(muse::String::fromStdString(outputDevicesList.at(i))));
        item->setId(QString::fromStdString(item->query().toString()));
        if (currentOutputDevice == outputDevicesList.at(i)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}

MenuItemList AudioSetupContextMenuModel::makeRecordingDevicesItems()
{
    MenuItemList items;
    auto currentInputDevice = audioDevicesProvider()->currentAudioInputDevice();

    auto makeChangeRecordingDeviceAction = [](int index) {
        ActionQuery q("action://playback/change-recording-device");
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    const auto& inputDevicesList = audioDevicesProvider()->audioInputDevices();
    for (int i = 0; i < inputDevicesList.size(); ++i) {
        MenuItem* item = makeMenuItem(makeChangeRecordingDeviceAction(i).toString(),
                                      muse::TranslatableString::untranslatable(muse::String::fromStdString(inputDevicesList.at(i))));
        item->setId(QString::fromStdString(item->query().toString()));
        if (currentInputDevice == inputDevicesList.at(i)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}

MenuItemList AudioSetupContextMenuModel::makeInputChannelsItems()
{
    MenuItemList items;
    auto currentInputChannels = audioDevicesProvider()->currentInputChannels();

    auto makeChangeInputChannelsAction = [](int index) {
        ActionQuery q("action://playback/change-input-channels");
        q.addParam("input-channels_index", muse::Val(index));
        return q;
    };

    const auto& inputChannelsList = audioDevicesProvider()->inputChannelsList();
    for (int i = 0; i < inputChannelsList.size(); ++i) {
        MenuItem* item = makeMenuItem(makeChangeInputChannelsAction(i).toString(),
                                      muse::TranslatableString::untranslatable(muse::String::fromStdString(inputChannelsList.at(i))));
        item->setId(QString::fromStdString(item->query().toString()));
        if (currentInputChannels == inputChannelsList.at(i)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}
