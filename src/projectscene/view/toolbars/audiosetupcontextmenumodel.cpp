/*
* Audacity: A Digital Audio Editor
*/
#include "audiosetupcontextmenumodel.h"

#include "framework/global/containers.h"

#include "appshell/appshellcommands.h"
#include "playback/playbackcommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::rcommand;

void AudioSetupContextMenuModel::load()
{
    AbstractMenuModel::load();

    makeMenuItems();

    audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta&) {
        makeMenuItems();
    });
    audioDriverController()->audioDeviceListChanged().onNotify(this, [this]() {
        makeMenuItems();
    });
}

void AudioSetupContextMenuModel::handleMenuItem(const QString& itemId)
{
    const MenuItem& item = findItem(itemId);
    if (!item.isValid()) {
        return;
    }

    //! NOTE: the check mark is switched optimistically by the menu; rebuild from the real
    //! configuration when the change was rejected, as a successful change rebuilds via configurationChanged
    commandDispatcher()->dispatch(item.commandQuery()).onResolve(this, [this](const Response& response) {
        if (!response.ret) {
            makeMenuItems();
        }
    });
}

void AudioSetupContextMenuModel::makeMenuItems()
{
    MenuItemList items {
        makeMenu(muse::TranslatableString("audio setup", "Host"), makeHostItems(), "hostMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Playback device"), makePlaybackDevicesItems(), "playbackDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording device"), makeRecordingDevicesItems(), "recordingDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording channels"), makeInputChannelsItems(), "inputChannelsMenu"),
        makeMenuItem(playback::PLAYBACK_RESCAN_DEVICES_COMMAND),
        makeMenuItem(appshell::APP_AUDIO_SETTINGS_COMMAND)
    };

    setItems(items);
}

MenuItemList AudioSetupContextMenuModel::makeHostItems()
{
    MenuItemList items;
    auto currentApi = audioDriverController()->configuration().api;

    const auto& apiList = audioDriverController()->apis();
    for (size_t i = 0; i < apiList.size(); ++i) {
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_AUDIO_API_COMMAND, {
            { playback::PLAYBACK_CHANGE_AUDIO_API_INDEX_PARAM, muse::Val(static_cast<int>(i)) },
        }), muse::TranslatableString::untranslatable(muse::String::fromStdString(apiList.at(i))));
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
    auto currentOutputDevice = audioDriverController()->configuration().outputDevice;

    const auto& outputDevicesList = audioDriverController()->outputDevices();
    const bool usesSystemDefaultOutput = !currentOutputDevice.has_value()
                                         || !muse::contains(outputDevicesList, currentOutputDevice.value());
    if (!outputDevicesList.empty()) {
        const std::string resolvedDevice
            = audioDriverController()->systemDefaultOutputDevice(audioDriverController()->configuration().api);
        const muse::TranslatableString title = resolvedDevice.empty()
                                               ? muse::TranslatableString("audio setup", "System default")
                                               : muse::TranslatableString("audio setup", "System default: %1")
                                               .arg(muse::String::fromStdString(resolvedDevice));
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND, {
            { playback::PLAYBACK_CHANGE_PLAYBACK_DEVICE_IS_DEFAULT_PARAM, muse::Val(true) },
        }), title);
        if (usesSystemDefaultOutput) {
            item->setChecked(true);
        }
        items << item;
        items << makeSeparator();
    }

    for (size_t i = 0; i < outputDevicesList.size(); ++i) {
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND, {
            { playback::PLAYBACK_CHANGE_PLAYBACK_DEVICE_INDEX_PARAM, muse::Val(static_cast<int>(i)) },
        }), muse::TranslatableString::untranslatable(muse::String::fromStdString(outputDevicesList.at(i))));
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
    auto currentInputDevice = audioDriverController()->configuration().inputDevice;

    const auto& inputDevicesList = audioDriverController()->inputDevices();
    const bool usesSystemDefaultInput = !currentInputDevice.has_value()
                                        || !muse::contains(inputDevicesList, *currentInputDevice);
    if (!inputDevicesList.empty()) {
        const std::string resolvedDevice
            = audioDriverController()->systemDefaultInputDevice(audioDriverController()->configuration().api);
        //: %1 is the device the system default currently resolves to
        const muse::TranslatableString title = resolvedDevice.empty()
                                               ? muse::TranslatableString("audio setup", "System default")
                                               : muse::TranslatableString("audio setup", "System default: %1")
                                               .arg(muse::String::fromStdString(resolvedDevice));
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND, {
            { playback::PLAYBACK_CHANGE_RECORDING_DEVICE_IS_DEFAULT_PARAM, muse::Val(true) },
        }), title);
        if (usesSystemDefaultInput) {
            item->setChecked(true);
        }
        items << item;
        items << makeSeparator();
    }

    for (size_t i = 0; i < inputDevicesList.size(); ++i) {
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND, {
            { playback::PLAYBACK_CHANGE_RECORDING_DEVICE_INDEX_PARAM, muse::Val(static_cast<int>(i)) },
        }), muse::TranslatableString::untranslatable(muse::String::fromStdString(inputDevicesList.at(i))));
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
    int inputChannelsSelected = audioDriverController()->configuration().inputChannels;
    int inputChannelsAvailable = audioDriverController()->inputChannelsAvailable();

    auto channelName = [](int channelNumber) -> QString {
        return channelNumber == 1
               //: %1 is the recording channel number
               ? muse::qtrc("projectscene/toolbars", "%1 (Mono) Recording channel").arg(channelNumber)
               : channelNumber == 2
               //: %1 is the recording channel number
               ? muse::qtrc("projectscene/toolbars", "%1 (Stereo) Recording channels").arg(channelNumber)
               : QString::number(channelNumber);
    };

    for (int i = 0; i < inputChannelsAvailable; ++i) {
        int channelNumber = i + 1;
        MenuItem* item = makeMenuItem(make_query(playback::PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND, {
            { playback::PLAYBACK_CHANGE_INPUT_CHANNELS_INDEX_PARAM, muse::Val(channelNumber) },
        }), muse::TranslatableString::untranslatable(channelName(channelNumber)));

        if (inputChannelsSelected == (channelNumber)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}
