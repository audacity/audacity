/*
* Audacity: A Digital Audio Editor
*/
#include "audiosetupcontextmenumodel.h"

#include "framework/global/containers.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

static const ActionQuery PLAYBACK_CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");
static const ActionQuery PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY("action://playback/change-input-channels");

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

    audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta&) {
        makeMenuItems();
    });
    audioDriverController()->audioDeviceListChanged().onNotify(this, [this]() {
        makeMenuItems();
    });
}

void AudioSetupContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);

    // here we use ActionQuery toString() to create ActionCodeList, update once we have ActionQueryList
    auto audioSetupCodeList = { PLAYBACK_CHANGE_AUDIO_API_QUERY.toString(),
                                PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString(),
                                PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString(),
                                PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString() };

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
        makeInputChannelsMenu(),
        makeMenuItem("rescan-devices"),
        makeMenuItem("audio-settings")
    };

    setItems(items);
}

MenuItemList AudioSetupContextMenuModel::makeHostItems()
{
    MenuItemList items;
    auto currentApi = audioDriverController()->configuration().api;

    auto makeChangeApiAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_AUDIO_API_QUERY;
        q.addParam("api_index", muse::Val(index));
        return q;
    };

    const auto& apiList = audioDriverController()->apis();
    for (size_t i = 0; i < apiList.size(); ++i) {
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
    auto currentOutputDevice = audioDriverController()->configuration().outputDevice;

    auto makeChangePlaybackDeviceAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY;
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    auto makeSystemDefaultPlaybackDeviceAction = []() {
        ActionQuery q = PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY;
        q.addParam("is_default_device", muse::Val(true));
        return q;
    };

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
        MenuItem* item = makeMenuItem(makeSystemDefaultPlaybackDeviceAction().toString(), title);
        item->setId(QString::fromStdString(item->query().toString()));
        if (usesSystemDefaultOutput) {
            item->setChecked(true);
        }
        items << item;
        items << makeSeparator();
    }

    for (size_t i = 0; i < outputDevicesList.size(); ++i) {
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
    auto currentInputDevice = audioDriverController()->configuration().inputDevice;

    auto makeChangeRecordingDeviceAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY;
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    auto makeSystemDefaultRecordingDeviceAction = []() {
        ActionQuery q = PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY;
        q.addParam("is_default_device", muse::Val(true));
        return q;
    };

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
        MenuItem* item = makeMenuItem(makeSystemDefaultRecordingDeviceAction().toString(), title);
        item->setId(QString::fromStdString(item->query().toString()));
        if (usesSystemDefaultInput) {
            item->setChecked(true);
        }
        items << item;
        items << makeSeparator();
    }

    for (size_t i = 0; i < inputDevicesList.size(); ++i) {
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

MenuItem* AudioSetupContextMenuModel::makeInputChannelsMenu()
{
    MenuItemList items;
    const auto& selection = audioDriverController()->configuration().inputChannelSelection;
    const int inputChannelsAvailable = audioDriverController()->inputChannelsAvailable();

    auto makeChangeInputChannelsAction = [](int channelCount) -> ActionQuery {
        ActionQuery q = PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY;
        q.addParam("input-channels_index", muse::Val(channelCount));
        return q;
    };

    auto channelName = [](int channelCount) -> muse::TranslatableString {
        if (channelCount == 1) {
            //: %1 is the recording channel count
            return muse::TranslatableString("projectscene/toolbars", "%1 (Mono) Recording channel").arg(channelCount);
        }
        if (channelCount == 2) {
            //: %1 is the recording channel count
            return muse::TranslatableString("projectscene/toolbars", "%1 (Stereo) Recording channels").arg(channelCount);
        }
        return muse::TranslatableString::untranslatable(QString::number(channelCount));
    };

    bool hasCheckedPreset = false;
    for (int channelCount = 1; channelCount <= inputChannelsAvailable; ++channelCount) {
        MenuItem* item = makeMenuItem(makeChangeInputChannelsAction(channelCount).toString(), channelName(channelCount));

        item->setId(QString::fromStdString(item->query().toString()));
        item->setCheckable(true);
        item->setChecked(selection == audio::legacyInputChannelSelection(channelCount));
        hasCheckedPreset = hasCheckedPreset || item->checked();
        items << item;
    }

    if (!items.empty()) {
        items << makeSeparator();
    }
    MenuItem* customItem = makeMenuItem("audio-settings", muse::TranslatableString("audio setup", "Custom..."));
    customItem->setId("customInputChannels");
    customItem->setCheckable(false);
    customItem->setChecked(false);
    items << customItem;

    const auto title = inputChannelsAvailable > 0 && !selection.empty() && !hasCheckedPreset
                       ? muse::TranslatableString("audio setup", "Recording channels: Custom")
                       : muse::TranslatableString("audio setup", "Recording channels");
    return makeMenu(title, items, "inputChannelsMenu");
}
