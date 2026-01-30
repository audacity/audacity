/*
* Audacity: A Digital Audio Editor
*/
#include "audiosetupcontextmenumodel.h"

#include "playback/playbacktypes.h"

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

void setItemsEnabled(const MenuItemList& items, bool enabled)
{
    for (MenuItem* item : items) {
        if (!item) {
            continue;
        }
        auto state = item->state();
        state.enabled = enabled;
        item->setState(state);
    }
}
}

void AudioSetupContextMenuModel::load()
{
    AbstractMenuModel::load();

    makeMenuItems();

    if (auto playbackState = globalContext()->playbackState()) {
        playbackState->playbackStatusChanged().onReceive(this, [this](au::playback::PlaybackStatus) {
            makeMenuItems();
        }, muse::async::Asyncable::Mode::SetReplace);
    }

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        makeMenuItems();
    }, muse::async::Asyncable::Mode::SetReplace);
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
    const bool allowChanges = (!globalContext()->playbackState()
                               || !globalContext()->playbackState()->isPlaying())
                              && !globalContext()->isRecording();
    MenuItemList hostItems = makeHostItems();
    MenuItemList playbackItems = makePlaybackDevicesItems();
    MenuItemList recordingItems = makeRecordingDevicesItems();
    MenuItemList inputChannelItems = makeInputChannelsItems();

    setItemsEnabled(hostItems, allowChanges);
    setItemsEnabled(playbackItems, allowChanges);
    setItemsEnabled(recordingItems, allowChanges);
    setItemsEnabled(inputChannelItems, allowChanges);

    MenuItemList items {
        makeMenu(muse::TranslatableString("audio setup", "Host"), hostItems, "hostMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Playback device"), playbackItems, "playbackDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording device"), recordingItems, "recordingDeviceMenu"),
        makeMenu(muse::TranslatableString("audio setup", "Recording channels"), inputChannelItems, "inputChannelsMenu"),
        makeMenuItem("rescan-devices"),
        makeMenuItem("audio-settings")
    };

    setItems(items);
}

MenuItemList AudioSetupContextMenuModel::makeHostItems()
{
    MenuItemList items;
    auto currentApi = audioDevicesProvider()->currentApi();

    auto makeChangeApiAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_AUDIO_API_QUERY;
        q.addParam("api_index", muse::Val(index));
        return q;
    };

    const auto& apiList = audioDevicesProvider()->apis();
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
    auto currentOutputDevice = audioDevicesProvider()->currentOutputDevice();

    auto makeChangePlaybackDeviceAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY;
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    const auto& outputDevicesList = audioDevicesProvider()->outputDevices();
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
    auto currentInputDevice = audioDevicesProvider()->currentInputDevice();

    auto makeChangeRecordingDeviceAction = [](int index) {
        ActionQuery q = PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY;
        q.addParam("device_index", muse::Val(index));
        return q;
    };

    const auto& inputDevicesList = audioDevicesProvider()->inputDevices();
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

MenuItemList AudioSetupContextMenuModel::makeInputChannelsItems()
{
    MenuItemList items;
    int inputChannelsSelected = audioDevicesProvider()->inputChannelsSelected();
    int inputChannelsAvailable = audioDevicesProvider()->inputChannelsAvailable();

    auto makeChangeInputChannelsAction = [](int index) -> ActionQuery {
        ActionQuery q = PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY;
        q.addParam("input-channels_index", muse::Val(index));
        return q;
    };

    auto channelName = [](int channelNumber) -> QString {
        return channelNumber == 1
               ? muse::qtrc("projectscene/toolbars", "%1 (Mono) Recording channel").arg(channelNumber)
               : channelNumber == 2
               ? muse::qtrc("projectscene/toolbars", "%1 (Stereo) Recording channels").arg(channelNumber)
               : QString::number(channelNumber);
    };

    for (int i = 0; i < inputChannelsAvailable; ++i) {
        int channelNumber = i + 1;
        MenuItem* item = makeMenuItem(makeChangeInputChannelsAction(channelNumber).toString(),
                                      muse::TranslatableString::untranslatable(channelName(channelNumber)));

        item->setId(QString::fromStdString(item->query().toString()));

        if (inputChannelsSelected == (channelNumber)) {
            item->setChecked(true);
        }
        items << item;
    }

    return items;
}
