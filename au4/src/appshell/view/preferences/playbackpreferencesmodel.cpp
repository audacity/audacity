/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "playbackpreferencesmodel.h"

#include "log.h"

using namespace au::appshell;
using namespace muse::audio;
// using namespace mu::midi;

PlaybackPreferencesModel::PlaybackPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

int PlaybackPreferencesModel::currentAudioApiIndex() const
{
    QString currentApi = QString::fromStdString(audioDevicesProvider()->currentAudioApi());
    return audioApiList().indexOf(currentApi);
}

void PlaybackPreferencesModel::setCurrentAudioApiIndex(int index)
{
    if (index == currentAudioApiIndex()) {
        return;
    }

    std::vector<std::string> apiList = audioDevicesProvider()->audioApiList();
    if (index < 0 || index >= static_cast<int>(apiList.size())) {
        return;
    }

    audioDevicesProvider()->setAudioApi(apiList[index]);
}

// QString PlaybackPreferencesModel::midiInputDeviceId() const
// {
//     return QString::fromStdString(midiInPort()->deviceID());
// }

// void PlaybackPreferencesModel::inputDeviceSelected(const QString& deviceId)
// {
//     midiConfiguration()->setMidiInputDeviceId(deviceId.toStdString());
// }

// QString PlaybackPreferencesModel::midiOutputDeviceId() const
// {
//     return QString::fromStdString(midiOutPort()->deviceID());
// }

// void PlaybackPreferencesModel::outputDeviceSelected(const QString& deviceId)
// {
//     midiConfiguration()->setMidiOutputDeviceId(deviceId.toStdString());
// }

void PlaybackPreferencesModel::init()
{
    // midiInPort()->availableDevicesChanged().onNotify(this, [this]() {
    //     emit midiInputDevicesChanged();
    // });

    // midiInPort()->deviceChanged().onNotify(this, [this]() {
    //     emit midiInputDeviceIdChanged();
    // });

    // midiOutPort()->availableDevicesChanged().onNotify(this, [this]() {
    //     emit midiOutputDevicesChanged();
    // });

    // midiOutPort()->deviceChanged().onNotify(this, [this]() {
    //     emit midiOutputDeviceIdChanged();
    // });

    // playbackConfiguration()->muteHiddenInstrumentsChanged().onReceive(this, [this](bool mute) {
    //     emit muteHiddenInstrumentsChanged(mute);
    // });
}

QStringList PlaybackPreferencesModel::audioApiList() const
{
    QStringList result;
    for (const std::string& api: audioDevicesProvider()->audioApiList()) {
        result.push_back(QString::fromStdString(api));
    }

    return result;
}

// void PlaybackPreferencesModel::restartAudioAndMidiDevices()
// {
//     NOT_IMPLEMENTED;
// }

// QVariantList PlaybackPreferencesModel::midiInputDevices() const
// {
//     QVariantList result;

//     std::vector<MidiDevice> devices = midiInPort()->availableDevices();
//     for (const MidiDevice& device : devices) {
//         QVariantMap obj;
//         obj["value"] = QString::fromStdString(device.id);
//         obj["text"] = QString::fromStdString(device.name);

//         result << obj;
//     }

//     return result;
// }

// QVariantList PlaybackPreferencesModel::midiOutputDevices() const
// {
//     QVariantList result;

//     std::vector<MidiDevice> devices = midiOutPort()->availableDevices();
//     for (const MidiDevice& device : devices) {
//         QVariantMap obj;
//         obj["value"] = QString::fromStdString(device.id);
//         obj["text"] = QString::fromStdString(device.name);

//         result << obj;
//     }

//     return result;
// }

// bool PlaybackPreferencesModel::isMIDI20OutputSupported() const
// {
//     return midiOutPort()->supportsMIDI20Output();
// }

// bool PlaybackPreferencesModel::useMIDI20Output() const
// {
//     return midiConfiguration()->useMIDI20Output();
// }

// void PlaybackPreferencesModel::setUseMIDI20Output(bool use)
// {
//     if (use == useMIDI20Output()) {
//         return;
//     }

//     midiConfiguration()->setUseMIDI20Output(use);
//     emit useMIDI20OutputChanged();
// }

// void PlaybackPreferencesModel::showMidiError(const MidiDeviceID& deviceId, const std::string& text) const
// {
//     // todo: display error
//     LOGE() << "failed connect to device, deviceID: " << deviceId << ", err: " << text;
// }

// bool PlaybackPreferencesModel::muteHiddenInstruments() const
// {
//     return playbackConfiguration()->muteHiddenInstruments();
// }

// void PlaybackPreferencesModel::setMuteHiddenInstruments(bool mute)
// {
//     if (mute == muteHiddenInstruments()) {
//         return;
//     }

//     playbackConfiguration()->setMuteHiddenInstruments(mute);
// }
