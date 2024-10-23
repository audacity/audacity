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

#include "commonaudioapiconfigurationmodel.h"

#include "log.h"

using namespace au::appshell;

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent)
{
}

void CommonAudioApiConfigurationModel::load()
{
    audioDevicesProvider()->audioOutputDeviceChanged().onNotify(this, [this]() { emit currentOutputDeviceIdChanged(); });
    audioDevicesProvider()->audioInputDeviceChanged().onNotify(this, [this]() { emit currentInputDeviceIdChanged(); });
    audioDevicesProvider()->audioApiChanged().onNotify(this, [this](){ emit outputDeviceListChanged(); });
}

QString CommonAudioApiConfigurationModel::currentOutputDeviceId() const
{
    return QString::fromStdString(audioDevicesProvider()->currentAudioOutputDevice());
}

QVariantList CommonAudioApiConfigurationModel::outputDeviceList() const
{
    QVariantList result;
    for (const auto& device : audioDevicesProvider()->audioOutputDevices()) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::outputDeviceSelected(const QString& deviceId)
{
    if (deviceId == currentOutputDeviceId()) {
        return;
    }
    audioDevicesProvider()->setAudioOutputDevice(deviceId.toStdString());
}

QString CommonAudioApiConfigurationModel::currentInputDeviceId() const
{
    return QString::fromStdString(audioDevicesProvider()->currentAudioInputDevice());
}

QVariantList CommonAudioApiConfigurationModel::inputDeviceList() const
{
    QVariantList result;
    for (const auto& device : audioDevicesProvider()->audioInputDevices()) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputDeviceSelected(const QString& deviceId)
{
    if (deviceId == currentOutputDeviceId()) {
        return;
    }
    audioDevicesProvider()->setAudioInputDevice(deviceId.toStdString());
}

unsigned int CommonAudioApiConfigurationModel::bufferSize() const
{
//     // return audioDriver()->outputDeviceBufferSize();
    return 1024;
}

QList<unsigned int> CommonAudioApiConfigurationModel::bufferSizeList() const
{
    QList<unsigned int> result;
//     std::vector<unsigned int> bufferSizes = audioDriver()->availableOutputDeviceBufferSizes();

//     for (unsigned int bufferSize : bufferSizes) {
//         result << bufferSize;
//     }

//     std::sort(result.begin(), result.end());

    return result;
}

void CommonAudioApiConfigurationModel::bufferSizeSelected(const QString& bufferSizeStr)
{
    UNUSED(bufferSizeStr);
// audioConfiguration()->setDriverBufferSize(bufferSizeStr.toInt());
}
