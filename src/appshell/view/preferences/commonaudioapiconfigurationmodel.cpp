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
    audioDevicesProvider()->inputChannelsListChanged().onNotify(this, [this](){ emit inputChannelsListChanged(); });
    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this](){ emit currentInputChannelsChanged(); });
    audioDevicesProvider()->bufferLengthChanged().onNotify(this, [this](){ emit bufferLengthChanged(); });
    audioDevicesProvider()->latencyCompensationChanged().onNotify(this, [this](){ emit latencyCompensationChanged(); });
    audioDevicesProvider()->defaultSampleRateChanged().onNotify(this, [this](){ emit defaultSampleRateChanged(); });
    audioDevicesProvider()->defaultSampleFormatChanged().onNotify(this, [this](){ emit defaultSampleFormatChanged(); });
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

double CommonAudioApiConfigurationModel::bufferLength() const
{
    return audioDevicesProvider()->bufferLength();
}

void CommonAudioApiConfigurationModel::bufferLengthSelected(const QString& bufferLengthStr)
{
    if (bufferLengthStr == QString::number(bufferLength())) {
        return;
    }

    audioDevicesProvider()->setBufferLength(bufferLengthStr.toDouble());
}

double CommonAudioApiConfigurationModel::latencyCompensation() const
{
    return audioDevicesProvider()->latencyCompensation();
}

void CommonAudioApiConfigurationModel::latencyCompensationSelected(
    const QString& latencyCompensationStr)
{
    if (latencyCompensationStr == QString::number(latencyCompensation())) {
        return;
    }

    audioDevicesProvider()->setLatencyCompensation(latencyCompensationStr.toDouble());
}

QString CommonAudioApiConfigurationModel::currentInputChannels() const
{
    return QString::fromStdString(audioDevicesProvider()->currentInputChannels());
}

QVariantList CommonAudioApiConfigurationModel::inputChannelsList() const
{
    QVariantList result;
    for (const auto& channel : audioDevicesProvider()->inputChannelsList()) {
        result << QString::fromStdString(channel);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputChannelsSelected(const QString& channelsStr)
{
    if (channelsStr == currentInputChannels()) {
        return;
    }

    audioDevicesProvider()->setInputChannels(channelsStr.toStdString());
}

uint64_t CommonAudioApiConfigurationModel::defaultSampleRate() const
{
    return audioDevicesProvider()->defaultSampleRate();
}

QVariantList CommonAudioApiConfigurationModel::defaultSampleRateList() const
{
    QVariantList result;
    for (const auto& rate : audioDevicesProvider()->defaultSampleRateList()) {
        result << rate;
    }

    return result;
}

void CommonAudioApiConfigurationModel::defaultSampleRateSelected(uint64_t rate)
{
    if (rate == defaultSampleRate()) {
        return;
    }

    audioDevicesProvider()->setDefaultSampleRate(rate);
}

QString CommonAudioApiConfigurationModel::defaultSampleFormat() const
{
    return QString::fromStdString(audioDevicesProvider()->defaultSampleFormat());
}

QVariantList CommonAudioApiConfigurationModel::defaultSampleFormatList() const
{
    QVariantList result;
    for (const auto& format : audioDevicesProvider()->defaultSampleFormatList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

void CommonAudioApiConfigurationModel::defaultSampleFormatSelected(const QString& format)
{
    if (format == defaultSampleFormat()) {
        return;
    }

    audioDevicesProvider()->setDefaultSampleFormat(format.toStdString());
}
