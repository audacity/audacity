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

#include "containers.h"
#include "log.h"
#include "types/translatablestring.h"

using namespace au::appshell;

namespace {
QString toSampleRateName(uint64_t sampleRate)
{
    return QString::number(sampleRate) + " Hz";
}
}

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent)
{
}

void CommonAudioApiConfigurationModel::load()
{
    audioDevicesProvider()->audioApiChanged().onNotify(this, [this]() {
        emit currentAudioApiIndexChanged();
        emit outputDeviceListChanged();
        emit inputDeviceListChanged();
        emit longestDeviceNameLengthChanged();

        emit currentOutputDeviceIdChanged();
        emit currentInputDeviceIdChanged();

        emit inputChannelsListChanged();
        emit currentInputChannelsChanged();
    });
    audioDevicesProvider()->audioOutputDeviceChanged().onNotify(this, [this]() { emit currentOutputDeviceIdChanged(); });
    audioDevicesProvider()->audioInputDeviceChanged().onNotify(this, [this]() { emit currentInputDeviceIdChanged(); });
    audioDevicesProvider()->inputChannelsListChanged().onNotify(this, [this](){ emit inputChannelsListChanged(); });
    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this](){ emit currentInputChannelsChanged(); });
    audioDevicesProvider()->bufferLengthChanged().onNotify(this, [this](){ emit bufferLengthChanged(); });
    audioDevicesProvider()->latencyCompensationChanged().onNotify(this, [this](){ emit latencyCompensationChanged(); });
    audioDevicesProvider()->defaultSampleRateChanged().onNotify(this, [this](){
        if (m_otherSampleRate) {
            emit defaultSampleRateValueChanged();
        } else {
            emit defaultSampleRateChanged();
        }
    });
    audioDevicesProvider()->defaultSampleFormatChanged().onNotify(this, [this](){ emit defaultSampleFormatChanged(); });
}

int CommonAudioApiConfigurationModel::currentAudioApiIndex() const
{
    QString currentApi = QString::fromStdString(audioDevicesProvider()->currentAudioApi());
    return audioApiList().indexOf(currentApi);
}

void CommonAudioApiConfigurationModel::setCurrentAudioApiIndex(int index)
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

QStringList CommonAudioApiConfigurationModel::audioApiList() const
{
    QStringList result;
    for (const std::string& api: audioDevicesProvider()->audioApiList()) {
        result.push_back(QString::fromStdString(api));
    }

    return result;
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
    if (deviceId == currentInputDeviceId()) {
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

QString CommonAudioApiConfigurationModel::defaultSampleRate() const
{
    auto currentSampleRate = audioDevicesProvider()->defaultSampleRate();
    if (!m_otherSampleRate) {
        for (const auto& rate : m_sampleRateMapping) {
            if (currentSampleRate == rate.first) {
                return rate.second;
            }
        }
    }

    return muse::TranslatableString("preferences", "Other").translated().toQString();
}

QVariantList CommonAudioApiConfigurationModel::defaultSampleRateList()
{
    QVariantList result;
    m_sampleRateMapping.clear();
    for (const auto& rate : audioDevicesProvider()->availableSampleRateList()) {
        QString sampleRateName = toSampleRateName(rate);
        m_sampleRateMapping.push_back(std::make_pair(rate, sampleRateName));
        result << QVariant::fromValue(sampleRateName);
    }

    result << QVariant::fromValue(muse::TranslatableString("preferences", "Other").translated().toQString());

    return result;
}

void CommonAudioApiConfigurationModel::defaultSampleRateSelected(const QString& rateName)
{
    if (rateName == defaultSampleRate()) {
        return;
    }

    auto it = std::find_if(m_sampleRateMapping.begin(), m_sampleRateMapping.end(),
                           [&rateName](const auto& rate) { return rateName == rate.second; });
    if (it != m_sampleRateMapping.end()) {
        setOtherSampleRate(false);
        audioDevicesProvider()->setDefaultSampleRate(it->first);
        return;
    }

    setOtherSampleRate(true);
    emit defaultSampleRateChanged();
    emit defaultSampleRateValueChanged();
}

uint64_t CommonAudioApiConfigurationModel::defaultSampleRateValue() const
{
    return audioDevicesProvider()->defaultSampleRate();
}

void CommonAudioApiConfigurationModel::defaultSampleRateValueSelected(uint64_t rateValue)
{
    if (rateValue == defaultSampleRateValue()) {
        return;
    }

    audioDevicesProvider()->setDefaultSampleRate(rateValue);
}

bool CommonAudioApiConfigurationModel::otherSampleRate() const
{
    return m_otherSampleRate;
}

void CommonAudioApiConfigurationModel::setOtherSampleRate(bool other)
{
    if (m_otherSampleRate == other) {
        return;
    }

    m_otherSampleRate = other;
    emit otherSampleRateChanged();
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

double CommonAudioApiConfigurationModel::longestDeviceNameLength() const
{
    QString longest;
    for (const auto& str : inputDeviceList()) {
        if (str.toString().length() > longest.length()) {
            longest = str.toString();
        }
    }

    for (const auto& str : outputDeviceList()) {
        if (str.toString().length() > longest.length()) {
            longest = str.toString();
        }
    }

    QFont font;
    font.setFamily(QString::fromStdString(uiConfiguration()->fontFamily()));
    font.setPointSize(uiConfiguration()->fontSize());
    QFontMetrics metrics(font);

    return metrics.horizontalAdvance(longest);
}
