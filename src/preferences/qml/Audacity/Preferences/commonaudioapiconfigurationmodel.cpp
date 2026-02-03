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

#include <algorithm>

#include "actions/actiontypes.h"
#include "containers.h"
#include "log.h"
#include "types/translatablestring.h"

using namespace au::appshell;
using namespace muse::actions;

static const ActionQuery PLAYBACK_CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");

namespace {
QString toSampleRateName(uint64_t sampleRate)
{
    return QString::number(sampleRate) + " Hz";
}

QString channelName(int channelNumber)
{
    return channelNumber == 1
           ? muse::qtrc("preferences", "%1 (Mono) Recording channel").arg(channelNumber)
           : channelNumber == 2
           ? muse::qtrc("preferences", "%1 (Stereo) Recording channels").arg(channelNumber)
           : QString::number(channelNumber);
}
}

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void CommonAudioApiConfigurationModel::load()
{
    LOGI() << "CommonAudioApiConfigurationModel load";
    audioDevicesProvider()->apiChanged().onNotify(this, [this]() {
        emit currentAudioApiIndexChanged();
        emit outputDeviceListChanged();
        emit inputDeviceListChanged();
        emit longestDeviceNameLengthChanged();

        emit currentOutputDeviceIdChanged();
        emit currentInputDeviceIdChanged();

        emit inputChannelsListChanged();
        emit currentInputChannelsSelectedChanged();
    });
    audioDevicesProvider()->outputDeviceChanged().onNotify(this, [this]() { emit currentOutputDeviceIdChanged(); });
    audioDevicesProvider()->inputDeviceChanged().onNotify(this, [this]() { emit currentInputDeviceIdChanged(); });
    audioDevicesProvider()->inputChannelsAvailableChanged().onNotify(this, [this](){ emit inputChannelsListChanged(); });
    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this](){ emit currentInputChannelsSelectedChanged(); });
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
    QString currentApi = QString::fromStdString(audioDevicesProvider()->currentApi());
    return audioApiList().indexOf(currentApi);
}

void CommonAudioApiConfigurationModel::setCurrentAudioApiIndex(int index)
{
    if (index == currentAudioApiIndex()) {
        return;
    }

    std::vector<std::string> apiList = audioDevicesProvider()->apis();
    if (index < 0 || index >= static_cast<int>(apiList.size())) {
        return;
    }

    ActionQuery q = PLAYBACK_CHANGE_AUDIO_API_QUERY;
    q.addParam("api_index", muse::Val(index));
    dispatcher()->dispatch(q);
}

QStringList CommonAudioApiConfigurationModel::audioApiList() const
{
    QStringList result;
    for (const std::string& api: audioDevicesProvider()->apis()) {
        result.push_back(QString::fromStdString(api));
    }

    return result;
}

QString CommonAudioApiConfigurationModel::currentOutputDeviceId() const
{
    return QString::fromStdString(audioDevicesProvider()->currentOutputDevice());
}

QVariantList CommonAudioApiConfigurationModel::outputDeviceList() const
{
    QVariantList result;
    LOGI() << "Preferences output device list requested (current="
           << currentOutputDeviceId().toStdString() << ")";
    for (const auto& device : audioDevicesProvider()->outputDevices()) {
        result << QString::fromStdString(device);
    }
    for (int i = 0; i < result.size(); ++i) {
        LOGI() << "  output[" << i << "]: " << result.at(i).toString().toStdString();
    }

    return result;
}

void CommonAudioApiConfigurationModel::outputDeviceSelected(const QString& device)
{
    LOGI() << "Preferences output device selected: " << device.toStdString();
    if (device == currentOutputDeviceId()) {
        return;
    }

    const std::string deviceId = device.toStdString();
    const auto& devices = audioDevicesProvider()->outputDevices();
    auto it = std::find(devices.begin(), devices.end(), deviceId);
    if (it == devices.end()) {
        LOGW() << "Preferences output device not found: " << deviceId;
        return;
    }

    const int index = static_cast<int>(std::distance(devices.begin(), it));
    ActionQuery q = PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY;
    q.addParam("device_index", muse::Val(index));
    dispatcher()->dispatch(q);
}

QString CommonAudioApiConfigurationModel::currentInputDeviceId() const
{
    return QString::fromStdString(audioDevicesProvider()->currentInputDevice());
}

QVariantList CommonAudioApiConfigurationModel::inputDeviceList() const
{
    QVariantList result;
    LOGI() << "Preferences input device list requested (current="
           << currentInputDeviceId().toStdString() << ")";
    for (const auto& device : audioDevicesProvider()->inputDevices()) {
        result << QString::fromStdString(device);
    }
    for (int i = 0; i < result.size(); ++i) {
        LOGI() << "  input[" << i << "]: " << result.at(i).toString().toStdString();
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputDeviceSelected(const QString& device)
{
    LOGI() << "Preferences input device selected: " << device.toStdString();
    if (device == currentInputDeviceId()) {
        return;
    }

    const std::string deviceId = device.toStdString();
    const auto& devices = audioDevicesProvider()->inputDevices();
    auto it = std::find(devices.begin(), devices.end(), deviceId);
    if (it == devices.end()) {
        LOGW() << "Preferences input device not found: " << deviceId;
        return;
    }

    const int index = static_cast<int>(std::distance(devices.begin(), it));
    ActionQuery q = PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY;
    q.addParam("device_index", muse::Val(index));
    dispatcher()->dispatch(q);
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

QString CommonAudioApiConfigurationModel::currentInputChannelsSelected() const
{
    return channelName(audioDevicesProvider()->inputChannelsSelected());
}

QVariantList CommonAudioApiConfigurationModel::inputChannelsList() const
{
    QVariantList result;

    for (int i = 0; i < audioDevicesProvider()->inputChannelsAvailable(); i++) {
        result << channelName(i + 1);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputChannelsSelected(const int index)
{
    audioDevicesProvider()->setInputChannels(index + 1);
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
    for (const auto& rate : audioDevicesProvider()->sampleRates()) {
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
    for (const auto& format : audioDevicesProvider()->sampleFormats()) {
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
