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

#include <QFontMetrics>

#include "containers.h"
#include "log.h"
#include "types/translatablestring.h"

using namespace au::appshell;

namespace {
QString toSampleRateName(uint64_t sampleRate)
{
    return QString::number(sampleRate) + " Hz";
}

QString channelName(int channelNumber)
{
    return channelNumber == 1
           //: %1 is the recording channel number
           ? muse::qtrc("preferences", "%1 (Mono) Recording channel").arg(channelNumber)
           : channelNumber == 2
           //: %1 is the recording channel number
           ? muse::qtrc("preferences", "%1 (Stereo) Recording channels").arg(channelNumber)
           : QString::number(channelNumber);
}

QString systemDefaultDeviceName(const std::string& resolvedDevice)
{
    if (resolvedDevice.empty()) {
        return muse::qtrc("preferences", "System default");
    }
    //: %1 is the device the system default currently resolves to
    return muse::qtrc("preferences", "System default: %1").arg(QString::fromStdString(resolvedDevice));
}
}

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CommonAudioApiConfigurationModel::load()
{
    audioDevicesProvider()->apiChanged().onNotify(this, [this]() {
        emit currentAudioApiIndexChanged();
        emit outputDeviceListChanged();
        emit inputDeviceListChanged();
        emit longestDeviceNameLengthChanged();

        emit currentOutputDeviceIndexChanged();
        emit currentInputDeviceIndexChanged();

        emit inputChannelsListChanged();
        emit currentInputChannelsSelectedChanged();
    });
    audioDevicesProvider()->outputDeviceChanged().onNotify(this, [this]() { emit currentOutputDeviceIndexChanged(); });
    audioDevicesProvider()->inputDeviceChanged().onNotify(this, [this]() { emit currentInputDeviceIndexChanged(); });
    audioDevicesProvider()->inputChannelsAvailableChanged().onNotify(this, [this](){ emit inputChannelsListChanged(); });
    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this](){ emit currentInputChannelsSelectedChanged(); });
    audioDevicesProvider()->automaticCompensationEnabledChanged().onNotify(this, [this](){ emit automaticCompensationEnabledChanged(); });
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
    audioDevicesProvider()->asioUseDeviceSampleRateChanged().onNotify(this, [this](){ emit asioUseDeviceSampleRateChanged(); });
}

bool CommonAudioApiConfigurationModel::isAsio() const
{
    return audioDevicesProvider()->currentApi() == "ASIO";
}

bool CommonAudioApiConfigurationModel::asioUseDeviceSampleRate() const
{
    return audioDevicesProvider()->asioUseDeviceSampleRate();
}

void CommonAudioApiConfigurationModel::setAsioUseDeviceSampleRate(bool use)
{
    audioDevicesProvider()->setAsioUseDeviceSampleRate(use);
}

void CommonAudioApiConfigurationModel::showAsioControlPanel()
{
    audioDevicesProvider()->showAsioControlPanel();
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

    audioDevicesProvider()->setApi(apiList[index]);
}

QStringList CommonAudioApiConfigurationModel::audioApiList() const
{
    QStringList result;
    for (const std::string& api: audioDevicesProvider()->apis()) {
        result.push_back(QString::fromStdString(api));
    }

    return result;
}

int CommonAudioApiConfigurationModel::currentOutputDeviceIndex() const
{
    const auto& devices = audioDevicesProvider()->outputDevices();
    if (devices.empty()) {
        return -1;
    }

    const std::optional<std::string> device = audioDevicesProvider()->currentOutputDevice();
    if (!device.has_value()) {
        return 0;
    }

    // entry 0 is "System default", devices follow
    const size_t idx = muse::indexOf(devices, device.value());
    return idx == muse::nidx ? 0 : static_cast<int>(idx) + 1;
}

QVariantList CommonAudioApiConfigurationModel::outputDeviceList() const
{
    QVariantList result;
    const auto& devices = audioDevicesProvider()->outputDevices();
    if (!devices.empty()) {
        result << systemDefaultDeviceName(audioDevicesProvider()->systemDefaultOutputDevice());
    }
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::outputDeviceSelected(int index)
{
    if (index == currentOutputDeviceIndex()) {
        return;
    }

    const auto& devices = audioDevicesProvider()->outputDevices();
    if (index == 0) {
        audioDevicesProvider()->setOutputDevice(std::nullopt);
    } else if (index > 0 && index <= static_cast<int>(devices.size())) {
        audioDevicesProvider()->setOutputDevice(devices[index - 1]);
    }
}

int CommonAudioApiConfigurationModel::currentInputDeviceIndex() const
{
    const auto& devices = audioDevicesProvider()->inputDevices();
    if (devices.empty()) {
        return -1;
    }

    const std::optional<std::string> device = audioDevicesProvider()->currentInputDevice();
    if (!device.has_value()) {
        return 0;
    }

    // entry 0 is "System default", devices follow
    const size_t idx = muse::indexOf(devices, device.value());
    return idx == muse::nidx ? 0 : static_cast<int>(idx) + 1;
}

QVariantList CommonAudioApiConfigurationModel::inputDeviceList() const
{
    QVariantList result;
    const auto& devices = audioDevicesProvider()->inputDevices();
    if (!devices.empty()) {
        result << systemDefaultDeviceName(audioDevicesProvider()->systemDefaultInputDevice());
    }
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputDeviceSelected(int index)
{
    if (index == currentInputDeviceIndex()) {
        return;
    }

    const auto& devices = audioDevicesProvider()->inputDevices();
    if (index == 0) {
        audioDevicesProvider()->setInputDevice(std::nullopt);
    } else if (index > 0 && index <= static_cast<int>(devices.size())) {
        audioDevicesProvider()->setInputDevice(devices[index - 1]);
    }
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

bool CommonAudioApiConfigurationModel::automaticCompensationEnabled() const
{
    return audioDevicesProvider()->automaticCompensationEnabled();
}

void CommonAudioApiConfigurationModel::setAutomaticCompensationEnabled(bool enabled)
{
    if (enabled == automaticCompensationEnabled()) {
        return;
    }

    audioDevicesProvider()->setAutomaticCompensationEnabled(enabled);
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
