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
#include "realfn.h"
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
}

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CommonAudioApiConfigurationModel::load()
{
    clearPendingValues();

    audioDevicesProvider()->apiChanged().onNotify(this, [this]() {
        notifyDeviceContextChanged();
    });
    audioDevicesProvider()->outputDeviceChanged().onNotify(this, [this]() { emit currentOutputDeviceIdChanged(); });
    audioDevicesProvider()->inputDeviceChanged().onNotify(this, [this]() { emit currentInputDeviceIdChanged(); });
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

bool CommonAudioApiConfigurationModel::apply()
{
    // The API must be applied before the devices (a pending device belongs to
    // the pending API) and the devices before the channel count. Each setter
    // no-ops when the value did not actually change, so only real differences
    // interrupt a running stream.
    if (m_pendingApi) {
        playbackController()->setAudioApi(*m_pendingApi);
    }
    if (m_pendingOutputDevice) {
        playbackController()->setAudioOutputDevice(*m_pendingOutputDevice);
    }
    if (m_pendingInputDevice) {
        playbackController()->setAudioInputDevice(*m_pendingInputDevice);
    }
    if (m_pendingInputChannels) {
        playbackController()->setInputChannels(*m_pendingInputChannels);
    }
    if (m_pendingBufferLength) {
        playbackController()->setBufferLength(*m_pendingBufferLength);
    }
    if (m_pendingLatencyCompensation) {
        playbackController()->setLatencyCompensation(*m_pendingLatencyCompensation);
    }
    if (m_pendingAutomaticCompensation) {
        audioDevicesProvider()->setAutomaticCompensationEnabled(*m_pendingAutomaticCompensation);
    }
    if (m_pendingSampleRate) {
        audioDevicesProvider()->setDefaultSampleRate(*m_pendingSampleRate);
    }
    if (m_pendingSampleFormat) {
        audioDevicesProvider()->setDefaultSampleFormat(*m_pendingSampleFormat);
    }
    if (m_pendingAsioUseDeviceSampleRate) {
        audioDevicesProvider()->setAsioUseDeviceSampleRate(*m_pendingAsioUseDeviceSampleRate);
    }

    clearPendingValues();

    // The backend may have adjusted a pushed value (e.g. clamped the channel
    // count), so refresh everything now that the getters read applied state.
    notifyDeviceContextChanged();
    emit bufferLengthChanged();
    emit latencyCompensationChanged();
    emit automaticCompensationEnabledChanged();
    emit defaultSampleRateChanged();
    emit defaultSampleRateValueChanged();
    emit defaultSampleFormatChanged();
    emit asioUseDeviceSampleRateChanged();

    return true;
}

void CommonAudioApiConfigurationModel::clearPendingValues()
{
    m_pendingApi.reset();
    m_pendingOutputDevice.reset();
    m_pendingInputDevice.reset();
    m_pendingInputChannels.reset();
    m_pendingBufferLength.reset();
    m_pendingLatencyCompensation.reset();
    m_pendingAutomaticCompensation.reset();
    m_pendingSampleRate.reset();
    m_pendingSampleFormat.reset();
    m_pendingAsioUseDeviceSampleRate.reset();
}

std::string CommonAudioApiConfigurationModel::effectiveApi() const
{
    return m_pendingApi ? *m_pendingApi : audioDevicesProvider()->currentApi();
}

std::string CommonAudioApiConfigurationModel::effectiveOutputDevice() const
{
    return m_pendingOutputDevice ? *m_pendingOutputDevice : audioDevicesProvider()->currentOutputDevice();
}

std::string CommonAudioApiConfigurationModel::effectiveInputDevice() const
{
    return m_pendingInputDevice ? *m_pendingInputDevice : audioDevicesProvider()->currentInputDevice();
}

int CommonAudioApiConfigurationModel::effectiveInputChannelsAvailable() const
{
    if (!m_pendingApi && !m_pendingInputDevice) {
        return audioDevicesProvider()->inputChannelsAvailable();
    }

    return audioDevicesProvider()->inputChannelsAvailable(effectiveApi(), effectiveInputDevice());
}

int CommonAudioApiConfigurationModel::effectiveInputChannels() const
{
    int channels = m_pendingInputChannels ? *m_pendingInputChannels : audioDevicesProvider()->inputChannelsSelected();

    const int available = effectiveInputChannelsAvailable();
    if (available > 0) {
        channels = std::min(channels, available);
    }

    return channels;
}

void CommonAudioApiConfigurationModel::notifyDeviceContextChanged()
{
    emit currentAudioApiIndexChanged();
    emit outputDeviceListChanged();
    emit inputDeviceListChanged();
    emit longestDeviceNameLengthChanged();

    emit currentOutputDeviceIdChanged();
    emit currentInputDeviceIdChanged();

    emit inputChannelsListChanged();
    emit currentInputChannelsSelectedChanged();
}

bool CommonAudioApiConfigurationModel::isAsio() const
{
    return effectiveApi() == "ASIO";
}

bool CommonAudioApiConfigurationModel::asioUseDeviceSampleRate() const
{
    return m_pendingAsioUseDeviceSampleRate
           ? *m_pendingAsioUseDeviceSampleRate
           : audioDevicesProvider()->asioUseDeviceSampleRate();
}

void CommonAudioApiConfigurationModel::setAsioUseDeviceSampleRate(bool use)
{
    if (use == asioUseDeviceSampleRate()) {
        return;
    }

    if (use == audioDevicesProvider()->asioUseDeviceSampleRate()) {
        m_pendingAsioUseDeviceSampleRate.reset();
    } else {
        m_pendingAsioUseDeviceSampleRate = use;
    }

    emit asioUseDeviceSampleRateChanged();
}

void CommonAudioApiConfigurationModel::showAsioControlPanel()
{
    audioDevicesProvider()->showAsioControlPanel();
}

int CommonAudioApiConfigurationModel::currentAudioApiIndex() const
{
    return audioApiList().indexOf(QString::fromStdString(effectiveApi()));
}

// The setters below only edit the pending values (and cascade the dependent
// lists); nothing reaches the audio backend until the dialog is confirmed and
// apply() pushes the differences.

void CommonAudioApiConfigurationModel::setCurrentAudioApiIndex(int index)
{
    std::vector<std::string> apiList = audioDevicesProvider()->apis();
    if (index < 0 || index >= static_cast<int>(apiList.size())) {
        return;
    }

    const std::string& api = apiList.at(index);
    if (api == effectiveApi()) {
        return;
    }

    if (api == audioDevicesProvider()->currentApi()) {
        m_pendingApi.reset();
    } else {
        m_pendingApi = api;
    }

    // A device selection belongs to the API it was made under
    m_pendingOutputDevice.reset();
    m_pendingInputDevice.reset();
    m_pendingInputChannels.reset();

    if (m_pendingApi) {
        // Preview the device the backend will fall back to when the new API is
        // applied: the current one when the new API also has it, else the first
        // available one (mirrors Au3AudioDevicesProvider::updateInputOutputDevices()).
        const std::vector<std::string> outputs = audioDevicesProvider()->outputDevices(*m_pendingApi);
        if (!outputs.empty() && !muse::contains(outputs, audioDevicesProvider()->currentOutputDevice())) {
            m_pendingOutputDevice = outputs.front();
        }

        const std::vector<std::string> inputs = audioDevicesProvider()->inputDevices(*m_pendingApi);
        if (!inputs.empty() && !muse::contains(inputs, audioDevicesProvider()->currentInputDevice())) {
            m_pendingInputDevice = inputs.front();
        }
    }

    notifyDeviceContextChanged();
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
    return QString::fromStdString(effectiveOutputDevice());
}

QVariantList CommonAudioApiConfigurationModel::outputDeviceList() const
{
    const std::vector<std::string> devices = m_pendingApi
                                             ? audioDevicesProvider()->outputDevices(*m_pendingApi)
                                             : audioDevicesProvider()->outputDevices();

    QVariantList result;
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::outputDeviceSelected(const QString& device)
{
    const std::string deviceId = device.toStdString();
    if (deviceId == effectiveOutputDevice()) {
        return;
    }

    // With an API pending, the applied device belongs to the old API and can
    // never make the selection a no-op
    if (!m_pendingApi && deviceId == audioDevicesProvider()->currentOutputDevice()) {
        m_pendingOutputDevice.reset();
    } else {
        m_pendingOutputDevice = deviceId;
    }

    emit currentOutputDeviceIdChanged();
}

QString CommonAudioApiConfigurationModel::currentInputDeviceId() const
{
    return QString::fromStdString(effectiveInputDevice());
}

QVariantList CommonAudioApiConfigurationModel::inputDeviceList() const
{
    const std::vector<std::string> devices = m_pendingApi
                                             ? audioDevicesProvider()->inputDevices(*m_pendingApi)
                                             : audioDevicesProvider()->inputDevices();

    QVariantList result;
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputDeviceSelected(const QString& device)
{
    const std::string deviceId = device.toStdString();
    if (deviceId == effectiveInputDevice()) {
        return;
    }

    if (!m_pendingApi && deviceId == audioDevicesProvider()->currentInputDevice()) {
        m_pendingInputDevice.reset();
    } else {
        m_pendingInputDevice = deviceId;
    }

    // The channel count depends on the selected device
    m_pendingInputChannels.reset();

    emit currentInputDeviceIdChanged();
    emit inputChannelsListChanged();
    emit currentInputChannelsSelectedChanged();
}

double CommonAudioApiConfigurationModel::bufferLength() const
{
    return m_pendingBufferLength ? *m_pendingBufferLength : audioDevicesProvider()->bufferLength();
}

void CommonAudioApiConfigurationModel::bufferLengthSelected(const QString& bufferLengthStr)
{
    const double newBufferLength = bufferLengthStr.toDouble();
    if (muse::RealIsEqual(newBufferLength, bufferLength())) {
        return;
    }

    if (muse::RealIsEqual(newBufferLength, audioDevicesProvider()->bufferLength())) {
        m_pendingBufferLength.reset();
    } else {
        m_pendingBufferLength = newBufferLength;
    }

    emit bufferLengthChanged();
}

bool CommonAudioApiConfigurationModel::automaticCompensationEnabled() const
{
    return m_pendingAutomaticCompensation
           ? *m_pendingAutomaticCompensation
           : audioDevicesProvider()->automaticCompensationEnabled();
}

void CommonAudioApiConfigurationModel::setAutomaticCompensationEnabled(bool enabled)
{
    if (enabled == automaticCompensationEnabled()) {
        return;
    }

    if (enabled == audioDevicesProvider()->automaticCompensationEnabled()) {
        m_pendingAutomaticCompensation.reset();
    } else {
        m_pendingAutomaticCompensation = enabled;
    }

    emit automaticCompensationEnabledChanged();
}

double CommonAudioApiConfigurationModel::latencyCompensation() const
{
    return m_pendingLatencyCompensation ? *m_pendingLatencyCompensation : audioDevicesProvider()->latencyCompensation();
}

void CommonAudioApiConfigurationModel::latencyCompensationSelected(
    const QString& latencyCompensationStr)
{
    const double newLatencyCompensation = latencyCompensationStr.toDouble();
    if (muse::RealIsEqual(newLatencyCompensation, latencyCompensation())) {
        return;
    }

    if (muse::RealIsEqual(newLatencyCompensation, audioDevicesProvider()->latencyCompensation())) {
        m_pendingLatencyCompensation.reset();
    } else {
        m_pendingLatencyCompensation = newLatencyCompensation;
    }

    emit latencyCompensationChanged();
}

QString CommonAudioApiConfigurationModel::currentInputChannelsSelected() const
{
    return channelName(effectiveInputChannels());
}

QVariantList CommonAudioApiConfigurationModel::inputChannelsList() const
{
    QVariantList result;

    for (int i = 0; i < effectiveInputChannelsAvailable(); i++) {
        result << channelName(i + 1);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputChannelsSelected(const int index)
{
    // A 1-based channel count, the list index is 0-based.
    const int count = index + 1;
    if (count == effectiveInputChannels()) {
        return;
    }

    if (!m_pendingApi && !m_pendingInputDevice && count == audioDevicesProvider()->inputChannelsSelected()) {
        m_pendingInputChannels.reset();
    } else {
        m_pendingInputChannels = count;
    }

    emit currentInputChannelsSelectedChanged();
}

QString CommonAudioApiConfigurationModel::defaultSampleRate() const
{
    auto currentSampleRate = defaultSampleRateValue();
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
        setPendingSampleRate(it->first);
        return;
    }

    setOtherSampleRate(true);
    emit defaultSampleRateChanged();
    emit defaultSampleRateValueChanged();
}

uint64_t CommonAudioApiConfigurationModel::defaultSampleRateValue() const
{
    return m_pendingSampleRate ? *m_pendingSampleRate : audioDevicesProvider()->defaultSampleRate();
}

void CommonAudioApiConfigurationModel::defaultSampleRateValueSelected(uint64_t rateValue)
{
    setPendingSampleRate(rateValue);
}

void CommonAudioApiConfigurationModel::setPendingSampleRate(uint64_t rateValue)
{
    if (rateValue == defaultSampleRateValue()) {
        return;
    }

    if (rateValue == audioDevicesProvider()->defaultSampleRate()) {
        m_pendingSampleRate.reset();
    } else {
        m_pendingSampleRate = rateValue;
    }

    emit defaultSampleRateChanged();
    emit defaultSampleRateValueChanged();
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
    return QString::fromStdString(m_pendingSampleFormat
                                  ? *m_pendingSampleFormat
                                  : audioDevicesProvider()->defaultSampleFormat());
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
    const std::string newFormat = format.toStdString();
    if (format == defaultSampleFormat()) {
        return;
    }

    if (newFormat == audioDevicesProvider()->defaultSampleFormat()) {
        m_pendingSampleFormat.reset();
    } else {
        m_pendingSampleFormat = newFormat;
    }

    emit defaultSampleFormatChanged();
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
