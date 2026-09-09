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

QString failureMessage(au::audio::ApplyStatus status)
{
    switch (status) {
    case au::audio::ApplyStatus::Busy:
        return muse::qtrc("preferences", "Audio settings are already being changed.");
    case au::audio::ApplyStatus::InvalidConfiguration:
        return muse::qtrc("preferences", "The selected audio settings are invalid.");
    case au::audio::ApplyStatus::InvalidRouting:
        return muse::qtrc("preferences", "The selected audio routing is invalid.");
    case au::audio::ApplyStatus::NoUsableAudioApi:
        return muse::qtrc("preferences", "No usable audio API is available.");
    case au::audio::ApplyStatus::NoAsioDevice:
        return muse::qtrc("preferences", "No ASIO device is available.");
    case au::audio::ApplyStatus::OwnerUnavailable:
        return muse::qtrc("preferences", "The active audio stream could not be stopped.");
    case au::audio::ApplyStatus::InternalError:
        return muse::qtrc("preferences", "An internal error occurred while changing the audio settings.");
    case au::audio::ApplyStatus::Applied:
    case au::audio::ApplyStatus::NoChange:
        return {};
    }
    return {};
}

QString resultMessage(const au::audio::ApplyResult& result,
                      QString message,
                      const QString& restorationFailure)
{
    if (result.streamRestorationFailed) {
        if (!message.isEmpty()) {
            message += " ";
        }
        message += restorationFailure;
    }
    return message;
}

QString systemDefaultDeviceName(const std::string& resolvedDevice)
{
    if (resolvedDevice.empty()) {
        return muse::qtrc("preferences", "System default");
    }
    return muse::qtrc("preferences", "System default: %1").arg(QString::fromStdString(resolvedDevice));
}
}

CommonAudioApiConfigurationModel::CommonAudioApiConfigurationModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CommonAudioApiConfigurationModel::load()
{
    clearPendingValues();
    setOtherSampleRate(!muse::contains(audioDriverController()->sampleRates(), defaultSampleRateValue()));
    audioDriverController()->configurationChanged().onReceive(this, [this](const audio::AudioConfigurationDelta& delta) {
        if (delta.contains(audio::AudioConfigurationField::Api)
            || delta.contains(audio::AudioConfigurationField::OutputDevice)
            || delta.contains(audio::AudioConfigurationField::InputDevice)
            || delta.contains(audio::AudioConfigurationField::InputChannels)) {
            notifyDeviceContextChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::BufferLength)) {
            emit bufferLengthChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::AutomaticLatencyCompensation)) {
            emit automaticCompensationEnabledChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::LatencyCompensation)) {
            emit latencyCompensationChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::DefaultSampleRate)) {
            setOtherSampleRate(!muse::contains(audioDriverController()->sampleRates(), defaultSampleRateValue()));
            emit defaultSampleRateChanged();
            emit defaultSampleRateValueChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::DefaultSampleFormat)) {
            emit defaultSampleFormatChanged();
        }
        if (delta.contains(audio::AudioConfigurationField::AsioUseDeviceSampleRate)) {
            emit asioUseDeviceSampleRateChanged();
        }
    });
    audioDriverController()->audioDeviceListChanged().onNotify(this, [this]() {
        notifyDeviceContextChanged();
    });
}

void CommonAudioApiConfigurationModel::reset()
{
    clearPendingValues();
    setOtherSampleRate(!muse::contains(audioDriverController()->sampleRates(), defaultSampleRateValue()));
    notifyDeviceContextChanged();
    emit bufferLengthChanged();
    emit automaticCompensationEnabledChanged();
    emit latencyCompensationChanged();
    emit defaultSampleRateChanged();
    emit defaultSampleRateValueChanged();
    emit defaultSampleFormatChanged();
    emit asioUseDeviceSampleRateChanged();
}

bool CommonAudioApiConfigurationModel::apply()
{
    const auto result = audioDriverController()->apply(iocContext(), m_pending);
    if (result.succeeded()) {
        clearPendingValues();
        const auto notice = resultMessage(
            result,
            {},
            muse::qtrc("preferences", "The audio stream could not be restored after changing the audio settings."));
        if (!notice.isEmpty() && interactive()) {
            //: Used as the title of the Audio settings preferences page and of related warning dialogs
            interactive()->warning(muse::qtrc("preferences", "Audio settings").toStdString(),
                                   notice.toStdString());
        }
        return true;
    }
    if (interactive()) {
        const auto message = resultMessage(
            result,
            failureMessage(result.status),
            muse::qtrc("preferences", "The previous audio state could not be restored."));
        interactive()->error(muse::qtrc("preferences", "Unable to apply audio settings").toStdString(),
                             message.toStdString());
    }
    return false;
}

void CommonAudioApiConfigurationModel::clearPendingValues()
{
    m_pending = {};
}

std::string CommonAudioApiConfigurationModel::effectiveApi() const
{
    return m_pending.api.value_or(audioDriverController()->configuration().api);
}

au::audio::AudioDeviceSelection CommonAudioApiConfigurationModel::effectiveOutputDevice() const
{
    return m_pending.outputDevice.value_or(audioDriverController()->configuration().outputDevice);
}

au::audio::AudioDeviceSelection CommonAudioApiConfigurationModel::effectiveInputDevice() const
{
    return m_pending.inputDevice.value_or(audioDriverController()->configuration().inputDevice);
}

int CommonAudioApiConfigurationModel::effectiveInputChannelsAvailable() const
{
    if (!m_pending.api && !m_pending.inputDevice) {
        return audioDriverController()->inputChannelsAvailable();
    }
    return audioDriverController()->inputChannelsAvailable(effectiveApi(), effectiveInputDevice());
}

int CommonAudioApiConfigurationModel::effectiveInputChannels() const
{
    int channels = m_pending.inputChannels.value_or(audioDriverController()->configuration().inputChannels);
    const int available = effectiveInputChannelsAvailable();
    return available > 0 ? std::min(channels, available) : 0;
}

void CommonAudioApiConfigurationModel::notifyDeviceContextChanged()
{
    emit currentAudioApiIndexChanged();
    emit outputDeviceListChanged();
    emit inputDeviceListChanged();
    emit longestDeviceNameLengthChanged();
    emit currentOutputDeviceIndexChanged();
    emit currentInputDeviceIndexChanged();
    emit inputChannelsListChanged();
    emit currentInputChannelsSelectedChanged();
}

bool CommonAudioApiConfigurationModel::isAsio() const
{
    return effectiveApi() == "ASIO";
}

bool CommonAudioApiConfigurationModel::asioUseDeviceSampleRate() const
{
    return m_pending.asioUseDeviceSampleRate.value_or(audioDriverController()->configuration().asioUseDeviceSampleRate);
}

void CommonAudioApiConfigurationModel::setAsioUseDeviceSampleRate(bool use)
{
    if (use == asioUseDeviceSampleRate()) {
        return;
    }
    if (use == audioDriverController()->configuration().asioUseDeviceSampleRate) {
        m_pending.asioUseDeviceSampleRate.reset();
    } else {
        m_pending.asioUseDeviceSampleRate = use;
    }
    emit asioUseDeviceSampleRateChanged();
}

void CommonAudioApiConfigurationModel::showAsioControlPanel()
{
    audio::AudioRoutingChange routing;
    routing.api = m_pending.api;
    routing.outputDevice = m_pending.outputDevice;
    routing.inputDevice = m_pending.inputDevice;
    const auto result = audioDriverController()->openAsioDriverSettings(routing);
    if (result.succeeded()) {
        m_pending.api.reset();
        m_pending.outputDevice.reset();
        m_pending.inputDevice.reset();
        notifyDeviceContextChanged();
        const auto notice = resultMessage(
            result,
            {},
            muse::qtrc("preferences", "The audio stream could not be restored after closing the ASIO settings."));
        if (!notice.isEmpty() && interactive()) {
            interactive()->warning(muse::qtrc("preferences", "Audio settings").toStdString(),
                                   notice.toStdString());
        }
    } else if (interactive()) {
        const auto message = resultMessage(
            result,
            failureMessage(result.status),
            muse::qtrc("preferences", "The previous audio state could not be restored."));
        interactive()->error(muse::qtrc("preferences", "Unable to open ASIO settings").toStdString(),
                             message.toStdString());
    }
}

int CommonAudioApiConfigurationModel::currentAudioApiIndex() const
{
    QString currentApi = QString::fromStdString(effectiveApi());
    return audioApiList().indexOf(currentApi);
}

void CommonAudioApiConfigurationModel::setCurrentAudioApiIndex(int index)
{
    if (index == currentAudioApiIndex()) {
        return;
    }

    std::vector<std::string> apiList = audioDriverController()->apis();
    if (index < 0 || index >= static_cast<int>(apiList.size())) {
        return;
    }

    const std::string& api = apiList[index];
    if (api == audioDriverController()->configuration().api) {
        m_pending.api.reset();
    } else {
        m_pending.api = api;
    }
    m_pending.outputDevice.reset();
    m_pending.inputDevice.reset();
    m_pending.inputChannels.reset();
    notifyDeviceContextChanged();
}

QStringList CommonAudioApiConfigurationModel::audioApiList() const
{
    QStringList result;
    for (const std::string& api: audioDriverController()->apis()) {
        result.push_back(QString::fromStdString(api));
    }

    return result;
}

int CommonAudioApiConfigurationModel::currentOutputDeviceIndex() const
{
    const auto devices = audioDriverController()->outputDevices(effectiveApi());
    if (devices.empty()) {
        return -1;
    }

    const auto device = effectiveOutputDevice();
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
    const auto devices = audioDriverController()->outputDevices(effectiveApi());
    if (!devices.empty()) {
        result << systemDefaultDeviceName(audioDriverController()->systemDefaultOutputDevice(effectiveApi()));
    }
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::outputDeviceSelected(int index)
{
    const auto devices = audioDriverController()->outputDevices(effectiveApi());
    if (index < 0 || index > static_cast<int>(devices.size())) {
        return;
    }

    // entry 0 is "System default", devices follow
    const auto value = index == 0
                       ? audio::AudioDeviceSelection {}
    : audio::AudioDeviceSelection { devices[index - 1] };
    if (!m_pending.api && value == audioDriverController()->configuration().outputDevice) {
        m_pending.outputDevice.reset();
    } else {
        m_pending.outputDevice = value;
    }
    emit currentOutputDeviceIndexChanged();
}

int CommonAudioApiConfigurationModel::currentInputDeviceIndex() const
{
    const auto devices = audioDriverController()->inputDevices(effectiveApi());
    if (devices.empty()) {
        return -1;
    }

    const auto device = effectiveInputDevice();
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
    const auto devices = audioDriverController()->inputDevices(effectiveApi());
    if (!devices.empty()) {
        result << systemDefaultDeviceName(audioDriverController()->systemDefaultInputDevice(effectiveApi()));
    }
    for (const auto& device : devices) {
        result << QString::fromStdString(device);
    }

    return result;
}

void CommonAudioApiConfigurationModel::inputDeviceSelected(int index)
{
    const auto devices = audioDriverController()->inputDevices(effectiveApi());
    if (index < 0 || index > static_cast<int>(devices.size())) {
        return;
    }

    // entry 0 is "System default", devices follow
    const auto value = index == 0
                       ? audio::AudioDeviceSelection {}
    : audio::AudioDeviceSelection { devices[index - 1] };
    if (!m_pending.api && value == audioDriverController()->configuration().inputDevice) {
        m_pending.inputDevice.reset();
    } else {
        m_pending.inputDevice = value;
    }
    m_pending.inputChannels.reset();
    emit currentInputDeviceIndexChanged();
    emit inputChannelsListChanged();
    emit currentInputChannelsSelectedChanged();
}

double CommonAudioApiConfigurationModel::bufferLength() const
{
    return m_pending.bufferLength.value_or(audioDriverController()->configuration().bufferLength);
}

void CommonAudioApiConfigurationModel::bufferLengthSelected(const QString& bufferLengthStr)
{
    if (bufferLengthStr == QString::number(bufferLength())) {
        return;
    }

    const double value = bufferLengthStr.toDouble();
    if (muse::RealIsEqual(value, audioDriverController()->configuration().bufferLength)) {
        m_pending.bufferLength.reset();
    } else {
        m_pending.bufferLength = value;
    }
    emit bufferLengthChanged();
}

bool CommonAudioApiConfigurationModel::automaticCompensationEnabled() const
{
    return m_pending.automaticLatencyCompensation.value_or(
        audioDriverController()->configuration().automaticLatencyCompensation);
}

void CommonAudioApiConfigurationModel::setAutomaticCompensationEnabled(bool enabled)
{
    if (enabled == automaticCompensationEnabled()) {
        return;
    }

    if (enabled == audioDriverController()->configuration().automaticLatencyCompensation) {
        m_pending.automaticLatencyCompensation.reset();
    } else {
        m_pending.automaticLatencyCompensation = enabled;
    }
    emit automaticCompensationEnabledChanged();
}

double CommonAudioApiConfigurationModel::latencyCompensation() const
{
    return m_pending.latencyCompensation.value_or(audioDriverController()->configuration().latencyCompensation);
}

void CommonAudioApiConfigurationModel::latencyCompensationSelected(
    const QString& latencyCompensationStr)
{
    if (latencyCompensationStr == QString::number(latencyCompensation())) {
        return;
    }

    const double value = latencyCompensationStr.toDouble();
    if (muse::RealIsEqual(value, audioDriverController()->configuration().latencyCompensation)) {
        m_pending.latencyCompensation.reset();
    } else {
        m_pending.latencyCompensation = value;
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
    const int value = index + 1;
    if (!m_pending.api && !m_pending.inputDevice
        && value == audioDriverController()->configuration().inputChannels) {
        m_pending.inputChannels.reset();
    } else {
        m_pending.inputChannels = value;
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
    for (const auto& rate : audioDriverController()->sampleRates()) {
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
    return m_pending.defaultSampleRate.value_or(audioDriverController()->configuration().defaultSampleRate);
}

void CommonAudioApiConfigurationModel::defaultSampleRateValueSelected(uint64_t rateValue)
{
    if (rateValue == defaultSampleRateValue()) {
        return;
    }

    setPendingSampleRate(rateValue);
}

void CommonAudioApiConfigurationModel::setPendingSampleRate(uint64_t rateValue)
{
    if (rateValue == defaultSampleRateValue()) {
        return;
    }
    if (rateValue == audioDriverController()->configuration().defaultSampleRate) {
        m_pending.defaultSampleRate.reset();
    } else {
        m_pending.defaultSampleRate = rateValue;
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
    return QString::fromStdString(m_pending.defaultSampleFormat.value_or(
                                      audioDriverController()->configuration().defaultSampleFormat));
}

QVariantList CommonAudioApiConfigurationModel::defaultSampleFormatList() const
{
    QVariantList result;
    for (const auto& format : audioDriverController()->sampleFormats()) {
        result << QString::fromStdString(format);
    }

    return result;
}

void CommonAudioApiConfigurationModel::defaultSampleFormatSelected(const QString& format)
{
    if (format == defaultSampleFormat()) {
        return;
    }

    const auto value = format.toStdString();
    if (value == audioDriverController()->configuration().defaultSampleFormat) {
        m_pending.defaultSampleFormat.reset();
    } else {
        m_pending.defaultSampleFormat = value;
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
