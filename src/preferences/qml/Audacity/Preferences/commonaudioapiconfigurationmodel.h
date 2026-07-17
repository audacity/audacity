/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity BVBA and others
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
#pragma once

#include <optional>

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "audio/iaudiodevicesprovider.h"
#include "playback/iplaybackcontroller.h"
#include "ui/iuiconfiguration.h"

namespace au::appshell {
class CommonAudioApiConfigurationModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(int currentAudioApiIndex READ currentAudioApiIndex WRITE setCurrentAudioApiIndex NOTIFY currentAudioApiIndexChanged)

    Q_PROPERTY(QString currentOutputDeviceId READ currentOutputDeviceId NOTIFY currentOutputDeviceIdChanged)
    Q_PROPERTY(QVariantList outputDeviceList READ outputDeviceList NOTIFY outputDeviceListChanged)

    Q_PROPERTY(QString currentInputDeviceId READ currentInputDeviceId NOTIFY currentInputDeviceIdChanged)
    Q_PROPERTY(QVariantList inputDeviceList READ inputDeviceList NOTIFY inputDeviceListChanged)

    Q_PROPERTY(double bufferLength READ bufferLength NOTIFY bufferLengthChanged)
    Q_PROPERTY(bool automaticCompensationEnabled READ automaticCompensationEnabled NOTIFY automaticCompensationEnabledChanged)
    Q_PROPERTY(double latencyCompensation READ latencyCompensation NOTIFY latencyCompensationChanged)

    Q_PROPERTY(QString currentInputChannelsSelected READ currentInputChannelsSelected NOTIFY currentInputChannelsSelectedChanged)
    Q_PROPERTY(QVariantList inputChannelsList READ inputChannelsList NOTIFY inputChannelsListChanged)

    Q_PROPERTY(QString defaultSampleRate READ defaultSampleRate NOTIFY defaultSampleRateChanged)
    Q_PROPERTY(uint64_t defaultSampleRateValue READ defaultSampleRateValue NOTIFY defaultSampleRateValueChanged)
    Q_PROPERTY(QVariantList defaultSampleRateList READ defaultSampleRateList NOTIFY defaultSampleRateListChanged)
    Q_PROPERTY(bool otherSampleRate READ otherSampleRate NOTIFY otherSampleRateChanged)

    Q_PROPERTY(QString defaultSampleFormat READ defaultSampleFormat NOTIFY defaultSampleFormatChanged)
    Q_PROPERTY(QVariantList defaultSampleFormatList READ defaultSampleFormatList NOTIFY defaultSampleFormatListChanged)

    Q_PROPERTY(double longestDeviceNameLength READ longestDeviceNameLength NOTIFY longestDeviceNameLengthChanged)

    Q_PROPERTY(bool isAsio READ isAsio NOTIFY currentAudioApiIndexChanged)
    Q_PROPERTY(bool asioUseDeviceSampleRate READ asioUseDeviceSampleRate NOTIFY asioUseDeviceSampleRateChanged)

    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;

    muse::ContextInject<audio::IAudioDevicesProvider> audioDevicesProvider { this };
    muse::ContextInject<playback::IPlaybackController> playbackController { this };

public:
    explicit CommonAudioApiConfigurationModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    //! Pushes the pending (edited but not yet applied) values to the audio
    //! backend. Called when the Preferences dialog is confirmed; closing the
    //! dialog any other way discards the pending values.
    Q_INVOKABLE bool apply();

    int currentAudioApiIndex() const;
    Q_INVOKABLE QStringList audioApiList() const;
    Q_INVOKABLE void setCurrentAudioApiIndex(int index);

    QString currentOutputDeviceId() const;
    QVariantList outputDeviceList() const;
    Q_INVOKABLE void outputDeviceSelected(const QString& device);

    QString currentInputDeviceId() const;
    QVariantList inputDeviceList() const;
    Q_INVOKABLE void inputDeviceSelected(const QString& device);

    double bufferLength() const;
    Q_INVOKABLE void bufferLengthSelected(const QString& bufferLengthStr);

    bool automaticCompensationEnabled() const;
    Q_INVOKABLE void setAutomaticCompensationEnabled(bool enabled);

    double latencyCompensation() const;
    Q_INVOKABLE void latencyCompensationSelected(const QString& latencyCompensationStr);

    QString currentInputChannelsSelected() const;
    QVariantList inputChannelsList() const;
    Q_INVOKABLE void inputChannelsSelected(const int index);

    // used for dropdown
    QString defaultSampleRate() const;
    QVariantList defaultSampleRateList();
    Q_INVOKABLE void defaultSampleRateSelected(const QString& rate);

    // used for incremental control
    uint64_t defaultSampleRateValue() const;
    Q_INVOKABLE void defaultSampleRateValueSelected(uint64_t rateValue);

    // control incremental control visibility
    bool otherSampleRate() const;
    void setOtherSampleRate(bool other);

    QString defaultSampleFormat() const;
    QVariantList defaultSampleFormatList() const;
    Q_INVOKABLE void defaultSampleFormatSelected(const QString& format);

    double longestDeviceNameLength() const;

    bool isAsio() const;
    bool asioUseDeviceSampleRate() const;
    Q_INVOKABLE void setAsioUseDeviceSampleRate(bool use);
    Q_INVOKABLE void showAsioControlPanel();

signals:
    void currentAudioApiIndexChanged();

    void currentOutputDeviceIdChanged();
    void outputDeviceListChanged();

    void currentInputDeviceIdChanged();
    void inputDeviceListChanged();

    void bufferLengthChanged();
    void latencyCompensationChanged();
    void automaticCompensationEnabledChanged();

    void currentInputChannelsSelectedChanged();
    void inputChannelsListChanged();

    void defaultSampleRateChanged();
    void defaultSampleRateValueChanged();
    void defaultSampleRateListChanged();
    void otherSampleRateChanged();

    void defaultSampleFormatChanged();
    void defaultSampleFormatListChanged();

    void longestDeviceNameLengthChanged();

    void asioUseDeviceSampleRateChanged();

private:
    std::string effectiveApi() const;
    std::string effectiveOutputDevice() const;
    std::string effectiveInputDevice() const;
    int effectiveInputChannelsAvailable() const;
    int effectiveInputChannels() const;

    void setPendingSampleRate(uint64_t rateValue);
    void clearPendingValues();
    void notifyDeviceContextChanged();

    std::vector<std::pair<uint64_t, QString> > m_sampleRateMapping;
    bool m_otherSampleRate = false;

    // The dialog edits these pending values; the audio backend is only touched
    // when apply() pushes them on OK. An empty optional means "unchanged, show
    // the applied value". A pending value set back to the applied one is
    // cleared, so apply() only pushes real differences.
    std::optional<std::string> m_pendingApi;
    std::optional<std::string> m_pendingOutputDevice;
    std::optional<std::string> m_pendingInputDevice;
    std::optional<int> m_pendingInputChannels;
    std::optional<double> m_pendingBufferLength;
    std::optional<double> m_pendingLatencyCompensation;
    std::optional<bool> m_pendingAutomaticCompensation;
    std::optional<uint64_t> m_pendingSampleRate;
    std::optional<std::string> m_pendingSampleFormat;
    std::optional<bool> m_pendingAsioUseDeviceSampleRate;
};
}
