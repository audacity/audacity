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
#ifndef AU_APPSHELL_COMMONAUDIOAPICONFIGURATIONMODEL_H
#define AU_APPSHELL_COMMONAUDIOAPICONFIGURATIONMODEL_H

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
// #include "audio/iaudioconfiguration.h"
// #include "audio/iaudiodriver.h"
#include "playback/iaudiodevicesprovider.h"

namespace au::appshell {
class CommonAudioApiConfigurationModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString currentOutputDeviceId READ currentOutputDeviceId NOTIFY currentOutputDeviceIdChanged)
    Q_PROPERTY(QVariantList outputDeviceList READ outputDeviceList NOTIFY outputDeviceListChanged)

    Q_PROPERTY(QString currentInputDeviceId READ currentInputDeviceId NOTIFY currentInputDeviceIdChanged)
    Q_PROPERTY(QVariantList inputDeviceList READ inputDeviceList NOTIFY inputDeviceListChanged)

    Q_PROPERTY(double bufferLength READ bufferLength NOTIFY bufferLengthChanged)
    Q_PROPERTY(double latencyCompensation READ latencyCompensation NOTIFY latencyCompensationChanged)

    Q_PROPERTY(QString currentInputChannels READ currentInputChannels NOTIFY currentInputChannelsChanged)
    Q_PROPERTY(QVariantList inputChannelsList READ inputChannelsList NOTIFY inputChannelsListChanged)

    Q_PROPERTY(uint64_t defaultSampleRate READ defaultSampleRate NOTIFY defaultSampleRateChanged)
    Q_PROPERTY(QVariantList defaultSampleRateList READ defaultSampleRateList NOTIFY defaultSampleRateListChanged)

    Q_PROPERTY(QString defaultSampleFormat READ defaultSampleFormat NOTIFY defaultSampleFormatChanged)
    Q_PROPERTY(QVariantList defaultSampleFormatList READ defaultSampleFormatList NOTIFY defaultSampleFormatListChanged)

    // INJECT(muse::audio::IAudioConfiguration, audioConfiguration)
    // INJECT(muse::audio::IAudioDriver, audioDriver)
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    explicit CommonAudioApiConfigurationModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    QString currentOutputDeviceId() const;
    QVariantList outputDeviceList() const;
    Q_INVOKABLE void outputDeviceSelected(const QString& deviceId);

    QString currentInputDeviceId() const;
    QVariantList inputDeviceList() const;
    Q_INVOKABLE void inputDeviceSelected(const QString& deviceId);

    double bufferLength() const;
    Q_INVOKABLE void bufferLengthSelected(const QString& bufferLengthStr);

    double latencyCompensation() const;
    Q_INVOKABLE void latencyCompensationSelected(const QString& latencyCompensationStr);

    QString currentInputChannels() const;
    QVariantList inputChannelsList() const;
    Q_INVOKABLE void inputChannelsSelected(const QString& channelsStr);

    uint64_t defaultSampleRate() const;
    QVariantList defaultSampleRateList() const;
    Q_INVOKABLE void defaultSampleRateSelected(uint64_t rate);

    QString defaultSampleFormat() const;
    QVariantList defaultSampleFormatList() const;
    Q_INVOKABLE void defaultSampleFormatSelected(const QString& format);

signals:
    void currentOutputDeviceIdChanged();
    void outputDeviceListChanged();

    void currentInputDeviceIdChanged();
    void inputDeviceListChanged();

    void bufferLengthChanged();
    void latencyCompensationChanged();

    void currentInputChannelsChanged();
    void inputChannelsListChanged();

    void defaultSampleRateChanged();
    void defaultSampleRateListChanged();

    void defaultSampleFormatChanged();
    void defaultSampleFormatListChanged();
};
}

#endif // AU_APPSHELL_COMMONAUDIOAPICONFIGURATIONMODEL_H
