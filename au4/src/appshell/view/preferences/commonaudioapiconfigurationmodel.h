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

    Q_PROPERTY(QString currentDeviceId READ currentDeviceId NOTIFY currentDeviceIdChanged)
    Q_PROPERTY(QVariantList deviceList READ deviceList NOTIFY deviceListChanged)

    Q_PROPERTY(unsigned int bufferSize READ bufferSize NOTIFY bufferSizeChanged)
    Q_PROPERTY(QList<unsigned int> bufferSizeList READ bufferSizeList NOTIFY bufferSizeListChanged)

    // INJECT(muse::audio::IAudioConfiguration, audioConfiguration)
    // INJECT(muse::audio::IAudioDriver, audioDriver)
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    explicit CommonAudioApiConfigurationModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    QString currentDeviceId() const;
    QVariantList deviceList() const;
    Q_INVOKABLE void deviceSelected(const QString& deviceId);

    unsigned int bufferSize() const;
    QList<unsigned int> bufferSizeList() const;
    Q_INVOKABLE void bufferSizeSelected(const QString& bufferSizeStr);

signals:
    void currentDeviceIdChanged();
    void deviceListChanged();

    void bufferSizeChanged();
    void bufferSizeListChanged();
};
}

#endif // AU_APPSHELL_COMMONAUDIOAPICONFIGURATIONMODEL_H
