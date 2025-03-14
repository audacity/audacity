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
#ifndef AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
#define AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "audio/iaudioconfiguration.h"
#include "playback/iaudiodevicesprovider.h"
#include "playback/iplaybackconfiguration.h"

namespace au::appshell {
class PlaybackPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString currentPlaybackQuality READ currentPlaybackQuality NOTIFY currentPlaybackQualityChanged)
    Q_PROPERTY(QVariantList playbackQualityList READ playbackQualityList NOTIFY playbackQualityListChanged)

    Q_PROPERTY(QString currentDithering READ currentDithering NOTIFY currentDitheringChanged)
    Q_PROPERTY(QVariantList ditheringList READ ditheringList NOTIFY ditheringListChanged)

    Q_PROPERTY(playback::TracksBehaviors::SoloBehavior soloBehavior READ soloBehavior NOTIFY soloBehaviorChanged)

    Q_PROPERTY(double shortSkip READ shortSkip NOTIFY shortSkipChanged)
    Q_PROPERTY(double longSkip READ longSkip NOTIFY longSkipChanged)

    muse::Inject<muse::audio::IAudioConfiguration> audioConfiguration;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    explicit PlaybackPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    QString currentPlaybackQuality() const;
    QVariantList playbackQualityList() const;
    Q_INVOKABLE void playbackQualitySelected(const QString& quality);

    QString currentDithering() const;
    QVariantList ditheringList() const;
    Q_INVOKABLE void ditheringSelected(const QString& quality);

    Q_INVOKABLE playback::TracksBehaviors::SoloBehavior soloBehavior() const;
    Q_INVOKABLE void soloBehaviorSelected(playback::TracksBehaviors::SoloBehavior behavior);

    double shortSkip() const;
    Q_INVOKABLE void shortSkipSelected(double seconds);

    double longSkip() const;
    Q_INVOKABLE void longSkipSelected(double seconds);

signals:
    void currentPlaybackQualityChanged();
    void playbackQualityListChanged();

    void currentDitheringChanged();
    void ditheringListChanged();

    void soloBehaviorChanged();

    void shortSkipChanged();
    void longSkipChanged();
};
}

#endif // AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
