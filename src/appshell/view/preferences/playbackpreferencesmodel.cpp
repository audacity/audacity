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

#include "playbackpreferencesmodel.h"

#include "log.h"

using namespace au::appshell;
using namespace muse::audio;

PlaybackPreferencesModel::PlaybackPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void PlaybackPreferencesModel::init()
{
    playbackConfiguration()->playbackQualityChanged().onNotify(this, [this](){ emit currentPlaybackQualityChanged(); });
    playbackConfiguration()->ditheringChanged().onNotify(this, [this](){ emit currentDitheringChanged(); });
    playbackConfiguration()->soloBehaviorChanged().onNotify(this, [this](){
        emit soloBehaviorChanged();
    });
    playbackConfiguration()->shortSkipChanged().onNotify(this, [this](){ emit shortSkipChanged(); });
    playbackConfiguration()->longSkipChanged().onNotify(this, [this](){ emit longSkipChanged(); });
}

QString PlaybackPreferencesModel::currentPlaybackQuality() const
{
    return QString::fromStdString(playbackConfiguration()->currentPlaybackQuality());
}

QVariantList PlaybackPreferencesModel::playbackQualityList() const
{
    QVariantList result;
    for (const auto& quality: playbackConfiguration()->playbackQualityList()) {
        result << QString::fromStdString(quality);
    }

    return result;
}

void PlaybackPreferencesModel::playbackQualitySelected(const QString& quality)
{
    playbackConfiguration()->setPlaybackQuality(quality.toStdString());
}

QString PlaybackPreferencesModel::currentDithering() const
{
    return QString::fromStdString(playbackConfiguration()->currentDithering());
}

QVariantList PlaybackPreferencesModel::ditheringList() const
{
    QVariantList result;
    for (const auto& dithering: playbackConfiguration()->ditheringList()) {
        result << QString::fromStdString(dithering);
    }

    return result;
}

void PlaybackPreferencesModel::ditheringSelected(const QString& dithering)
{
    playbackConfiguration()->setDithering(dithering.toStdString());
}

au::playback::TracksBehaviors::SoloBehavior PlaybackPreferencesModel::soloBehavior() const
{
    return playbackConfiguration()->currentSoloBehavior();
}

void PlaybackPreferencesModel::soloBehaviorSelected(playback::TracksBehaviors::SoloBehavior behavior)
{
    playbackConfiguration()->setSoloBehavior(behavior);
}

double PlaybackPreferencesModel::shortSkip() const
{
    return playbackConfiguration()->shortSkip();
}

void PlaybackPreferencesModel::shortSkipSelected(double seconds)
{
    playbackConfiguration()->setShortSkip(seconds);
}

double PlaybackPreferencesModel::longSkip() const
{
    return playbackConfiguration()->longSkip();
}

void PlaybackPreferencesModel::longSkipSelected(double seconds)
{
    playbackConfiguration()->setLongSkip(seconds);
}
