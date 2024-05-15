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
#include "playbacktoolbarlevelitem.h"

#include <QVariantMap>

using namespace au::playback;
using namespace au::au3;
using namespace au::audio;

static constexpr volume_dbfs_t MAX_DISPLAYED_DBFS = 0.f; // 100%
static constexpr volume_dbfs_t MIN_DISPLAYED_DBFS = -60.f; // 0%

static int au3VolumeToLocal(float volume)
{
    //! convert from range 0-1 to -60-0
    float old_max = 1;
    float old_min = 0;
    int old_range = old_max - old_min;

    int new_max = 0;
    int new_min = -60;
    int new_range = new_max - new_min;

    return (((volume - old_min) * new_range) / old_range) + new_min;
}

static float localVolumeToAu3(int volume)
{
    //! convert from range -60-0 to 0-1
    float old_max = 0;
    float old_min = -60;
    int old_range = old_max - old_min;

    int new_max = 1;
    int new_min = 0;
    int new_range = new_max - new_min;

    return (((volume - old_min) * new_range) / old_range) + new_min;
}

PlaybackToolBarLevelItem::PlaybackToolBarLevelItem(const muse::ui::UiAction& action, const ItemType& type, QObject* parent)
    : PlaybackToolBarAbstractItem(action, type, parent)
{
    playback()->audioOutput()->playbackVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume){
        m_level = au3VolumeToLocal(volume);
        emit levelChanged();
    });

    playback()->audioOutput()->playbackSignalChanges()
    .onResolve(this, [this](muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> signalVal) {
        signalVal.onReceive(this, [this](const audioch_t audioChNum, const audio::AudioSignalVal& newValue) {
            if (newValue.pressure < MIN_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum, MIN_DISPLAYED_DBFS);
            } else if (newValue.pressure > MAX_DISPLAYED_DBFS) {
                setAudioChannelVolumePressure(audioChNum, MAX_DISPLAYED_DBFS);
            } else {
                setAudioChannelVolumePressure(audioChNum, newValue.pressure);
            }
        });
    });
}

int PlaybackToolBarLevelItem::level() const
{
    return m_level;
}

void PlaybackToolBarLevelItem::setLevel(int newLevel)
{
    if (m_level == newLevel) {
        return;
    }

    playback()->audioOutput()->setPlaybackVolume(localVolumeToAu3(newLevel));
}

float PlaybackToolBarLevelItem::leftChannelPressure() const
{
    return m_leftChannelPressure;
}

float PlaybackToolBarLevelItem::rightChannelPressure() const
{
    return m_rightChannelPressure;
}

void PlaybackToolBarLevelItem::setLeftChannelPressure(float leftChannelPressure)
{
    if (qFuzzyCompare(m_leftChannelPressure, leftChannelPressure)) {
        return;
    }

    m_leftChannelPressure = leftChannelPressure;
    emit leftChannelPressureChanged(m_leftChannelPressure);
}

void PlaybackToolBarLevelItem::setRightChannelPressure(float rightChannelPressure)
{
    if (qFuzzyCompare(m_rightChannelPressure, rightChannelPressure)) {
        return;
    }

    m_rightChannelPressure = rightChannelPressure;
    emit rightChannelPressureChanged(m_rightChannelPressure);
}

void PlaybackToolBarLevelItem::setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue)
{
    if (chNum == 0) {
        setLeftChannelPressure(newValue);
    } else {
        setRightChannelPressure(newValue);
    }
}

void PlaybackToolBarLevelItem::resetAudioChannelsVolumePressure()
{
    setLeftChannelPressure(MIN_DISPLAYED_DBFS);
    setRightChannelPressure(MIN_DISPLAYED_DBFS);
}
