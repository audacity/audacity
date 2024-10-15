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

#include "iaudiooutput.h"

using namespace au::playback;
using namespace au::audio;

PlaybackToolBarLevelItem::PlaybackToolBarLevelItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                                   QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    playback()->audioOutput()->playbackVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume){
        m_level = volume;
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

    resetAudioChannelsVolumePressure();
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

    playback()->audioOutput()->setPlaybackVolume(newLevel);
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

float PlaybackToolBarLevelItem::leftRecentPeak() const
{
    return m_leftRecentPeak;
}

void PlaybackToolBarLevelItem::setLeftRecentPeak(float newLeftRecentPeak)
{
    if (qFuzzyCompare(m_leftRecentPeak, newLeftRecentPeak)) {
        return;
    }

    m_leftRecentPeak = newLeftRecentPeak;
    emit leftRecentPeakChanged();
}

float PlaybackToolBarLevelItem::leftMaxPeak() const
{
    return m_leftMaxPeak;
}

void PlaybackToolBarLevelItem::setLeftMaxPeak(float newLeftMaxPeak)
{
    if (qFuzzyCompare(m_leftMaxPeak, newLeftMaxPeak)) {
        return;
    }

    m_leftMaxPeak = newLeftMaxPeak;
    emit leftMaxPeakChanged();
}

float PlaybackToolBarLevelItem::rightRecentPeak() const
{
    return m_rightRecentPeak;
}

void PlaybackToolBarLevelItem::setRightRecentPeak(float newRightRecentPeak)
{
    if (qFuzzyCompare(m_rightRecentPeak, newRightRecentPeak)) {
        return;
    }

    m_rightRecentPeak = newRightRecentPeak;
    emit rightRecentPeakChanged();
}

float PlaybackToolBarLevelItem::rightMaxPeak() const
{
    return m_rightMaxPeak;
}

void PlaybackToolBarLevelItem::setRightMaxPeak(float newRightMaxPeak)
{
    if (qFuzzyCompare(m_rightMaxPeak, newRightMaxPeak)) {
        return;
    }

    m_rightMaxPeak = newRightMaxPeak;
    emit rightMaxPeakChanged();
}
