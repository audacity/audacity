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
