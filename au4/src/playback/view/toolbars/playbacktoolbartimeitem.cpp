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
#include "playbacktoolbartimeitem.h"

#include <QVariantMap>

#include "playback/iaudiooutput.h"

using namespace au::playback;
using namespace au::audio;

PlaybackToolBarTimeItem::PlaybackToolBarTimeItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                                 QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](audio::secs_t secs) {
        if (secs.is_negative()) {
            return;
        }

        emit currentValueChanged();
    });

    playback()->audioOutput()->sampleRateChanged().onReceive(this, [this](audio::sample_rate_t) {
        emit sampleRateChanged();
    });

    globalContext()->currentProcessingProjectChanged().onNotify(this, [this](){
        auto project = globalContext()->currentProcessingProject();
        if (!project) {
            return;
        }

        emit timeSignatureChanged();
    });
}

int PlaybackToolBarTimeItem::currentFormat() const
{
    return m_currentFormat; // from settings
}

void PlaybackToolBarTimeItem::setCurrentFormat(int format)
{
    if (m_currentFormat == format) {
        return;
    }

    m_currentFormat = format;
    emit currentFormatChanged();
}

double PlaybackToolBarTimeItem::currentValue() const
{
    return playbackState()->playbackPosition();
}

void PlaybackToolBarTimeItem::setCurrentValue(double value)
{
    if (currentValue() == value) {
        return;
    }

    dispatcher()->dispatch("playback_seek", muse::actions::ActionData::make_arg1(value));
}

au::context::IPlaybackStatePtr PlaybackToolBarTimeItem::playbackState() const
{
    return globalContext()->playbackState();
}

double PlaybackToolBarTimeItem::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double PlaybackToolBarTimeItem::tempo() const
{
    auto project = globalContext()->currentProcessingProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().tempo;
}

int PlaybackToolBarTimeItem::upperTimeSignature() const
{
    auto project = globalContext()->currentProcessingProject();
    if (!project) {
        return 0;
    }

    return project->timeSignature().upper;
}

int PlaybackToolBarTimeItem::lowerTimeSignature() const
{
    auto project = globalContext()->currentProcessingProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().lower;
}
