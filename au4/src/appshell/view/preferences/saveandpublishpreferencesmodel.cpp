/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#include "saveandpublishpreferencesmodel.h"

using namespace au::appshell;

SaveAndPublishPreferencesModel::SaveAndPublishPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void SaveAndPublishPreferencesModel::load()
{
    projectConfiguration()->autoSaveEnabledChanged().onReceive(this, [this](bool enabled) {
        emit autoSaveEnabledChanged(enabled);
    });

    projectConfiguration()->autoSaveIntervalChanged().onReceive(this, [this](int minutes) {
        emit autoSaveIntervalChanged(minutes);
    });

    projectConfiguration()->alsoShareAudioComChanged().onReceive(this, [this](bool share) {
        emit alsoShareAudioComChanged(share);
    });
}

bool SaveAndPublishPreferencesModel::isAutoSaveEnabled() const
{
    return projectConfiguration()->isAutoSaveEnabled();
}

int SaveAndPublishPreferencesModel::autoSaveInterval() const
{
    return projectConfiguration()->autoSaveIntervalMinutes();
}

bool SaveAndPublishPreferencesModel::alsoShareAudioCom() const
{
    return projectConfiguration()->alsoShareAudioCom();
}

void SaveAndPublishPreferencesModel::setAutoSaveEnabled(bool enabled)
{
    if (enabled == isAutoSaveEnabled()) {
        return;
    }

    projectConfiguration()->setAutoSaveEnabled(enabled);
}

void SaveAndPublishPreferencesModel::setAutoSaveInterval(int minutes)
{
    if (minutes == autoSaveInterval()) {
        return;
    }

    projectConfiguration()->setAutoSaveInterval(minutes);
}

void SaveAndPublishPreferencesModel::setAlsoShareAudioCom(bool share)
{
    if (share == alsoShareAudioCom()) {
        return;
    }

    projectConfiguration()->setAlsoShareAudioCom(share);
}
