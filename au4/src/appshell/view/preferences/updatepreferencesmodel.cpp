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

#include "updatepreferencesmodel.h"

#include "log.h"
#include "translation.h"

using namespace au::appshell;

UpdatePreferencesModel::UpdatePreferencesModel(QObject* parent)
    : QObject(parent)
{
}

bool UpdatePreferencesModel::isAppUpdatable() const
{
    return updateConfiguration()->isAppUpdatable();
}

bool UpdatePreferencesModel::needCheckForNewAppVersion() const
{
    return updateConfiguration()->needCheckForUpdate();
}

void UpdatePreferencesModel::setNeedCheckForNewAppVersion(bool value)
{
    if (value == needCheckForNewAppVersion()) {
        return;
    }

    updateConfiguration()->setNeedCheckForUpdate(value);
    emit needCheckForNewAppVersionChanged(value);
}

QString UpdatePreferencesModel::museScorePrivacyPolicyUrl() const
{
    return QString::fromStdString(updateConfiguration()->museScorePrivacyPolicyUrl());
}
