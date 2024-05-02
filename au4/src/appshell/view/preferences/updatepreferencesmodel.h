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
#ifndef AU_APPSHELL_UPDATEPREFERENCESMODEL_H
#define AU_APPSHELL_UPDATEPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "update/iupdateconfiguration.h"

namespace au::appshell {
class UpdatePreferencesModel : public QObject
{
    Q_OBJECT

    INJECT(update::IUpdateConfiguration, updateConfiguration)

    Q_PROPERTY(
        bool needCheckForNewAppVersion READ needCheckForNewAppVersion WRITE setNeedCheckForNewAppVersion NOTIFY needCheckForNewAppVersionChanged)

public:
    explicit UpdatePreferencesModel(QObject* parent = nullptr);

    bool needCheckForNewAppVersion() const;

    Q_INVOKABLE bool isAppUpdatable() const;
    Q_INVOKABLE QString museScorePrivacyPolicyUrl() const;

public slots:
    void setNeedCheckForNewAppVersion(bool value);

signals:
    void needCheckForNewAppVersionChanged(bool value);
};
}

#endif // AU_APPSHELL_UPDATEPREFERENCESMODEL_H
