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
#ifndef AU_APPSHELL_SAVEANDPUBLISHPREFERENCESMODEL_H
#define AU_APPSHELL_SAVEANDPUBLISHPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "async/asyncable.h"

#include "project/iprojectconfiguration.h"

namespace au::appshell {
class SaveAndPublishPreferencesModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(project::IProjectConfiguration, projectConfiguration)

    Q_PROPERTY(bool isAutoSaveEnabled READ isAutoSaveEnabled WRITE setAutoSaveEnabled NOTIFY autoSaveEnabledChanged)
    Q_PROPERTY(int autoSaveInterval READ autoSaveInterval WRITE setAutoSaveInterval NOTIFY autoSaveIntervalChanged)
    Q_PROPERTY(int alsoShareAudioCom READ alsoShareAudioCom WRITE setAlsoShareAudioCom NOTIFY alsoShareAudioComChanged)

public:
    explicit SaveAndPublishPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    bool isAutoSaveEnabled() const;
    int autoSaveInterval() const;
    bool alsoShareAudioCom() const;

public slots:
    void setAutoSaveEnabled(bool enabled);
    void setAutoSaveInterval(int minutes);
    void setAlsoShareAudioCom(bool share);

signals:
    void autoSaveEnabledChanged(bool enabled);
    void autoSaveIntervalChanged(int minutes);
    void alsoShareAudioComChanged(int prompt);
};
}

#endif // AU_APPSHELL_SAVEANDPUBLISHPREFERENCESMODEL_H
