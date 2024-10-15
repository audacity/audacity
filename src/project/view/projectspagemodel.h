/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#ifndef AU_PROJECT_PROJECTSPAGEMODEL_H
#define AU_PROJECT_PROJECTSPAGEMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "actions/iactionsdispatcher.h"
#include "iinteractive.h"
#include "cloud/musescorecom/imusescorecomservice.h"
#include "cloud/audiocom/iaudiocomservice.h"

class QString;

namespace au::project {
class ProjectsPageModel : public QObject
{
    Q_OBJECT

    muse::Inject<IProjectConfiguration> configuration;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<muse::cloud::IAudioComService> audioComService;

    Q_PROPERTY(int tabIndex READ tabIndex WRITE setTabIndex NOTIFY tabIndexChanged)
    Q_PROPERTY(ViewType viewType READ viewType WRITE setViewType NOTIFY viewTypeChanged)

public:
    explicit ProjectsPageModel(QObject* parent = nullptr);

    int tabIndex() const;
    void setTabIndex(int index);

    enum ViewType {
        Grid = int(IProjectConfiguration::HomeProjectsPageViewType::Grid),
        List = int(IProjectConfiguration::HomeProjectsPageViewType::List),
    };
    Q_ENUM(ViewType);

    ViewType viewType() const;
    void setViewType(ViewType type);

    Q_INVOKABLE void createNewProject();
    Q_INVOKABLE void openOther();
    Q_INVOKABLE void openProject(const QString& scorePath, const QString& displayNameOverride);
    Q_INVOKABLE void openProjectManager();

signals:
    void tabIndexChanged();
    void viewTypeChanged();
};
}

#endif // AU_PROJECT_PROJECTSPAGEMODEL_H
