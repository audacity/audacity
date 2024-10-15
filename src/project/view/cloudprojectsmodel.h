/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
#ifndef AU_PROJECT_CLOUDPROJECTSMODEL_H
#define AU_PROJECT_CLOUDPROJECTSMODEL_H

#include "abstractprojectsmodel.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "cloud/musescorecom/imusescorecomservice.h"

namespace au::project {
class CloudProjectsModel : public AbstractProjectsModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::project::IProjectConfiguration> configuration;
    muse::Inject<muse::cloud::IMuseScoreComService> museScoreComService;

    Q_PROPERTY(State state READ state NOTIFY stateChanged)
    Q_PROPERTY(bool hasMore READ hasMore NOTIFY hasMoreChanged)

    Q_PROPERTY(int desiredRowCount READ desiredRowCount WRITE setDesiredRowCount NOTIFY desiredRowCountChanged)

public:
    CloudProjectsModel(QObject* parent = nullptr);

    enum class State {
        Fine,
        Loading,
        NotSignedIn,
        Error
    };
    Q_ENUM(State)

    void load() override;
    Q_INVOKABLE void reload();

    State state() const;
    bool hasMore() const;

    // Used by the view to request more items
    int desiredRowCount() const;
    void setDesiredRowCount(int count);

signals:
    void stateChanged();
    void hasMoreChanged();

    void desiredRowCountChanged();

private:
    void setState(State state);

    void loadItemsIfNecessary();
    bool needsLoading();

    State m_state = State::Fine;
    bool m_isWaitingForPromise = false;

    size_t m_totalItems = muse::nidx;

    int m_desiredRowCount = 0;
};
}

#endif // AU_PROJECT_CLOUDPROJECTSMODEL_H
