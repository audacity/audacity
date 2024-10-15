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
#include "cloudprojectsmodel.h"

#include "dataformatter.h"

#include "log.h"

using namespace muse;
using namespace au::project;

static const int BATCH_SIZE = 20;

CloudProjectsModel::CloudProjectsModel(QObject* parent)
    : AbstractProjectsModel(parent)
{
}

void CloudProjectsModel::load()
{
    auto onUserAuthorizedChanged = [this](bool authorized) {
        if (authorized) {
            setState(State::Loading);
            loadItemsIfNecessary();
        } else {
            setState(State::NotSignedIn);
        }
    };

    ValCh<bool> authorized = museScoreComService()->authorization()->userAuthorized();

    onUserAuthorizedChanged(authorized.val);

    authorized.ch.onReceive(this, onUserAuthorizedChanged);

    connect(this, &CloudProjectsModel::desiredRowCountChanged, this, &CloudProjectsModel::loadItemsIfNecessary);
}

void CloudProjectsModel::reload()
{
    beginResetModel();

    m_items.clear();
    m_totalItems = muse::nidx;
    m_desiredRowCount = 0;

    endResetModel();

    emit hasMoreChanged();
    emit desiredRowCountChanged();

    setState(State::Loading);
}

CloudProjectsModel::State CloudProjectsModel::state() const
{
    return m_state;
}

void CloudProjectsModel::setState(State state)
{
    if (m_state == state) {
        return;
    }

    m_state = state;
    emit stateChanged();
}

bool CloudProjectsModel::hasMore() const
{
    return m_totalItems == muse::nidx || m_items.size() < m_totalItems;
}

int CloudProjectsModel::desiredRowCount() const
{
    return m_desiredRowCount;
}

void CloudProjectsModel::setDesiredRowCount(int count)
{
    if (m_desiredRowCount == count) {
        return;
    }

    m_desiredRowCount = count;
    emit desiredRowCountChanged();
}

void CloudProjectsModel::loadItemsIfNecessary()
{
    if (m_isWaitingForPromise) {
        return;
    }

    if (m_state == State::Error || m_state == State::NotSignedIn) {
        return;
    }

    if (needsLoading()) {
        setState(State::Loading);

        m_isWaitingForPromise = true;

        // museScoreComService()->downloadScoresList(BATCH_SIZE, static_cast<int>(m_items.size()) / BATCH_SIZE + 1)
        // .onResolve(this, [this](const cloud::ScoresList& scoresList) {
        //     if (!scoresList.items.empty()) {
        //         beginInsertRows(QModelIndex(), static_cast<int>(m_items.size()),
        //                         static_cast<int>(m_items.size() + scoresList.items.size()) - 1);

        //         for (const cloud::ScoresList::Item& item : scoresList.items) {
        //             QVariantMap obj;

        //             obj[NAME_KEY] = item.title;
        //             obj[PATH_KEY] = configuration()->cloudProjectPath(item.id).toQString();
        //             obj[SUFFIX_KEY] = "";
        //             obj[FILE_SIZE_KEY] = (item.fileSize > 0) ? DataFormatter::formatFileSize(item.fileSize).toQString() : QString();
        //             obj[IS_CLOUD_KEY] = true;
        //             obj[CLOUD_SCORE_ID_KEY] = item.id;
        //             obj[TIME_SINCE_MODIFIED_KEY] = DataFormatter::formatTimeSince(Date::fromQDate(item.lastModified.date())).toQString();
        //             obj[THUMBNAIL_URL_KEY] = item.thumbnailUrl;
        //             obj[IS_CREATE_NEW_KEY] = false;
        //             obj[IS_NO_RESULTS_FOUND_KEY] = false;
        //             obj[CLOUD_VISIBILITY_KEY] = static_cast<int>(item.visibility);
        //             obj[CLOUD_VIEW_COUNT_KEY] = item.viewCount;

        //             m_items.push_back(obj);
        //         }

        //         endInsertRows();
        //     }

        //     m_totalItems = scoresList.meta.totalScoresCount;
        //     emit hasMoreChanged();

        //     m_isWaitingForPromise = false;

        //     loadItemsIfNecessary();
        // })
        // .onReject(this, [this](int code, const std::string& err) {
        //     LOGE() << "Loading scores list failed: [" << code << "] " << err;
        //     setState(State::Error);
        //     m_isWaitingForPromise = false;
        // });
    } else {
        setState(State::Fine);
    }
}

bool CloudProjectsModel::needsLoading()
{
    return hasMore() && static_cast<int>(m_items.size()) < m_desiredRowCount;
}
