/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore Limited
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
#include "recentfilescontroller.h"

#include "global/concurrency/concurrent.h"
#include "global/async/async.h"
#include "global/defer.h"
#include "global/serialization/json.h"

#include "multiinstances/resourcelockguard.h"

using namespace mu::project;
using namespace muse;
using namespace muse::async;

static const std::string RECENT_FILES_RESOURCE_NAME("RECENT_FILES");

void RecentFilesController::init()
{
    TRACEFUNC;

    m_dirty = true;

    multiInstancesProvider()->resourceChanged().onReceive(this, [this](const std::string& resourceName) {
        if (resourceName == RECENT_FILES_RESOURCE_NAME) {
            if (!m_isSaving) {
                m_dirty = true;

                m_recentFilesListChanged.notify();
            }
        }
    });
}

const RecentFilesList& RecentFilesController::recentFilesList() const
{
    TRACEFUNC;

    if (m_dirty) {
        const_cast<RecentFilesController*>(this)->loadRecentFilesList();
    }

    const_cast<RecentFilesController*>(this)->removeNonexistentFiles();

    return m_recentFilesList;
}

Notification RecentFilesController::recentFilesListChanged() const
{
    return m_recentFilesListChanged;
}

void RecentFilesController::prependRecentFile(const RecentFile& newFile)
{
    if (!newFile.isValid()) {
        return;
    }

    TRACEFUNC;

    RecentFilesList newList;
    newList.reserve(m_recentFilesList.size() + 1);
    newList.push_back(newFile);

    for (const RecentFile& file : m_recentFilesList) {
        if (file.path != newFile.path && fileSystem()->exists(file.path)) {
            newList.push_back(file);
        }
    }

    setRecentFilesList(newList, true);

    prependPlatformRecentFile(newFile.path);
}

void RecentFilesController::moveRecentFile(const muse::io::path_t& before, const RecentFile& after)
{
    bool moved = false;
    RecentFilesList newList = m_recentFilesList;

    for (RecentFile& file : newList) {
        if (file.path == before) {
            file = after;
            moved = true;
            break;
        }
    }

    if (moved) {
        setRecentFilesList(newList, true);
    }
}

void RecentFilesController::clearRecentFiles()
{
    setRecentFilesList({}, true);

    clearPlatformRecentFiles();
}

void RecentFilesController::prependPlatformRecentFile(const muse::io::path_t&) {}

void RecentFilesController::clearPlatformRecentFiles() {}

void RecentFilesController::loadRecentFilesList()
{
    RecentFilesList newList;

    DEFER {
        setRecentFilesList(newList, false);
    };

    RetVal<ByteArray> data;
    {
        mi::ReadResourceLockGuard lock_guard(multiInstancesProvider(), RECENT_FILES_RESOURCE_NAME);
        data = fileSystem()->readFile(configuration()->recentFilesJsonPath());
    }

    if (!data.ret || data.val.empty()) {
        data.val = configuration()->compatRecentFilesData();
    }

    if (data.val.empty()) {
        return;
    }

    std::string err;
    const JsonDocument json = JsonDocument::fromJson(data.val, &err);
    if (!err.empty()) {
        LOGE() << "Loading JSON failed: " << err;
        return;
    }

    if (!json.isArray()) {
        return;
    }

    const JsonArray array = json.rootArray();
    for (size_t i = 0; i < array.size(); ++i) {
        const JsonValue val = array.at(i);

        if (val.isString()) {
            newList.emplace_back(muse::io::path_t(val.toStdString()));
        } else if (val.isObject()) {
            const JsonObject obj = val.toObject();
            RecentFile file;
            file.path = obj["path"].toStdString();
            file.displayNameOverride = QString::fromStdString(obj["displayName"].toStdString());
            newList.emplace_back(std::move(file));
        } else {
            continue;
        }
    }
}

void RecentFilesController::removeNonexistentFiles()
{
    bool removed = false;

    RecentFilesList newList;
    newList.reserve(m_recentFilesList.size());

    for (const RecentFile& file : m_recentFilesList) {
        if (fileSystem()->exists(file.path)) {
            newList.push_back(file);
        } else {
            removed = true;
        }
    }

    if (removed) {
        setRecentFilesList(newList, false);

        async::Async::call(nullptr, [this, newList]() {
            saveRecentFilesList();

            m_recentFilesListChanged.notify();
        });
    }
}

void RecentFilesController::setRecentFilesList(const RecentFilesList& list, bool saveAndNotify)
{
    if (m_recentFilesList == list) {
        return;
    }

    m_recentFilesList = list;

    cleanUpThumbnailCache(list);

    if (saveAndNotify) {
        saveRecentFilesList();

        m_recentFilesListChanged.notify();
    }
}

void RecentFilesController::saveRecentFilesList()
{
    TRACEFUNC;

    m_isSaving = true;

    DEFER {
        m_isSaving = false;
    };

    JsonArray jsonArray;
    for (const RecentFile& file : m_recentFilesList) {
        if (!file.displayNameOverride.isEmpty()) {
            JsonObject obj;
            obj["path"] = file.path.toStdString();
            obj["displayName"] = file.displayNameOverride.toStdString();
            jsonArray << obj;
        } else {
            jsonArray << file.path.toStdString();
        }
    }

    JsonDocument json(jsonArray);

    mi::WriteResourceLockGuard resource_guard(multiInstancesProvider(), RECENT_FILES_RESOURCE_NAME);
    Ret ret = fileSystem()->writeFile(configuration()->recentFilesJsonPath(), json.toJson());
    if (!ret) {
        LOGE() << "Failed to save recent files list: " << ret.toString();
    }
}

Promise<QPixmap> RecentFilesController::thumbnail(const muse::io::path_t& filePath) const
{
    return Promise<QPixmap>([this, filePath](auto resolve, auto reject) {
        if (filePath.empty()) {
            return reject(int(Ret::Code::UnknownError), "Invalid file specified");
        }

        Concurrent::run([this, filePath, resolve, reject]() {
            std::lock_guard lock(m_thumbnailCacheMutex);

            DateTime lastModified = fileSystem()->lastModified(filePath);

            auto it = m_thumbnailCache.find(filePath);
            if (it != m_thumbnailCache.cend()) {
                if (lastModified == it->second.lastModified) {
                    (void)resolve(it->second.thumbnail);
                    return;
                }
            }

            RetVal<ProjectMeta> rv = mscMetaReader()->readMeta(filePath);
            if (!rv.ret) {
                m_thumbnailCache[filePath] = CachedThumbnail();
                (void)reject(rv.ret.code(), rv.ret.toString());
            } else {
                m_thumbnailCache[filePath] = CachedThumbnail { rv.val.thumbnail, lastModified };
                (void)resolve(rv.val.thumbnail);
            }
        });

        return Promise<QPixmap>::Result::unchecked();
    }, Promise<QPixmap>::AsynchronyType::ProvidedByBody);
}

void RecentFilesController::cleanUpThumbnailCache(const RecentFilesList& files)
{
    Concurrent::run([this, files] {
        std::lock_guard lock(m_thumbnailCacheMutex);

        if (files.empty()) {
            m_thumbnailCache.clear();
        } else {
            std::map<muse::io::path_t, CachedThumbnail> cleanedCache;

            for (const RecentFile& file : files) {
                auto it = m_thumbnailCache.find(file.path);
                if (it != m_thumbnailCache.cend()) {
                    cleanedCache[file.path] = it->second;
                }
            }

            m_thumbnailCache = cleanedCache;
        }
    });
}
