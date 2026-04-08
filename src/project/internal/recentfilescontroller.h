/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-Studio-CLA-applies
 *
 * Audacity Studio
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity Limited
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
#pragma once

#include "irecentfilescontroller.h"

#include <mutex>
#include <map>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "project/iprojectconfiguration.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/multiwindows/imultiwindowsprovider.h"

namespace au::project {
class RecentFilesController : public IRecentFilesController, public muse::async::Asyncable
{
    muse::GlobalInject<IProjectConfiguration> configuration;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiwindowsProvider;

public:
    void init();

    const RecentFilesList& recentFilesList() const override;
    muse::async::Notification recentFilesListChanged() const override;

    void prependRecentFile(const RecentFile& file) override;
    void moveRecentFile(const muse::io::path_t& before, const RecentFile& after) override;
    void clearRecentFiles() override;

protected:
    virtual void prependPlatformRecentFile(const muse::io::path_t& path);
    virtual void clearPlatformRecentFiles();

private:
    void loadRecentFilesList();
    void removeNonexistentFiles();
    void setRecentFilesList(const RecentFilesList& list, bool saveAndNotify);
    void saveRecentFilesList() const;

    mutable bool m_dirty = true;
    mutable RecentFilesList m_recentFilesList;
    muse::async::Notification m_recentFilesListChanged;
    mutable bool m_isSaving = false;

    struct CachedThumbnail {
        QPixmap thumbnail;
        muse::DateTime lastModified;
    };

    mutable std::mutex m_thumbnailCacheMutex;
    mutable std::map<muse::io::path_t, CachedThumbnail> m_thumbnailCache;
};
}
