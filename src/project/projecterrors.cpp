/*
* SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity BVBA and others
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
#include "projecterrors.h"

#include "global/translation.h"
#include "global/io/path.h"

namespace au::project {
muse::Ret make_ret(Err err, const muse::io::path_t& filePath)
{
    auto ret = muse::Ret(static_cast<int>(err));

    muse::String title = muse::mtrc("project", "Cannot read file %1").arg(muse::io::toNativeSeparators(filePath).toString());
    muse::String body;
    muse::String path = muse::io::toNativeSeparators(filePath).toString();

    switch (err) {
    case Err::NoError:
        return muse::make_ok();
    case Err::AudacityExceptionError:
        title = muse::mtrc("project", "Audacity exception occurred");
        body = muse::mtrc("project", "An Audacity exception occurred while trying to open the project file:\n“%1”").arg(muse::io::toNativeSeparators(
                                                                                                                            filePath).toString());
        break;
    case Err::DatabaseError:
        title = muse::mtrc("project", "Database error");
        body = muse::mtrc("project",
                          "An error occurred while trying to access the project database.\nFailed to load project: “%1”").arg(
            filePath.toString());
        break;
    case Err::ProjectFileNotFound:
        title = muse::mtrc("project", "Cannot read file %1").arg(muse::io::toNativeSeparators(filePath).toString());
        body = muse::mtrc("project",
                          "The file:\n“%1”\nCannot be found or accessed at this location. If it’s stored on an external or cloud drive, please verify that the drive is connected and syncing properly.")
               .arg(muse::io::toNativeSeparators(filePath).toString());
        break;
    case Err::ProjectFileIsReadProtected:
        title = muse::mtrc("project", "This file cannot be opened due to access restrictions");
        body = muse::mtrc("project",
                          "To open this file:\n“%1”\nPlease check the file’s properties and permissions, ensure it is not stored on a drive or folder with restricted access, or try running Audacity as an administrator.")
               .arg(muse::io::toNativeSeparators(filePath).toString());
        break;
    case Err::ProjectFileIsWriteProtected:
        title = muse::mtrc("project",
                           "This file is write-protected and cannot be opened");
        body = muse::mtrc("project",
                          "To open this file:\n“%1”\nPlease remove the write protection by checking the file’s properties, ensuring it is not stored on a write-protected drive or folder, or by running Audacity as an administrator.")
               .arg(muse::io::toNativeSeparators(filePath).toString());
        break;
    case Err::UnknownError:
    case Err::Undefined:
    default:
        break;
    }

    ret.setData("title", title.toStdString());
    ret.setData("body", body.toStdString());
    ret.setData("path", path);

    return ret;
}
} // namespace au::project
