/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
#ifndef AU_PROJECT_PROJECTERRORS_H
#define AU_PROJECT_PROJECTERRORS_H

#include "types/ret.h"
#include "global/translation.h"

namespace au::project {
enum class Err {
    Undefined       = int(muse::Ret::Code::Undefined),
    NoError         = int(muse::Ret::Code::Ok),
    UnknownError    = int(muse::Ret::Code::ProjectFirst),

    NoProjectError,
    NoPartsError,
    CorruptionError,
    CorruptionUponOpeningError,

    ProjectFileNotFound,
    ProjectFileIsReadProtected,
    ProjectFileIsWriteProtected,
    InvalidCloudProjectId,

    UnsupportedUrl,
    MalformedOpenProjectUrl,
};

inline muse::Ret make_ret(Err e)
{
    return muse::Ret(static_cast<int>(e));
}

inline muse::Ret make_ret(Err err, const muse::io::path_t& filePath)
{
    muse::String text;

    switch (err) {
    case Err::NoError:
        return muse::make_ok();
    // case Err::FileUnknownError:
    // text = muse::mtrc("project", "Unknown error");
    // break;
    case Err::ProjectFileNotFound:
        text = muse::mtrc("project", "Project “%1” not found").arg(filePath.toString());
        break;
    case Err::ProjectFileIsReadProtected:
        text = muse::mtrc("project", "Project file “%1” is read-protected and cannot be opened").arg(filePath.toString());
        break;
    case Err::ProjectFileIsWriteProtected:
        text = muse::mtrc("project", "Project file “%1” is write-protected and cannot be opened").arg(filePath.toString());
        break;
    // case Err::FileBadFormat:
    // text = muse::mtrc("project", "Bad format");
    // break;
    // case Err::FileUnknownType:
    // text = muse::mtrc("project", "Unknown filetype");
    // break;
    // case Err::FileTooOld:
    //     text = muse::mtrc("project", "This file was last saved in a version older than 2.0.0. "
    //                                    "You can convert this score by opening and then "
    //                                    "saving in MuseScore version 2.x. "
    //                                    "Visit the <a href=\"%1\">MuseScore download page</a> to obtain such a 2.x version.")
    //            .arg(u"https://musescore.org/download#older-versions");
    //     break;
    // case Err::FileTooNew:
    //     text = muse::mtrc("project", "This file was saved using a newer version of MuseScore Studio. "
    //                                    "Please visit <a href=\"%1\">MuseScore.org</a> to obtain the latest version.")
    //            .arg(u"https://musescore.org");
    //     break;
    // case Err::FileOld300Format:
    //     text = muse::mtrc("project", "This file was last saved in a development version of 3.0.");
    //     break;
    // case Err::FileCorrupted:
    //     text = muse::mtrc("project", "File “%1” is corrupted.").arg(filePath.toString());
    //     break;
    // case Err::FileCriticallyCorrupted:
    //     text = muse::mtrc("project", "File “%1” is critically corrupted and cannot be processed.").arg(filePath.toString());
    //     break;
    case Err::Undefined:
    case Err::UnknownError:
        // case Err::IgnoreError:
        // case Err::UserAbort:
        break;
    }

    return muse::Ret(static_cast<int>(err), text.toStdString());
}
}

#endif // AU_PROJECT_PROJECTERRORS_H
