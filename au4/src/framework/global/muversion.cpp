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
#include "muversion.h"

#include "types/version.h"

using namespace mu;

bool MUVersion::unstable()
{
#ifdef MUSESCORE_UNSTABLE
    return true;
#else
    return false;
#endif
}

mu::String MUVersion::version()
{
    //! TODO AU4
    return String();
    //return String::fromStdString(MUSESCORE_VERSION);
}

mu::String MUVersion::fullVersion()
{
    //! TODO AU4
    return String();
    // Version res(version());

    // String versionLabel = String::fromStdString(MUSESCORE_VERSION_LABEL);
    // if (!versionLabel.isEmpty()) {
    //     res.setSuffix(versionLabel);
    // }

    // return res.toString();
}

mu::String MUVersion::revision()
{
    //! TODO AU4
    return String();
    // return String::fromStdString(MUSESCORE_REVISION);
}

int MUVersion::majorVersion()
{
    return Version(version()).majorVersion();
}

int MUVersion::minorVersion()
{
    return Version(version()).minorVersion();
}

int MUVersion::patchVersion()
{
    return Version(version()).patchVersion();
}
