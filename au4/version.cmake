# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2024 MuseScore BVBA and others
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

set(MUSESCORE_NAME "Audacity")
set(MUSESCORE_VERSION_MAJOR "4")
set(MUSESCORE_VERSION_MINOR "0")
set(MUSESCORE_VERSION_PATCH "0")
set(MUSESCORE_VERSION_MAJ_MIN "${MUSESCORE_VERSION_MAJOR}.${MUSESCORE_VERSION_MINOR}")
set(MUSESCORE_VERSION "${MUSESCORE_VERSION_MAJ_MIN}.${MUSESCORE_VERSION_PATCH}")
set(MUSESCORE_VERSION_LABEL "")

if(MUSESCORE_BUILD_MODE MATCHES "dev")
    set(MUSESCORE_RELEASE_CHANNEL "devel")
endif()

if(MUSESCORE_BUILD_MODE MATCHES "testing")
    set(MUSESCORE_RELEASE_CHANNEL "testing")
endif()

if(MUSESCORE_BUILD_MODE MATCHES "release")
    set(MUSESCORE_RELEASE_CHANNEL "stable")
endif()

# Print variables which are needed by CI build scripts.
message(STATUS "MUSESCORE_RELEASE_CHANNEL ${MUSESCORE_RELEASE_CHANNEL}")
message(STATUS "MUSESCORE_VERSION ${MUSESCORE_VERSION}")
