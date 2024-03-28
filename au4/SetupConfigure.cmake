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

include(GetBuildType)
include(GetPlatformInfo)
include(version)

if (NOT MUSESCORE_BUILD_CONFIGURATION)
    set(MUSESCORE_BUILD_CONFIGURATION "app")
endif()

if (NOT MUSESCORE_BUILD_MODE)
    set(MUSESCORE_BUILD_MODE "dev")
endif()

# Set revision for local builds
# TODO AU4
#include(TryUseLocalRevision)

message(STATUS "MUSESCORE_BUILD_CONFIGURATION: ${MUSESCORE_BUILD_CONFIGURATION}")
message(STATUS "MUSESCORE_BUILD_MODE: ${MUSESCORE_BUILD_MODE}")
message(STATUS "MUSESCORE_BUILD_NUMBER: ${CMAKE_BUILD_NUMBER}")

string(TOUPPER ${MUSESCORE_BUILD_CONFIGURATION} BUILD_CONFIGURE)
string(TOUPPER ${MUSESCORE_BUILD_MODE} BUILD_MODE)

###########################################
# Setup by mode
###########################################
if(BUILD_MODE MATCHES "DEV")
    set(MUSESCORE_UNSTABLE ON)
    set(MUSESCORE_RELEASE_CHANNEL "dev")
    set(MUSESCORE_NAME_VERSION "${MUSESCORE_NAME} ${MUSESCORE_VERSION_MAJOR} ${MUSESCORE_RELEASE_CHANNEL}")
    set(MUSESCORE_IS_PRERELEASE ON)
    set(MUSESCORE_ALLOW_UPDATE_ON_PRERELEASE OFF)
endif()

if(BUILD_MODE MATCHES "TESTING")
    set(MUSESCORE_UNSTABLE OFF)
    set(MUSESCORE_RELEASE_CHANNEL "Testing")
    set(MUSESCORE_NAME_VERSION "${MUSESCORE_NAME} ${MUSESCORE_VERSION_MAJOR} ${MUSESCORE_RELEASE_CHANNEL}")
    set(MUSESCORE_IS_PRERELEASE ON)
    set(MUSESCORE_ALLOW_UPDATE_ON_PRERELEASE ON)
endif()

if(BUILD_MODE MATCHES "RELEASE")
    set(MUSESCORE_UNSTABLE OFF)
    set(MUSESCORE_NAME_VERSION "${MUSESCORE_NAME} ${MUSESCORE_VERSION_MAJOR}")
    set(MUSESCORE_IS_PRERELEASE OFF)
    set(MUSESCORE_ALLOW_UPDATE_ON_PRERELEASE OFF)
endif()

###########################################
# Setup paths
###########################################
if (OS_IS_MAC)
    SET(Mscore_INSTALL_NAME  "Contents/Resources/")
    SET(Mscore_SHARE_NAME    "mscore.app/")
elseif (OS_IS_WIN)
    SET(Mscore_INSTALL_NAME  "")
    SET(Mscore_SHARE_NAME    "./")
else()
    SET(Mscore_INSTALL_NAME  "mscore${MUSESCORE_INSTALL_SUFFIX}-${MUSESCORE_VERSION_MAJ_MIN}/")
    SET(Mscore_SHARE_NAME    "share/")
endif()

###########################################
# CONFIGURE: Desktop App
###########################################
set(MU_GENERAL_APP OFF)
if(BUILD_CONFIGURE MATCHES "APP")
    set(MU_GENERAL_APP ON)
endif()

if(BUILD_CONFIGURE MATCHES "APP-PORTABLE")
    set(MU_GENERAL_APP ON)
    set(WIN_PORTABLE ON)
endif()

if (MU_GENERAL_APP)
    if (BUILD_IS_DEBUG)
        set(MUE_ENABLE_LOGGER_DEBUGLEVEL ON)
    else()
        set(MUE_ENABLE_LOGGER_DEBUGLEVEL OFF)
    endif()
endif()

if (WIN_PORTABLE)
    set(MU_BUILD_UPDATE_MODULE OFF)
endif()
if (OS_IS_FBSD)
    message(WARNING "Not building unsupported chrashpad client on FreeBSD")
    set(MU_BUILD_CRASHPAD_CLIENT OFF)
endif()

###########################################
# CONFIGURE: UTest
###########################################
if(BUILD_CONFIGURE MATCHES "UTEST")
    set(MU_BUILD_UNIT_TESTS ON)
    set(MU_ENABLE_LOGGER_DEBUGLEVEL ON)
    set(MU_BUILD_ASAN ON)

    message(STATUS "If you added tests to a module that didn't have them yet, make sure that this module is enabled, see SetupConfigure.cmake")
    set(MUE_BUILD_MIDI_MODULE OFF)
    set(MUE_BUILD_MUSESAMPLER_MODULE OFF)
    set(MUE_BUILD_NETWORK_MODULE OFF)
    set(MUE_BUILD_SHORTCUTS_MODULE OFF)

    set(MUE_BUILD_APPSHELL_MODULE OFF)
    set(MUE_BUILD_AUTOBOT_MODULE OFF)
    set(MUE_BUILD_CLOUD_MODULE OFF)
    set(MUE_BUILD_CONVERTER_MODULE OFF)
    set(MUE_BUILD_INSPECTOR_MODULE OFF)
    set(MUE_BUILD_INSTRUMENTSSCENE_MODULE OFF)
    set(MUE_BUILD_LANGUAGES_MODULE OFF)
    set(MUE_BUILD_LEARN_MODULE OFF)
    set(MUE_BUILD_MULTIINSTANCES_MODULE OFF)
    set(MUE_BUILD_PALETTE_MODULE OFF)
    set(MUE_BUILD_PLAYBACK_MODULE OFF)
    set(MU_BUILD_UPDATE_MODULE OFF)
    set(MUE_BUILD_WORKSPACE_MODULE OFF)
endif()

###########################################
# Subsystem
###########################################

set(QT_SUPPORT ON)

if (NOT MUE_BUILD_AUDIO_MODULE)
    set(MUE_BUILD_MUSESAMPLER_MODULE OFF)
    set(MUE_BUILD_VST_MODULE OFF)
endif()

if (MUE_ENABLE_AUDIO_JACK)
    add_definitions(-DJACK_AUDIO)
endif()

if (NOT MUE_BUILD_IMPORTEXPORT_MODULE)
    set(MUE_BUILD_VIDEOEXPORT_MODULE OFF)
endif()

if (NOT MUE_BUILD_DIAGNOSTICS_MODULE)
    set(MU_BUILD_CRASHPAD_CLIENT OFF)
endif()

if (MUE_BUILD_ASAN)
    set(MUE_ENABLE_CUSTOM_ALLOCATOR OFF)
endif()

if (NOT MUE_BUILD_NOTATION_MODULE)
    set(MUE_BUILD_PROJECT_MODULE OFF) # hard dependency
    set(MUE_BUILD_PALETTE_MODULE OFF) # hard dependency
endif()

if (NOT MUE_BUILD_UI_MODULE)
    set(MUE_BUILD_APPSHELL_MODULE OFF) # hard dependency
endif()

###########################################
# Unit tests
###########################################
if (NOT MU_BUILD_UNIT_TESTS)

    set(MU_BUILD_ACCESSIBILITY_TESTS OFF)
    set(MU_BUILD_AUDIO_TESTS OFF)
    set(MU_BUILD_DRAW_TESTS OFF)
    set(MU_BUILD_GLOBAL_TESTS OFF)
    set(MU_BUILD_NETWORK_TESTS OFF)
    set(MU_BUILD_UI_TESTS OFF)

    set(MU_BUILD_DIAGNOSTICS_TESTS OFF)
    set(MU_BUILD_EXTENSIONS_TESTS OFF)
    set(MU_BUILD_PROJECT_TESTS OFF)
    set(MU_BUILD_UPDATE_TESTS OFF)

endif()

###########################################
# Global definitions
###########################################

add_definitions(-DMU_APP_NAME="${MUSESCORE_NAME}")
add_definitions(-DMU_APP_REVISION="${MUSESCORE_REVISION}")
add_definitions(-DMU_APP_BUILD_NUMBER="${CMAKE_BUILD_NUMBER}")
add_definitions(-DMU_APP_VERSION="${MUSESCORE_VERSION}")
add_definitions(-DMU_APP_VERSION_LABEL="${MUSESCORE_VERSION_LABEL}")
add_definitions(-DMU_APP_INSTALL_SUFFIX="${MUSESCORE_INSTALL_SUFFIX}")
add_definitions(-DMU_APP_INSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")
add_definitions(-DMU_APP_INSTALL_NAME="${Mscore_INSTALL_NAME}")

if (MUSESCORE_UNSTABLE)
    add_definitions(-DMUSESCORE_UNSTABLE)
endif()

# modules config
add_definitions(-DMU_LANGUAGES_SERVER_URL="http://extensions.musescore.org/4.2/languages/")

if (MUSESCORE_ALLOW_UPDATE_ON_PRERELEASE)
    add_definitions(-DMUSESCORE_ALLOW_UPDATE_ON_PRERELEASE)
endif()

function(def_opt name val)
    if (${val})
        add_definitions(-D${name})
    endif()
endfunction()

# framework
def_opt(MU_BUILD_ACCESSIBILITY_MODULE ${MU_BUILD_ACCESSIBILITY_MODULE})
def_opt(MU_BUILD_ACTIONS_MODULE ${MU_BUILD_ACTIONS_MODULE})
def_opt(MU_BUILD_EXTENSIONS_MODULE ${MU_BUILD_EXTENSIONS_MODULE})
def_opt(MU_BUILD_LANGUAGES_MODULE ${MU_BUILD_LANGUAGES_MODULE})
def_opt(MU_BUILD_MIDI_MODULE ${MU_BUILD_MIDI_MODULE})
def_opt(MU_BUILD_MULTIINSTANCES_MODULE ${MU_BUILD_MULTIINSTANCES_MODULE})
def_opt(MU_BUILD_NETWORK_MODULE ${MU_BUILD_NETWORK_MODULE})
def_opt(MU_BUILD_SHORTCUTS_MODULE ${MU_BUILD_SHORTCUTS_MODULE})
def_opt(MU_BUILD_UI_MODULE ${MU_BUILD_UI_MODULE})
def_opt(MU_BUILD_WORKSPACE_MODULE ${MU_BUILD_WORKSPACE_MODULE})


#def_opt(MU_BUILD_VST_MODULE ${MU_BUILD_VST_MODULE})
# modules
def_opt(MU_BUILD_APPSHELL_MODULE ${MU_BUILD_APPSHELL_MODULE})
#def_opt(MU_BUILD_CLOUD_MODULE ${MU_BUILD_CLOUD_MODULE})
#def_opt(MU_BUILD_CONVERTER_MODULE ${MU_BUILD_CONVERTER_MODULE})
#def_opt(MU_BUILD_DIAGNOSTICS_MODULE ${MU_BUILD_DIAGNOSTICS_MODULE})
#def_opt(MU_BUILD_LEARN_MODULE ${MU_BUILD_LEARN_MODULE})
def_opt(MU_BUILD_PROJECTSCENE_MODULE ${MU_BUILD_PROJECTSCENE_MODULE})
def_opt(MU_BUILD_UPDATE_MODULE ${MU_BUILD_UPDATE_MODULE})
#def_opt(MU_BUILD_IMPORTEXPORT_MODULE ${MU_BUILD_IMPORTEXPORT_MODULE})
#def_opt(MU_BUILD_CRASHPAD_CLIENT ${MU_BUILD_CRASHPAD_CLIENT})

if (QT_SUPPORT)
    add_definitions(-DQT_SUPPORT)
    add_definitions(-DKORS_LOGGER_QT_SUPPORT)
else()
    add_definitions(-DNO_QT_SUPPORT)
endif()

if (WIN_PORTABLE)
    add_definitions(-DWIN_PORTABLE)
endif()

add_definitions(-DKORS_PROFILER_ENABLED)
