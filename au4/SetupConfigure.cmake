#=============================================================================
#  MuseScore
#  Linux Music Score Editor
#
#  Copyright (C) 2023 MuseScore BVBA and others
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 2.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#=============================================================================

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
# Before need run 'make revision' or 'msvc_build.bat revision'
include(TryUseLocalRevision)

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
set(MUE_GENERAL_APP OFF)
if(BUILD_CONFIGURE MATCHES "APP")
    set(MUE_GENERAL_APP ON)
endif()

if(BUILD_CONFIGURE MATCHES "APP-PORTABLE")
    set(MUE_GENERAL_APP ON)
    set(WIN_PORTABLE ON)
endif()

if (MUE_GENERAL_APP)
    if (BUILD_IS_DEBUG)
        set(MUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL ON)
    else()
        set(MUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL OFF)
    endif()
endif()

if (WIN_PORTABLE)
    set(MUSE_MODULE_UPDATE OFF)
endif()
if (OS_IS_FBSD)
    message(WARNING "Not building unsupported chrashpad client on FreeBSD")
    set(MUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT OFF)
endif()

###########################################
# CONFIGURE: VTest
###########################################
if(BUILD_CONFIGURE MATCHES "VTEST")
    set(MUSE_BUILD_UNIT_TESTS OFF)
    set(MUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL ON)
    set(MUE_BUILD_ASAN ON)

    set(MUE_BUILD_IMAGESEXPORT_MODULE ON)
    set(MUE_BUILD_CONVERTER_MODULE ON)
    set(MUE_BUILD_PROJECT_MODULE ON)
    set(MUE_BUILD_NOTATION_MODULE ON)
    set(MUSE_MODULE_UI ON)

    set(MUSE_MODULE_ACCESSIBILITY OFF)
    set(MUSE_MODULE_AUDIO OFF)
    set(MUE_BUILD_BRAILLE_MODULE OFF)
    set(MUSE_MODULE_MIDI OFF)
    set(MUSE_MODULE_MPE OFF)
    set(MUSE_MODULE_MUSESAMPLER OFF)
    set(MUSE_MODULE_NETWORK OFF)
    set(MUSE_MODULE_SHORTCUTS OFF)
    set(MUSE_MODULE_VST OFF)
    set(MUE_BUILD_APPSHELL_MODULE OFF)
    set(MUSE_MODULE_AUTOBOT OFF)
    set(MUSE_MODULE_CLOUD OFF)
    set(MUE_BUILD_INSPECTOR_MODULE OFF)
    set(MUE_BUILD_INSTRUMENTSSCENE_MODULE OFF)
    set(MUSE_MODULE_LANGUAGES OFF)
    set(MUSE_MODULE_LEARN OFF)
    set(MUSE_MODULE_MULTIINSTANCES OFF)
    set(MUE_BUILD_PALETTE_MODULE OFF)
    set(MUE_BUILD_PLAYBACK_MODULE OFF)
    set(MUSE_MODULE_EXTENSIONS OFF)
    set(MUSE_MODULE_UPDATE OFF)
    set(MUSE_MODULE_WORKSPACE OFF)

    set(MUE_BUILD_IMPORTEXPORT_MODULE OFF)
    set(MUE_BUILD_VIDEOEXPORT_MODULE OFF)

    set(MUE_INSTALL_SOUNDFONT OFF)

    set(MUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT OFF)

endif()

###########################################
# CONFIGURE: UTest
###########################################
if(BUILD_CONFIGURE MATCHES "UTEST")
    set(MUSE_BUILD_UNIT_TESTS ON)
    set(MUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL ON)
    set(MUSE_MODULE_AUDIO ON)
    set(MUE_BUILD_ASAN ON)

    message(STATUS "If you added tests to a module that didn't have them yet, make sure that this module is enabled, see SetupConfigure.cmake")
    set(MUSE_MODULE_MIDI OFF)
    set(MUSE_MODULE_MUSESAMPLER OFF)
    set(MUSE_MODULE_NETWORK OFF)
    set(MUSE_MODULE_SHORTCUTS OFF)

    set(MUE_BUILD_APPSHELL_MODULE OFF)
    set(MUSE_MODULE_AUTOBOT OFF)
    set(MUSE_MODULE_CLOUD OFF)
    set(MUE_BUILD_CONVERTER_MODULE OFF)
    set(MUE_BUILD_INSPECTOR_MODULE OFF)
    set(MUE_BUILD_INSTRUMENTSSCENE_MODULE OFF)
    set(MUSE_MODULE_LANGUAGES OFF)
    set(MUSE_MODULE_LEARN OFF)
    set(MUSE_MODULE_MULTIINSTANCES OFF)
    set(MUE_BUILD_PALETTE_MODULE OFF)
    set(MUE_BUILD_PLAYBACK_MODULE OFF)
    set(MUSE_MODULE_UPDATE OFF)
    set(MUSE_MODULE_WORKSPACE OFF)
endif()

###########################################
# Subsystem
###########################################

set(QT_SUPPORT ON)

if (MUSE_MODULE_AUDIO_JACK)
    if (OS_IS_LIN OR MINGW)
        add_definitions(-DJACK_AUDIO)
    else()
        set(MUSE_MODULE_AUDIO_JACK OFF)
    endif()
endif()

if (NOT MUE_BUILD_IMPORTEXPORT_MODULE)
    set(MUE_BUILD_VIDEOEXPORT_MODULE OFF)
endif()

if (MUE_BUILD_ASAN)
    set(MUE_ENABLE_CUSTOM_ALLOCATOR OFF)
endif()

if (NOT MUE_BUILD_NOTATION_MODULE)
    set(MUE_BUILD_PROJECT_MODULE OFF) # hard dependency
    set(MUE_BUILD_PALETTE_MODULE OFF) # hard dependency
endif()

if (NOT MUSE_MODULE_UI)
    set(MUE_BUILD_APPSHELL_MODULE OFF) # hard dependency
endif()

###########################################
# Unit tests
###########################################
if (NOT MUSE_BUILD_UNIT_TESTS)

    set(MUE_BUILD_BRAILLE_TESTS OFF)
    set(MUE_BUILD_ENGRAVING_TESTS OFF)
    set(MUE_BUILD_IMPORTEXPORT_TESTS OFF)
    set(MUE_BUILD_NOTATION_TESTS OFF)
    set(MUE_BUILD_PLAYBACK_TESTS OFF)
    set(MUE_BUILD_PROJECT_TESTS OFF)

endif()

###########################################
# Configure framework
###########################################
set(MUSE_APP_NAME ${MUSESCORE_NAME})
set(MUSE_APP_UNSTABLE ${MUSESCORE_UNSTABLE})
set(MUSE_APP_REVISION ${MUSESCORE_REVISION})
set(MUSE_APP_BUILD_NUMBER ${CMAKE_BUILD_NUMBER})
set(MUSE_APP_VERSION ${MUSESCORE_VERSION})
set(MUSE_APP_VERSION_LABEL "\"${MUSESCORE_VERSION_LABEL}\"")
set(MUSE_APP_INSTALL_SUFFIX "\"${MUSESCORE_INSTALL_SUFFIX}\"")
set(MUSE_APP_INSTALL_PREFIX "\"${CMAKE_INSTALL_PREFIX}\"")
set(MUSE_APP_INSTALL_NAME "\"${Mscore_INSTALL_NAME}\"")

include(${MUSE_FRAMEWORK_SRC_PATH}/cmake/MuseSetupConfiguration.cmake)

###########################################
# Global definitions
###########################################


# modules config

if (MUSESCORE_ALLOW_UPDATE_ON_PRERELEASE)
    add_definitions(-DMUSESCORE_ALLOW_UPDATE_ON_PRERELEASE)
endif()

if (QT_SUPPORT)
    add_definitions(-DQT_SUPPORT)
    add_definitions(-DKORS_LOGGER_QT_SUPPORT)
    add_definitions(-DSCRIPT_INTERFACE)

    if (MUE_COMPILE_QT5_COMPAT)
        add_definitions(-DMU_QT5_COMPAT)
    endif()

else()
    add_definitions(-DNO_QT_SUPPORT)
endif()

if (WIN_PORTABLE)
    add_definitions(-DWIN_PORTABLE)
endif()

add_definitions(-DKORS_PROFILER_ENABLED)

if (MUE_ENABLE_LOAD_QML_FROM_SOURCE)
    add_definitions(-DMUE_ENABLE_LOAD_QML_FROM_SOURCE)
endif()
