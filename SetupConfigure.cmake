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

if (NOT AU4_BUILD_CONFIGURATION)
    set(AU4_BUILD_CONFIGURATION "app")
endif()

if (NOT AU4_BUILD_MODE)
    set(AU4_BUILD_MODE "dev")
endif()

# Set revision for local builds
include(TryUseLocalRevision)

message(STATUS "AU4_BUILD_CONFIGURATION: ${AU4_BUILD_CONFIGURATION}")
message(STATUS "AU4_BUILD_MODE: ${AU4_BUILD_MODE}")
message(STATUS "AU4_BUILD_NUMBER: ${CMAKE_BUILD_NUMBER}")

string(TOUPPER ${AU4_BUILD_CONFIGURATION} BUILD_CONFIGURE)
string(TOUPPER ${AU4_BUILD_MODE} BUILD_MODE)

###########################################
# Setup by mode
###########################################
if(BUILD_MODE MATCHES "DEV")
    set(MUSE_APP_UNSTABLE ON)
    set(MUSE_APP_RELEASE_CHANNEL "dev")
    set(MUSE_APP_NAME_VERSION "${MUSE_APP_NAME_VERSION} ${MUSE_APP_RELEASE_CHANNEL}")
    set(MUSE_APP_IS_PRERELEASE ON)
    set(AU4_ALLOW_UPDATE_ON_PRERELEASE OFF)
endif()

if(BUILD_MODE MATCHES "TESTING")
    set(MUSE_APP_UNSTABLE OFF)
    set(MUSE_APP_RELEASE_CHANNEL "Testing")
    set(MUSE_APP_NAME_VERSION "${MUSE_APP_NAME_VERSION} ${MUSE_APP_RELEASE_CHANNEL}")
    set(MUSE_APP_IS_PRERELEASE ON)
    set(AU4_ALLOW_UPDATE_ON_PRERELEASE ON)
endif()

if(BUILD_MODE MATCHES "RELEASE")
    set(MUSE_APP_UNSTABLE OFF)
    set(MUSE_APP_IS_PRERELEASE OFF)
    set(AU4_ALLOW_UPDATE_ON_PRERELEASE OFF)
endif()

###########################################
# Setup paths
###########################################
if (OS_IS_MAC)
    SET(AU4_INSTALL_NAME  "Contents/Resources/")
    SET(AU4_SHARE_NAME    "audacity.app/")
    SET(AU4_FRAMEWORKS_NAME "Frameworks/")
elseif (OS_IS_WIN)
    SET(AU4_INSTALL_NAME  "")
    SET(AU4_SHARE_NAME    "./")
else()
    SET(AU4_INSTALL_NAME  "audacity${MUSE_APP_INSTALL_SUFFIX}-${AU4_VERSION_MAJ_MIN}/")
    SET(AU4_SHARE_NAME    "share/")
endif()

###########################################
# CONFIGURE: Desktop App
###########################################
set(AU4_GENERAL_APP OFF)
if(BUILD_CONFIGURE MATCHES "APP")
    set(AU4_GENERAL_APP ON)
endif()

if(BUILD_CONFIGURE MATCHES "APP-PORTABLE")
    set(AU4_GENERAL_APP ON)
    set(WIN_PORTABLE ON)
endif()

if (AU4_GENERAL_APP)
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
# CONFIGURE: UTest
###########################################
if(BUILD_CONFIGURE MATCHES "UTEST")
    set(MUSE_ENABLE_UNIT_TESTS ON)
    set(MUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL ON)
    set(MUSE_MODULE_AUDIO OFF)
    set(MUSE_COMPILE_ASAN ON)

    message(STATUS "If you added tests to a module that didn't have them yet, make sure that this module is enabled, see SetupConfigure.cmake")
    set(MUSE_MODULE_MIDI OFF)
    set(MUSE_MODULE_MUSESAMPLER OFF)
    set(MUSE_MODULE_NETWORK OFF)
    set(MUSE_MODULE_SHORTCUTS OFF)

    set(MUE_BUILD_APPSHELL_MODULE OFF)

endif()

###########################################
# Subsystem
###########################################

set(QT_SUPPORT ON)

if (NOT MUSE_MODULE_UI)
    set(MUE_BUILD_APPSHELL_MODULE OFF) # hard dependency
endif()

###########################################
# Unit tests
###########################################
if (NOT MUSE_ENABLE_UNIT_TESTS)

    set(MUE_BUILD_PROJECT_TESTS OFF)

endif()

###########################################
# Configure framework
###########################################
set(MUSE_APP_BUILD_NUMBER ${CMAKE_BUILD_NUMBER})
set(MUSE_APP_INSTALL_PREFIX "\"${CMAKE_INSTALL_PREFIX}\"")
set(MUSE_APP_INSTALL_NAME "\"${AU4_INSTALL_NAME}\"")

include(${MUSE_FRAMEWORK_SRC_PATH}/cmake/MuseSetupConfiguration.cmake)

###########################################
# Global definitions
###########################################


# modules config

if (AU4_ALLOW_UPDATE_ON_PRERELEASE)
    add_definitions(-DAU4_ALLOW_UPDATE_ON_PRERELEASE)
endif()

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

if (MUE_ENABLE_LOAD_QML_FROM_SOURCE)
    add_definitions(-DMUE_ENABLE_LOAD_QML_FROM_SOURCE)
endif()
