# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore BVBA and others
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

declare_module(appshell)

set(MODULE_QRC appshell.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/appshellmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/appshellmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iappshellconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iapplicationactioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/istartupscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/appshelltypes.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/applicationuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/applicationuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/appshellconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/appshellconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/applicationactioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/applicationactioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/framelesswindowcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/framelesswindowcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/startupscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/startupscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/isessionsmanager.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/sessionsmanager.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/sessionsmanager.h
    ${CMAKE_CURRENT_LIST_DIR}/view/devtools/settingslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/devtools/settingslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/appmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/appmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/mainwindowtitleprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/mainwindowtitleprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/view/projectpagemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectpagemodel.h
    # TODO AU4
    # ${CMAKE_CURRENT_LIST_DIR}/view/notationstatusbarmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/notationstatusbarmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/aboutmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/aboutmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/firstlaunchsetup/firstlaunchsetupmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/firstlaunchsetup/firstlaunchsetupmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/firstlaunchsetup/themespagemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/firstlaunchsetup/themespagemodel.h

    # TODO AU4
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/preferencesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/preferencesmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/preferencepageitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/preferencepageitem.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/folderspreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/folderspreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/canvaspreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/canvaspreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/saveandpublishpreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/saveandpublishpreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/generalpreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/generalpreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/updatepreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/updatepreferencesmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/appearancepreferencesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/appearancepreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/noteinputpreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/noteinputpreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/advancedpreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/advancedpreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/scorepreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/scorepreferencesmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/importpreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/importpreferencesmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/playbackpreferencesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/playbackpreferencesmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/commonaudioapiconfigurationmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/preferences/commonaudioapiconfigurationmodel.h
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/braillepreferencesmodel.cpp
    # ${CMAKE_CURRENT_LIST_DIR}/view/preferences/braillepreferencesmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/framelesswindow/framelesswindowmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/framelesswindow/framelesswindowmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/publish/publishtoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/publish/publishtoolbarmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/windowdroparea.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/windowdroparea.h
    ${CMAKE_CURRENT_LIST_DIR}/view/internal/iappmenumodelhook.h
    ${CMAKE_CURRENT_LIST_DIR}/view/internal/maintoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/internal/maintoolbarmodel.h

    # TODO AU4
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/splashscreen.cpp
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/splashscreen.h
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/loadingscreenview.cpp
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/loadingscreenview.h
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/newinstanceloadingscreenview.cpp
    #${CMAKE_CURRENT_LIST_DIR}/view/internal/splashscreen/newinstanceloadingscreenview.h
    )

set(MODULE_LINK
    muse::uicomponents
    muse::ui
    muse::dockwindow
    )

if (OS_IS_MAC)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/view/internal/platform/macos/macosappmenumodelhook.mm
        ${CMAKE_CURRENT_LIST_DIR}/view/internal/platform/macos/macosappmenumodelhook.h
        ${CMAKE_CURRENT_LIST_DIR}/view/internal/platform/macos/macosscrollinghook.cpp
        ${CMAKE_CURRENT_LIST_DIR}/view/internal/platform/macos/macosscrollinghook.h
    )
    set_source_files_properties(
        ${CMAKE_CURRENT_LIST_DIR}/view/internal/platform/macos/macosappmenumodelhook.mm
        PROPERTIES
        SKIP_UNITY_BUILD_INCLUSION ON
        SKIP_PRECOMPILE_HEADERS ON
        )
endif(OS_IS_MAC)

if (OS_IS_WIN)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/win/winframelesswindowcontroller.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/win/winframelesswindowcontroller.h
    )

    set(MODULE_LINK ${MODULE_LINK} dwmapi)
endif(OS_IS_WIN)

if (NOT OS_IS_MAC)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/view/navigableappmenumodel.cpp
        ${CMAKE_CURRENT_LIST_DIR}/view/navigableappmenumodel.h
    )

    # we use QKeyMapper to fix https://github.com/musescore/MuseScore/issues/10181
    if (MUE_COMPILE_QT5_COMPAT)
        set(MODULE_INCLUDE ${Qt5Gui_PRIVATE_INCLUDE_DIRS} )
    else()
        set(MODULE_INCLUDE ${Qt6Gui_PRIVATE_INCLUDE_DIRS} )
    endif()
endif(NOT OS_IS_MAC)

setup_module()
