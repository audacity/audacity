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
import QtQuick 2.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Dock 1.0

import MuseScore.Audio 1.0

import "./Gallery"
import "./Interactive"
import "./CrashHandler"
import "./CorruptScore"
import "./KeyNav"
import "./Preferences"

import "../dockwindow"

DockPage {
    id: root

    objectName: "DevTools"
    uri: "musescore://devtools"

    function setCurrentCentral(name) {
        switch (name) {
        case "settings": root.central = settingsComp; break
        case "gallery": root.central = galleryComp; break
        case "interactive": root.central = interactiveComp; break
        case "crashhandler": root.central = crashhandlerComp; break
        case "corruptscore": root.central = corruptScoreComp; break
        case "mpe": root.central = mpeComponent; break
        case "extensions": root.central = extensionsComp; break
        case "navigation": root.central = keynavComp; break
        }
    }

    panels: [
        DockPanel {
            id: devtoolsPanel

            objectName: "devtoolsPanel"

            width: maximumWidth
            minimumWidth: 200
            maximumWidth: 280

            floatable: false
            closable: false

            Rectangle {
                anchors.fill: parent
                color: ui.theme.backgroundPrimaryColor

                DevToolsMenu {
                    anchors.fill: parent

                    model: [
                        { "name": "settings", "title": "Settings" },
                        { "name": "gallery", "title": "UI Gallery" },
                        { "name": "interactive", "title": "Interactive" },
                        { "name": "crashhandler", "title": "Crash handler" },
                        { "name": "corruptscore", "title": "Corrupt score" },
                        { "name": "mpe", "title": "MPE" },
                        { "name": "extensions", "title": "Extensions" },
                        //{ "name": "navigation", "title": "KeyNav" } broken
                    ]

                    onSelected: function(name) {
                        root.setCurrentCentral(name)
                    }
                }
            }
        }
    ]

    central: extensionsComp

    Component {
        id: settingsComp

        SettingsPage {}
    }

    Component {
        id: galleryComp

        GeneralComponentsGallery {}
    }

    Component {
        id: interactiveComp

        InteractiveTests {}
    }

    Component {
        id: crashhandlerComp

        CrashHandlerDevTools {}
    }

    Component {
        id: corruptScoreComp

        CorruptScoreDevTools {}
    }

    Component {
        id: mpeComponent

        Loader {
            source: "qrc:/qml/DevTools/MPE/ArticulationsProfileEditorView.qml"
        }
    }

    Component {
        id: extensionsComp

        Loader {
            source: "qrc:/qml/DevTools/Extensions/ExtensionsListView.qml"
        }
    }

    Component {
        id: keynavComp

        KeyNavExample {}
    }
}
