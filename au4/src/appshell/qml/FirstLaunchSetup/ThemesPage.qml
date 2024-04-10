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
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

import "../shared"

Page {
    id: root

    title: qsTrc("appshell/gettingstarted", "Welcome to MuseScore 4")
    explanation: qsTrc("appshell/gettingstarted", "Let’s get started by choosing a theme.")

    titleContentSpacing: model.isFollowSystemThemeAvailable ? 24 : 28

    ThemesPageModel {
        id: model
    }

    Component.onCompleted: {
        model.load()
    }

    ColumnLayout {
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: model.isFollowSystemThemeAvailable ? 20 : 28

        CheckBox {
            visible: model.isFollowSystemThemeAvailable
            Layout.alignment: Qt.AlignCenter

            text: qsTrc("appshell/gettingstarted", "Follow system theme")

            checked: model.isFollowSystemTheme

            navigation.name: "FollowSystemThemeBox"
            navigation.order: 1
            navigation.panel: NavigationPanel {
                name: "FollowSystemThemeBox"
                enabled: parent.enabled && parent.visible
                section: root.navigationSection
                order: root.navigationStartRow + 1
                direction: NavigationPanel.Horizontal
            }

            onClicked: {
                model.isFollowSystemTheme = !checked
            }
        }

        ThemeSamplesList {
            id: themeSamplesList
            Layout.alignment: Qt.AlignCenter

            themes: model.highContrastEnabled ? model.highContrastThemes : model.generalThemes
            currentThemeCode: model.currentThemeCode

            spacing: 48

            navigationPanel.section: root.navigationSection
            navigationPanel.order: root.navigationStartRow + 2

            onThemeChangeRequested: function(newThemeCode) {
                model.currentThemeCode = newThemeCode
            }
        }

        AccentColorsList {
            id: accentColorsList
            Layout.alignment: Qt.AlignCenter
            Layout.preferredHeight: Math.max(implicitHeight, highContrastPreferencesHintLabel.implicitHeight)
            visible: !model.highContrastEnabled

            colors: model.accentColors
            currentColorIndex: model.currentAccentColorIndex

            sampleSize: 20
            spacing: 4

            navigationPanel.section: root.navigationSection
            navigationPanel.order: root.navigationStartRow + 3

            onAccentColorChangeRequested: function(newColorIndex) {
                model.currentAccentColorIndex = newColorIndex
            }
        }

        StyledTextLabel {
            id: highContrastPreferencesHintLabel
            visible: model.highContrastEnabled
            Layout.fillWidth: true
            Layout.preferredHeight: Math.max(implicitHeight, accentColorsList.implicitHeight)
            text: qsTrc("appshell/gettingstarted", "Further high contrast settings are available in Preferences.")
        }

        CheckBox {
            Layout.alignment: Qt.AlignCenter

            text: qsTrc("appshell/gettingstarted", "Enable high contrast")
            checked: model.highContrastEnabled

            navigation.name: "EnableHighContrastCheckbox"
            navigation.order: 1
            navigation.panel: NavigationPanel {
                name: "EnableHighContrast"
                enabled: parent.enabled && parent.visible
                section: root.navigationSection
                order: root.navigationStartRow + 4
                direction: NavigationPanel.Horizontal
            }
            navigation.accessible.description: highContrastPreferencesHintLabel.text

            onClicked: {
                model.highContrastEnabled = !checked
            }
        }
    }
}
