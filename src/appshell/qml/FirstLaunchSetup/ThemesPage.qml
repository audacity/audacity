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

    title: qsTrc("appshell/gettingstarted", "Select a theme")

    property NavigationPanel checkboxesPanel: NavigationPanel {
        name: "CheckboxesPanel"
        enabled: root.enabled && root.visible
        section: root.navigationSection
        order: root.navigationStartRow + 2
        direction: NavigationPanel.Vertical
        accessible.name: qsTrc("appshell/gettingstarted", "Theme options")
        accessible.description: qsTrc("appshell/gettingstarted", "Additional theme configuration options")
    }

    ThemesPageModel {
        id: model
    }

    Component.onCompleted: {
        model.load()
    }

    // Page-level accessibility information
    AccessibleItem {
        id: pageAccessibleInfo

        accessibleParent: root.navigationSection.accessible
        visualItem: root
        role: MUAccessible.Panel

        name: root.title
        description: qsTrc("appshell/gettingstarted", "Choose your preferred theme, follow system theme option, and accent color")
    }

    ColumnLayout {
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter

        spacing: 24

        ThemeSamplesList {
            id: themeSamplesList

            Layout.alignment: Qt.AlignCenter

            themes: model.highContrastEnabled ? model.highContrastThemes : model.generalThemes
            currentThemeCode: model.currentThemeCode
            spacing: 24

            navigationPanel.section: root.navigationSection
            navigationPanel.order: root.navigationStartRow + 1
            navigationPanel.accessible.name: qsTrc("appshell/gettingstarted", "Theme selection")
            navigationPanel.accessible.description: qsTrc("appshell/gettingstarted", "Choose between light, dark, or system theme")

            onThemeChangeRequested: function (newThemeCode) {
                model.currentThemeCode = newThemeCode
            }
        }

        Column {
            id: checkboxesColumn

            Layout.alignment: Qt.AlignCenter

            height: childrenRect.height
            spacing: 16

            CheckBox {
                Layout.alignment: Qt.AlignCenter

                enabled: model.isFollowSystemThemeAvailable
                text: qsTrc("appshell/gettingstarted", "Follow system theme")
                checked: model.isFollowSystemTheme

                navigation.name: "FollowSystemThemeBox"
                navigation.panel: root.checkboxesPanel
                navigation.row: 0
                navigation.column: 0
                navigation.accessible.description: qsTrc("appshell/gettingstarted", "When enabled, the theme will automatically match your system's theme setting")

                onClicked: {
                    model.isFollowSystemTheme = !checked
                }
            }

            CheckBox {
                Layout.alignment: Qt.AlignCenter

                text: qsTrc("appshell/gettingstarted", "Enable high-contrast")
                checked: model.highContrastEnabled

                navigation.name: "EnableHighContrastCheckbox"
                navigation.panel: root.checkboxesPanel
                navigation.row: 1
                navigation.column: 0
                navigation.accessible.description: qsTrc("appshell/gettingstarted", "Enable high contrast mode for better visibility. %1").arg(highContrastPreferencesHintLabel.text)

                onClicked: {
                    model.highContrastEnabled = !checked
                }
            }
        }

        Column {
            id: accentColorColumn
            visible: !model.highContrastEnabled

            height: childrenRect.height

            spacing: 6
            StyledTextLabel {
                id: accentColorTitleLabel
                Layout.alignment: Qt.AlignCenter
                anchors.horizontalCenter: parent.horizontalCenter

                text: qsTrc("project", "Accent color")
                font: ui.theme.bodyFont
            }

            AccentColorsList {
                id: accentColorsList

                Layout.alignment: Qt.AlignCenter
                Layout.preferredHeight: Math.max(implicitHeight, highContrastPreferencesHintLabel.implicitHeight)

                colors: model.accentColors
                currentColorIndex: model.currentAccentColorIndex

                sampleSize: 22
                spacing: 6

                navigationPanel.section: root.navigationSection
                navigationPanel.order: root.navigationStartRow + 3
                navigationPanel.accessible.name: qsTrc("project", "Accent color")
                navigationPanel.accessible.description: qsTrc("appshell/gettingstarted", "Choose an accent color for the interface")

                onAccentColorChangeRequested: function (newColorIndex) {
                    model.currentAccentColorIndex = newColorIndex
                }
            }
        }

        StyledTextLabel {
            id: highContrastPreferencesHintLabel
            visible: model.highContrastEnabled
            Layout.fillWidth: true
            Layout.topMargin: 15
            Layout.preferredHeight: Math.max(implicitHeight, accentColorsList.implicitHeight)
            text: qsTrc("appshell/gettingstarted", "Further options for high contrast mode can be found in Preferences")
        }

        // Accessibility group for the entire page content
        AccessibleItem {
            id: contentAccessibleGroup

            accessibleParent: pageAccessibleInfo
            visualItem: root
            role: MUAccessible.Group

            name: qsTrc("appshell/gettingstarted", "Theme configuration")
            description: {
                var themeName = model.currentThemeCode === "light" ? qsTrc("appshell/gettingstarted", "Light") : model.currentThemeCode === "dark" ? qsTrc("appshell/gettingstarted", "Dark") : qsTrc("appshell/gettingstarted", "System")
                var baseDesc = qsTrc("appshell/gettingstarted", "Current theme: %1").arg(themeName)

                if (model.isFollowSystemTheme) {
                    baseDesc = qsTrc("appshell/gettingstarted", "%1. Following system theme").arg(baseDesc)
                }

                if (model.highContrastEnabled) {
                    baseDesc = qsTrc("appshell/gettingstarted", "%1. High contrast enabled").arg(baseDesc)
                } else {
                    baseDesc = qsTrc("appshell/gettingstarted", "%1. Accent color selected").arg(baseDesc)
                }

                return baseDesc
            }
        }
    }
}
