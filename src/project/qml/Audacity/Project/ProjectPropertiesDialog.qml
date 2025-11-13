/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import Audacity.Project 1.0

import "internal/Properties"

StyledDialogView {
    id: root

    title: qsTrc("project/properties", "Project properties")

    contentWidth: 680
    contentHeight: 500
    margins: ui.theme.extra.space_16

    readonly property int propertyNameWidth: 160
    readonly property int propertyRowHorizontalSpacing: ui.theme.extra.space_8
    readonly property int propertyRowRightMargin: propertiesListView.propertyRowRightMargin

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ProjectPropertiesPanel"
        section: root.navigationSection
        direction: NavigationPanel.Horizontal
        order: 1
        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    ProjectPropertiesModel {
        id: projectPropertiesModel
    }

    Component.onCompleted: {
        projectPropertiesModel.load()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: ui.theme.extra.space_8

        ProjectPropertiesView {
            id: propertiesListView

            propertiesModel: projectPropertiesModel

            Layout.fillHeight: true
            Layout.fillWidth: true

            propertyNameWidth: root.propertyNameWidth
            propertyRowHorizontalSpacing: root.propertyRowHorizontalSpacing

            navigationPanel: root.navigationPanel
            navigationColumnStart: propertiesFileInfoPanel.navigationColumnEnd + 1
        }

        SeparatorLine {}

        ProjectPropertiesFileInfoPanel {
            id: propertiesFileInfoPanel

            propertiesModel: projectPropertiesModel

            Layout.fillWidth: true
            Layout.topMargin: ui.theme.extra.space_4
            Layout.rightMargin: root.propertyRowRightMargin
            Layout.bottomMargin: ui.theme.extra.space_8

            propertyNameWidth: root.propertyNameWidth
            propertyRowHorizontalSpacing: root.propertyRowHorizontalSpacing
            propertyRowRightMargin: root.propertyRowRightMargin

            navigationPanel: root.navigationPanel
        }

        ButtonBox {
            Layout.fillWidth: true

            buttons: [ButtonBoxModel.Ok, ButtonBoxModel.Cancel]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

            FlatButton {
                text: qsTrc("project", "New property")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true

                onClicked: {
                    projectPropertiesModel.newProperty()
                }
            }

            onStandardButtonClicked: function (buttonId) {
                if (buttonId === ButtonBoxModel.Ok) {
                    projectPropertiesModel.saveProperties()
                    root.hide()
                } else if (buttonId === ButtonBoxModel.Cancel) {
                    root.hide()
                }
            }
        }
    }
}
