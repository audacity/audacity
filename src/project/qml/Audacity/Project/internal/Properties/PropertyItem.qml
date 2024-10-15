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

RowLayout {
    id: root

    property int propertyNameWidth: -1
    property NavigationPanel navigationPanel: null
    property int navigationColumnEnd: deletePropertyButton.navigation.column

    property int index: 0
    property string propertyName: ""
    property string propertyValue: ""
    property bool isStandardProperty: true
    property bool isFileInfoPanelProperty: false
    property bool valueFillWidth: false

    signal scrollIntoViewRequested()
    signal deletePropertyRequested()

    spacing: 8

    QtObject {
        id: prv

        property int navigationStartIndex: root.index * 3
    }

    StyledTextLabel {
        Layout.preferredWidth: root.propertyNameWidth

        text: root.propertyName ? root.propertyName : ""
        font: ui.theme.bodyBoldFont
        horizontalAlignment: Qt.AlignLeft
        visible: root.isStandardProperty ? root.isStandardProperty : false
    }

    TextInputField {
        Layout.preferredWidth: root.propertyNameWidth

        currentText: root.propertyName ? root.propertyName : ""
        visible: !root.isStandardProperty
        hint: qsTrc("project/properties", "Property")

        navigation.name: root.propertyName + "PropertyName"
        navigation.panel: root.navigationPanel
        navigation.column: prv.navigationStartIndex
        navigation.onActiveChanged: {
            if (navigation.active) {
                root.scrollIntoViewRequested()
            }
        }

        onTextChanged: function(newValue) {
            root.propertyName = newValue
        }
    }

    TextInputField {
        Layout.fillWidth: true

        currentText: root.propertyValue ? root.propertyValue : ""
        hint: root.isStandardProperty ? "" : qsTrc("project/properties", "Value")
        visible: !root.isFileInfoPanelProperty

        navigation.name: root.propertyName + "PropertyValue"
        navigation.panel: root.navigationPanel
        navigation.column: prv.navigationStartIndex + 1
        accessible.name: root.propertyName + " " + currentText
        navigation.onActiveChanged: {
            if (navigation.active && !root.isFileInfoPanelProperty) {
                root.scrollIntoViewRequested()
            }
        }

        onTextChanged: function(newValue) {
            root.propertyValue = newValue
        }
    }

    StyledTextLabel {
        Layout.fillWidth: root.valueFillWidth

        text: root.propertyValue ? root.propertyValue : ""
        font: ui.theme.bodyBoldFont
        horizontalAlignment: Qt.AlignLeft
        visible: root.isFileInfoPanelProperty
    }

    FlatButton {
        id: deletePropertyButton

        icon: IconCode.DELETE_TANK
        opacity: !root.isStandardProperty
        enabled: !root.isStandardProperty
        visible: !root.isFileInfoPanelProperty

        navigation.name: root.propertyName + "Delete"
        navigation.panel: root.navigationPanel
        navigation.column: prv.navigationStartIndex + 2
        accessible.name: "Delete"

        onClicked: root.deletePropertyRequested()
    }
}
