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
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import MuseScore.UiComponents 1.0
import MuseScore.Ui 1.0

ListItemBlank {
    id: root

    property var item: null
    property string keyRoleName: "key"
    property string valueRoleName: "value"
    property string valueTypeRole: "valueType"
    property string valueEnabledRoleName: "enabled"
    property string minValueRoleName: "min"
    property string maxValueRoleName: "max"
    property string iconRoleName: "icon"

    property bool readOnly: false

    property alias spacing: row.spacing
    property real sideMargin: 0
    property real valueItemWidth: 126

    height: 34

    normalColor: (index % 2 == 0) ? ui.theme.backgroundSecondaryColor : ui.theme.backgroundPrimaryColor

    navigation.accessible.name: titleLabel.text + ": " + (Boolean(loader.item) ? loader.item.accessibleName : "")

    QtObject {
        id: privateProperties

        function componentByType(type) {
            switch (type) {
            case "Undefined": return textComp
            case "Bool": return boolComp
            case "Int": return intComp
            case "Double": return doubleComp
            case "String": return textComp
            case "Color": return colorComp
            }

            return textComp
        }

        function isNumberComponent() {
            return root.item[valueTypeRole] === "Int" || root.item[valueTypeRole] === "Double"
        }
    }

    onClicked: {
        forceActiveFocus()
    }

    RowLayout {
        id: row

        anchors.fill: parent

        Row {
            Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
            Layout.fillWidth: true
            Layout.leftMargin: root.sideMargin

            spacing: 18

            StyledIconLabel {
                iconCode: Boolean(root.item[iconRoleName]) ? root.item[iconRoleName] : IconCode.NONE
            }

            StyledTextLabel {
                id: titleLabel
                text: root.item[keyRoleName]
                horizontalAlignment: Text.AlignLeft
            }
        }

        Loader {
            id: loader
            property var val: root.item[valueRoleName]

            Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
            Layout.preferredWidth: root.valueItemWidth
            Layout.rightMargin: root.sideMargin

            enabled: root.item[valueEnabledRoleName] !== undefined ? root.item[valueEnabledRoleName] : true

            sourceComponent: !root.readOnly ? privateProperties.componentByType(root.item[valueTypeRole]) : readOnlyComponent

            onLoaded: {
                loader.item.val = loader.val

                if (privateProperties.isNumberComponent() && !root.readOnly) {
                    if (Boolean(root.item[minValueRoleName])) {
                        loader.item.minValue = root.item[minValueRoleName]
                    }

                    if (Boolean(root.item[maxValueRoleName])) {
                        loader.item.maxValue = root.item[maxValueRoleName]
                    }
                }
            }

            onValChanged: {
                if (loader.item) {
                    loader.item.val = loader.val
                }
            }

            Connections {
                target: loader.item
                function onChanged(newVal) {
                    root.item[valueRoleName] = newVal
                }
            }
        }
    }

    Component {
        id: textComp

        TextInputField {
            id: textControl

            property string val
            signal changed(string newVal)

            property string accessibleName: navigation.accessible.name

            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: 1

            currentText: val

            onTextChanged: function(newTextValue) {
                textControl.changed(newTextValue)
            }
        }
    }

    Component {
        id: colorComp

        ColorPicker {
            id: colorControl

            property color val
            signal changed(color newVal)

            property string accessibleName: navigation.accessible.name

            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: 1

            color: val

            onNewColorSelected: function(newColor) {
                colorControl.changed(newColor)
            }
        }
    }

    Component {
        id: intComp

        IncrementalPropertyControl {
            id: intControl

            property int val
            signal changed(int newVal)

            property string accessibleName: navigation.accessible.name

            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: 1

            currentValue: val

            step: 1
            decimals: 0

            onValueEdited: function(newValue) {
                intControl.changed(newValue)
            }
        }
    }

    Component {
        id: doubleComp

        IncrementalPropertyControl {
            id: doubleControl

            property double val
            signal changed(double newVal)

            property string accessibleName: navigation.accessible.name

            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: 1

            currentValue: val
            step: 1.0

            onValueEdited: function(newValue) {
                doubleControl.changed(newValue)
            }
        }
    }

    Component {
        id: boolComp

        CheckBox {
            id: boolControl

            property bool val
            signal changed(bool newVal)

            property string accessibleName: checked ? qsTrc("ui", "checked", "checkstate") : qsTrc("ui", "unchecked", "checkstate")

            navigation.panel: root.navigation.panel
            navigation.row: root.navigation.row
            navigation.column: 1

            checked: val ? true : false
            onClicked: {
                boolControl.changed(!boolControl.checked)
            }
        }
    }

    Component {
        id: readOnlyComponent

        StyledTextLabel {
            property var val
            signal changed(var stub)

            property string accessibleName: text

            text: val
            horizontalAlignment: Text.AlignLeft
        }
    }
}
