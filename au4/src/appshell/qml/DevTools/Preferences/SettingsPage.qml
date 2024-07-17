/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Dock 1.0

import Audacity.Preferences 1.0

ColumnLayout {
    id: root

    SettingListModel {
        id: settingsModel
    }

    Component.onCompleted: {
        settingsModel.load()
    }

    SearchField {
        id: searchField
    }

    StyledListView {
        Layout.fillWidth: true
        Layout.fillHeight: true

        model: SortFilterProxyModel {
            id: sortFilterModel
            sourceModel: settingsModel

            filters: [
                FilterValue {
                    roleName: "keyRole"
                    roleValue: searchField.searchText
                    compareType: CompareType.Contains
                }
            ]

            sorters: [
                SorterValue {
                    roleName: "sectionRole"
                    sortOrder: Qt.AscendingOrder
                    enabled: true
                }
            ]
        }

        section.property: "sectionRole"
        section.delegate: Rectangle {
            width: parent.width
            height: 24
            color: ui.theme.backgroundSecondaryColor
            StyledTextLabel {
                anchors.fill: parent
                anchors.margins: 2
                horizontalAlignment: Qt.AlignLeft
                text: section
            }
        }

        delegate: Item {
            anchors.left: parent ? parent.left : undefined
            anchors.right: parent ? parent.right : undefined
            anchors.leftMargin: 8
            anchors.rightMargin: 8
            height: 32

            StyledTextLabel {
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.right: control.left
                horizontalAlignment: Qt.AlignLeft

                text: keyRole
            }

            Item {
                id: control
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.right: parent.right
                anchors.margins: 2
                width: 150

                Loader {
                    id: loader
                    property var val: valueRole
                    property var minValue: minValueRole
                    property var maxValue: maxValueRole
                    anchors.fill: parent
                    sourceComponent: root.componentByType(typeRole)
                    onLoaded: {
                        loader.item.val = loader.val

                        if (loader.item.minValue !== undefined) {
                            loader.item.minValue = loader.minValue
                        }

                        if (loader.item.maxValue !== undefined) {
                            loader.item.maxValue = loader.maxValue
                        }
                    }

                    onValChanged: {
                        if (loader.item) {
                            loader.item.val = loader.val
                        }
                    }
                }

                Connections {
                    target: loader.item
                    function onChanged(newVal) {
                        let sortFilterModelIndex = sortFilterModel.index(model.index, 0);
                        let sourceModelIndex = sortFilterModel.mapToSource(sortFilterModelIndex)
                        settingsModel.changeVal(sourceModelIndex.row, newVal)
                    }
                }
            }
        }
    }

    function componentByType(type) {
        switch (type) {
        case "Undefined": return textComp;
        case "Bool": return boolComp;
        case "Int": return intComp;
        case "Double": return doubleComp;
        case "String": return textComp;
        case "Color": return colorComp;
        }

        return textComp;
    }

    Component {
        id: textComp

        Rectangle {
            id: textControl
            property var val
            signal changed(var newVal)
            anchors.fill: parent
            border.width: 1
            border.color: ui.theme.strokeColor
            TextEdit {
                anchors.fill: parent
                anchors.margins: 2
                verticalAlignment: Text.AlignVCenter
                text: String(val)
                onEditingFinished: textControl.changed(text)
            }
        }
    }

    Component {
        id: colorComp
        ColorPicker {
            property var val
            signal changed(var newVal)

            anchors.fill: parent
            color: val

            onNewColorSelected: function(newColor) {
                changed(newColor)
            }
        }
    }

    Component {
        id: intComp

        IncrementalPropertyControl {
            id: control

            property int val

            signal changed(var newVal)
            anchors.centerIn: parent

            currentValue: val

            step: 1
            decimals: 0

            onValueEdited: function(newValue) {
                control.changed(newValue)
            }
        }
    }

    Component {
        id: doubleComp

        IncrementalPropertyControl {
            id: control

            property real val

            signal changed(var newVal)
            anchors.centerIn: parent

            currentValue: val

            step: 1
            decimals: 2

            onValueEdited: function(newValue) {
                control.changed(newValue)
            }
        }
    }

    Component {
        id: boolComp
        CheckBox {
            id: checkbox
            property var val
            signal changed(var newVal)
            anchors.fill: parent
            checked: val ? true : false
            onClicked: checkbox.changed(!checkbox.checked)
        }
    }
}
