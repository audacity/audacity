/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

RowLayout {
    id: root

    property var model: null

    property bool showMenu: true
    property int backgroundLeftRadius: 3
    property Border border: Border {}
    property int arrowSpacing: 1
    property color textColor: ui.theme.fontSecondaryColor
    property color backgroundColor: ui.theme.backgroundQuarternaryColor

    property NavigationControl navigation: NavigationControl {
        property bool triggerLocked: false

        name: "NumericViewItem"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Information
        accessible.name: accessibleName + (model ? model.valueString : "")

        onTriggered: {
            if (triggerLocked) {
                return
            }

            prv.isFieldsNavigationEnabled = true

            var item = repeater.itemAt(0)
            if (item) {
                root.model.currentEditedFieldIndex = 0
            }
        }
    }
    property alias navigationColumnEnd: menuBtn.navigation.column

    property string accessibleName: ""

    signal valueChangeRequested(var newValue)
    signal valueEditingFinished()

    height: 28

    spacing: root.arrowSpacing

    Component.onCompleted: {
        root.model.visualItem = root
    }

    Connections {
        target: root.model
        function onValueChanged() {
            root.valueChangeRequested(value)
        }

        function onEditingFinished() {
            root.navigation.triggerLocked = true
            root.navigation.requestActive()
            root.navigation.triggerLocked = false

            prv.isFieldsNavigationEnabled = false

            root.valueEditingFinished()
        }
    }

    QtObject {
        id: prv

        property bool isFieldsNavigationEnabled: false
    }

    RoundedRectangle {
        Layout.preferredWidth: childrenRect.width
        Layout.fillHeight: true

        topLeftRadius: root.backgroundLeftRadius
        bottomLeftRadius: root.backgroundLeftRadius

        border: root.border
        color: root.backgroundColor

        Item {
            property int margin: 6

            width: row.width + margin * 2
            height: parent.height

            Row {
                id: row

                anchors.left: parent.left
                anchors.leftMargin: parent.margin
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                spacing: 0

                Repeater {
                    id: repeater

                    model: root.model

                    delegate: NumericField {
                        height: row.height
                        value: symbol

                        isSelected: model.index === root.model.currentEditedFieldIndex

                        isEditable: editable

                        color: root.textColor
                        enabled: root.enabled

                        navigation.panel: root.navigation.panel
                        navigation.enabled: prv.isFieldsNavigationEnabled
                        navigation.row: root.navigation.row
                        navigation.column: root.navigation.column + 1 + model.index

                        navigation.onNavigationEvent: function(event) {
                            if (event.type === NavigationEvent.Escape) {
                                prv.isFieldsNavigationEnabled = false

                                root.navigation.navigationEvent(event)
                                event.accepted = true
                            }
                        }

                        onIsSelectedChanged: {
                            if (isSelected && !navigation.active) {
                                navigation.requestActive()
                            }
                        }

                        onClicked: {
                            root.model.currentEditedFieldIndex = model.index
                        }
                    }
                }
            }

            NavigationFocusBorder { navigationCtrl: root.navigation }
        }
    }

    ArrowMenuButton {
        id: menuBtn

        Layout.preferredWidth: 16
        Layout.fillHeight: true

        menuModel: root.model.availableFormats

        border: root.border
        backgroundColor: root.backgroundColor
        iconColor: root.textColor
        visible: root.showMenu

        navigation.panel: root.navigation.panel
        navigation.row: root.navigation.row
        navigation.column: root.navigation.column + 1 + repeater.count + 1

        onHandleMenuItem: function(itemId) {
            root.model.currentFormat = parseInt(itemId)
        }
    }
}
