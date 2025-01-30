import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

ListItemBlank {
    id: root

    property var modelRef

    property var item: null
    property var availableEffects: null
    property var handleMenuItemWithState: null

    height: 24
    clip: false // should be true?
    background.color: "transparent"
    hoverHitColor: "transparent"

    DropArea {
        id: dragTarget
        anchors.fill: parent
        property alias item: root.item

        RowLayout {
            id: content

            width: parent.width
            height: parent.height
            anchors.verticalCenter: parent.verticalCenter
            anchors.horizontalCenter: parent.horizontalCenter

            Drag.active: gripButton.mouseArea.drag.active
            Drag.hotSpot.x: width / 2
            Drag.hotSpot.y: height / 2

            spacing: 4

            states: [
                State {
                    name: "DRAGGED"
                    when: gripButton.mouseArea.drag.active
                    AnchorChanges { target: content; anchors.verticalCenter: undefined; anchors.horizontalCenter: undefined }
                    PropertyChanges { target: root; z: modelRef.count() }
                },
                State {
                    name: "CONTAINS_DRAG"
                    when: dragTarget.containsDrag
                    PropertyChanges { target: root.background; color: "blue" }
                }
            ]

            FlatButton {
                id: gripButton

                Layout.preferredWidth: 14
                Layout.preferredHeight: 16
                Layout.alignment: Qt.AlignVCenter
                backgroundRadius: 0

                visible: true
                transparent: true
                hoverHitColor: ui.theme.buttonColor
                mouseArea.cursorShape: Qt.SizeAllCursor

                // do not accept buttons as FlatButton's mouseArea will override
                // DockTitleBar mouseArea and effect will not be draggable
                mouseArea.drag.target: content
                mouseArea.drag.axis: Drag.YAxis
                mouseArea.onReleased: {
                    if (!content.Drag.target) {
                        return
                    }
                    modelRef.moveRow(root.item.getIndex(), content.Drag.target.item.getIndex())
                }

                mouseArea.onPositionChanged: {
                    if (!mouseArea.drag.active) {
                        return;
                    }
                    var posInListView = content.mapToItem(trackEffectList, 0, 0);
                    // console.log("Y position relative to listView: " + posInListView.y);
                }

                contentItem: StyledIconLabel {
                    iconCode: IconCode.TOOLBAR_GRIP
                }

            }

            BypassEffectButton {
                Layout.margins: 0
                Layout.alignment: Qt.AlignVCenter
                Layout.preferredWidth: root.height
                Layout.minimumHeight: root.height
                Layout.maximumHeight: root.height

                isMasterEffect: item && item.isMasterEffect
                accentButton: item && item.isActive

                onClicked: {
                    item.isActive = item && !item.isActive
                }
            }

            // Wrappinng a FlatButton in a Rectangle because of two bad reasons:
            // * I don't find a `border` property for `FlatButton`
            // * Somehow, the button's color is a bit darkened it direct child of the RowLayout
            Rectangle {
                id: effectNameRect

                border.color: ui.theme.strokeColor
                border.width: 1
                radius: effectNameButton.backgroundRadius
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.preferredWidth: 148

                FlatButton {
                    id: effectNameButton

                    anchors.fill: parent
                    normalColor: ui.theme.backgroundPrimaryColor

                    StyledTextLabel {
                        anchors.fill: parent
                        anchors.leftMargin: 6
                        anchors.rightMargin: 6
                        id: trackNameLabel
                        horizontalAlignment: Text.AlignLeft
                        verticalAlignment: Text.AlignVCenter
                        text: root.item ? root.item.effectName() : ""
                    }

                    onClicked: {
                        root.item.toggleDialog()
                    }
                }
            }

            // Wrapping a FlatButton for the same reasons as above.
            Rectangle {
                id: chooseEffectRect

                border.color: ui.theme.strokeColor
                border.width: 1
                radius: effectNameButton.backgroundRadius
                Layout.fillHeight: true
                Layout.preferredWidth: height

                FlatButton {
                    id: chooseEffectDropdown

                    anchors.fill: parent
                    normalColor: ui.theme.backgroundPrimaryColor
                    icon: IconCode.SMALL_ARROW_DOWN

                    onClicked: {
                        effectMenuLoader.toggleOpened(root.availableEffects)
                    }

                    StyledMenuLoader {
                        id: effectMenuLoader

                        onHandleMenuItem: function(menuItem) {
                            root.handleMenuItemWithState(menuItem, root.item)
                        }
                    }
                }
            }
        }
    }
}
