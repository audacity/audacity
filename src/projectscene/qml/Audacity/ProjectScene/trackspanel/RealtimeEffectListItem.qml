import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.ProjectScene

ListItemBlank {
    id: root

    property var item: null
    property var listView: null
    property var availableEffects: null
    property int index: -1
    property int scrollOffset: 0
    property int topMargin: 0

    // Internal properties
    property int yOffset: 0
    readonly property int animationDuration: 200
    property int itemHeight: listView ? height + listView.spacing : 0

    height: 24
    y: index * itemHeight + yOffset
    clip: false // should be true?
    background.color: "transparent"
    hoverHitColor: "transparent"

    Component.onCompleted: {
        menuModel.load()
    }

    Behavior on y {
        NumberAnimation {
            id: yAnimation
            duration: animationDuration
            easing.type: Easing.InOutQuad
        }
    }

    RowLayout {
        id: content

        property alias item: root.item
        property alias index: root.index

        width: parent.width
        height: parent.height
        anchors.verticalCenter: parent.verticalCenter
        anchors.horizontalCenter: parent.horizontalCenter

        Drag.active: gripButton.mouseArea.drag.active

        spacing: 4

        states: State {
            when: gripButton.mouseArea.drag.active
            AnchorChanges { target: content; anchors.verticalCenter: undefined }
            PropertyChanges { target: root; z: listView.model.count() }
        }

        FlatButton {
            id: gripButton

            Layout.preferredWidth: 14
            Layout.preferredHeight: 16
            Layout.alignment: Qt.AlignVCenter
            backgroundRadius: 0

            visible: true
            transparent: true
            hoverHitColor: normalColor
            accentColor: normalColor
            mouseArea.cursorShape: Qt.SizeAllCursor

            mouseArea.drag.target: content
            mouseArea.drag.axis: Drag.YAxis
            mouseArea.onReleased: {
                const posInListView = content.mapToItem(listView, 0, itemHeight / 2 - topMargin).y + scrollOffset
                const targetIndex = Math.floor(posInListView / itemHeight)
                const siblings = listView.contentItem.children

                const prevContentY = listView.contentY
                // Temporarily disable the animation, otherwise the dragged item first returns to its original position
                // before sliding to its new position, which looks strange.
                yAnimation.duration = 0
                for (var i = 0; i < siblings.length; ++i) {
                    const sibling = siblings[i]
                    if (sibling.hasOwnProperty("yOffset")) {
                        sibling.yOffset = 0
                    }
                }
                listView.model.moveRow(root.index, targetIndex)
                listView.contentY = prevContentY
                yAnimation.duration = animationDuration
            }
            mouseArea.onPositionChanged: {
                if (!mouseArea.drag.active) {
                    return
                }
                const posInListView = content.mapToItem(listView, 0, itemHeight / 2 - topMargin).y + scrollOffset
                const targetIndex = Math.floor(posInListView / itemHeight)
                const siblings = listView.contentItem.children
                for (var i = 0; i < siblings.length; ++i) {
                    const sibling = siblings[i]
                    if (!sibling.hasOwnProperty("yOffset")) {
                        continue
                    }
                    else if (sibling.index > root.index && sibling.index <= targetIndex) {
                        sibling.yOffset = -itemHeight
                    } else if (sibling.index < root.index && sibling.index >= targetIndex) {
                        sibling.yOffset = itemHeight
                    } else {
                        sibling.yOffset = 0
                    }
                }
            }
            mouseArea.drag.minimumY: {
                const itemMiddle = (root.index + 0.5) * itemHeight
                // Pardon the magic number 5 ; it's probably half the top and bottom margin + 1.
                // It allows the drag to stop exactly when the middle of the item meets the top or bottom of the listView.
                return -itemMiddle - 5 + scrollOffset
            }
            mouseArea.drag.maximumY: {
                const itemMiddle = (root.index + 0.5) * itemHeight
                return listView.height - itemMiddle - 5 + scrollOffset
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

        FlatButton {
            id: effectNameButton

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 148

            backgroundItem: RealtimeEffectListItemButtonBackground {
                mouseArea: effectNameButton.mouseArea
            }

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

        FlatButton {
            id: chooseEffectDropdown

            Layout.fillHeight: true
            Layout.preferredWidth: height

            icon: IconCode.SMALL_ARROW_DOWN
            backgroundItem: RealtimeEffectListItemButtonBackground {
                mouseArea: chooseEffectDropdown.mouseArea
            }

            RealtimeEffectListItemMenuModel {
                id: menuModel
                effectState: root.item ? root.item.effectState() : null
            }

            onClicked: {
                effectMenuLoader.toggleOpened(menuModel.availableEffects)
            }

            StyledMenuLoader {
                id: effectMenuLoader

                onHandleMenuItem: function(menuItem) {
                    menuModel.handleMenuItem(menuItem)
                }
            }
        }
    }
}
