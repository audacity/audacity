/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import "../components"
import "internal"

RowLayout {
    id: root

    property alias upper: upperLabel.text
    property alias lower: lowerLabel.text

    signal upperChangeRequested(var newValue)
    signal lowerChangeRequested(var newValue)

    height: 28

    spacing: 1

    RoundedRectangle {
        Layout.preferredWidth: childrenRect.width
        Layout.fillHeight: true

        topLeftRadius: 3
        bottomLeftRadius: 3

        color: ui.theme.backgroundQuarternaryColor

        Item {
            property int margin: 10

            width: row.width + margin * 2
            height: row.height

            Row {
                id: row

                anchors.left: parent.left
                anchors.leftMargin: parent.margin
                anchors.verticalCenter: parent.verticalCenter

                height: root.height

                spacing: 6

                StyledTextLabel {
                    id: upperLabel

                    anchors.verticalCenter: parent.verticalCenter

                    font: ui.theme.tabBoldFont
                    color: ui.theme.fontSecondaryColor
                }

                StyledTextLabel {
                    anchors.verticalCenter: parent.verticalCenter

                    text: "/"
                    opacity: 0.75
                    font: ui.theme.tabBoldFont
                    color: ui.theme.fontSecondaryColor
                }

                StyledTextLabel {
                    id: lowerLabel

                    anchors.verticalCenter: parent.verticalCenter

                    font: ui.theme.tabBoldFont
                    color: ui.theme.fontSecondaryColor
                }
            }
        }
    }

    ArrowButton {
        id: popupBtn

        Layout.preferredWidth: 16
        Layout.fillHeight: true

        function togglePopupOpened() {
            if (popup.isOpened) {
                popup.close()
            } else {
                popup.open()
            }
        }

        onClicked: function(mouse) {
            togglePopupOpened()
        }

        TimeSignaturePopup {
            id: popup

            upper: root.upper
            lower: root.lower

            onUpperChangeRequested: function(newValue){
                root.upperChangeRequested(newValue)
            }

            onLowerChangeRequested: function(newValue){
                root.lowerChangeRequested(newValue)
            }
        }
    }
}
