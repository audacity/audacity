/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Preferences

StyledDialogView {
    id: root

    property var availableRates: []
    property int rate: 44100
    property string title: ""

    QtObject {
        id: prv

        readonly property string measureUnitsSymbol: qsTrc("global", "Hz")
    }

    contentWidth: 280
    contentHeight: 80

    margins: 16

    Row {
        id: rateRow

        anchors {
            left: parent.left
            right: parent.right
            top: parent.top
        }

        spacing: 16
        
        StyledTextLabel {
            id: label

            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("trackedit/rate", "New sample rate (Hz):")
        }

        Item {
            id: dropdown
            width: parent.width *.5
            height: 30

            TextInputField {
                id: inputItem

                anchors.fill: parent

                currentText: root.rate > 0 ? root.rate.toString() : ""

                validator: IntValidator { bottom: 1 }

                onTextChanged: function(newValue) {
                    var val = parseInt(newValue)
                    root.rate = (val > 0) ? val : 1
                }

            }

            StyledIconLabel {
                id: dropIconItem
                anchors.verticalCenter: parent.verticalCenter
                anchors.right: parent.right
                anchors.rightMargin: 8

                iconCode: IconCode.SMALL_ARROW_DOWN

                visible: root.availableRates && root.availableRates.length > 0

                MouseArea {
                    id: mouseAreaItem
                    anchors.fill: parent

                    onClicked: {
                        menuLoader.toggleOpened(availableRates.map(function(rate) {
                            return {"title": rate.toString(), "id": rate.toString()}
                        }))
                    }

                    onPressed: {
                        ui.tooltip.hide(dropdown, true)
                    }

                    onContainsMouseChanged: {
                        if (!inputItem.truncated || menuLoader.isMenuOpened) {
                            return
                        }

                        if (mouseAreaItem.containsMouse) {
                            ui.tooltip.show(dropdown, inputItem.text)
                        } else {
                            ui.tooltip.hide(dropdown)
                        }
                    }
                }
            }

            StyledMenuLoader {
                id: menuLoader
                width: parent.width

                onHandleMenuItem: function(itemId) {
                    if (itemId === null) {
                        return
                    }

                    inputItem.currentText = itemId
                }
            }
        }

    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok]

        onStandardButtonClicked: function(buttonId) {
            switch (buttonId) {
            case ButtonBoxModel.Cancel:
                root.reject()
                return
            case ButtonBoxModel.Ok:
                root.ret = {
                    errcode: 0,
                    value: root.rate
                }
                root.hide()
                return

            }
        }
    }
}
