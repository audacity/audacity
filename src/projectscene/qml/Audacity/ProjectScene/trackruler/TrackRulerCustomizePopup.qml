/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

StyledPopupView {
    id: root

    property bool isVerticalRulersVisible: true
    property int rulerType: 2

    signal hideRulersRequested()
    signal rulerTypeChangeRequested(int newType)

    contentWidth: uiModel.popupWidth - 2*uiModel.popupMargins
    contentHeight: uiModel.popupHeight - 2*uiModel.popupMargins

    margins: uiModel.popupMargins

    QtObject {
        id: uiModel

        readonly property int popupWidth: 200
        readonly property int popupHeight: 260

        readonly property int popupMargins: 12
        readonly property int itemsSpacing: 12
        readonly property int btnSpacing: 6

        readonly property int btnHeight: 28
        readonly property int zoomBtnWidth: 40
        readonly property int resetBtnWidth: 85
        readonly property int formatGroupBoxHeight: 120
        readonly property int showRulersBoxHeight: 40
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: uiModel.itemsSpacing

        Row {
            Layout.preferredHeight: uiModel.btnHeight
            Layout.fillWidth: true

            spacing: uiModel.btnSpacing

            FlatButton {
                id: zoomInBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: uiModel.zoomBtnWidth

                enabled: false

                normalColor: ui.theme.buttonColor
                icon: IconCode.ZOOM_IN
            }

            FlatButton {
                id: zoomOutBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: uiModel.zoomBtnWidth

                enabled: false

                normalColor: ui.theme.buttonColor
                icon: IconCode.ZOOM_OUT
            }

            FlatButton {
                id: resetBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: uiModel.resetBtnWidth

                enabled: false

                normalColor: ui.theme.buttonColor
                icon: IconCode.UNDO

                orientation: Qt.Horizontal

                text: qsTrc("trackruler", "Reset")
            }
        }

        StyledGroupBox {
            Layout.fillWidth: true
            Layout.preferredHeight: uiModel.formatGroupBoxHeight

            title: qsTrc("trackruler", "Ruler format")

            titleSpacing: 4

            backgroundColor: ui.theme.backgroundSecondaryColor

            value: root.rulerType

            model: [
                {label : qsTrc("trackruler","Logarithmic (dB)"), value: 0},
                {label : qsTrc("trackruler","Linear (dB)"), value: 1},
                {label : qsTrc("trackruler","Linear (amp)"), value: 2}
            ]

            onValueChangeRequested: function(value) {
                root.rulerTypeChangeRequested(value)
            }
        }

        CheckBox {
            id: halfwave

            enabled: false

            text: qsTrc("trackruler", "Half wave")

            checked: false

            onClicked: {
                console.log("Half wave toggled: " + checked)
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: uiModel.showRulersBoxHeight

            SeparatorLine {
                anchors.top: parent.top
                anchors.leftMargin: -uiModel.popupMargins
                anchors.rightMargin: -uiModel.popupMargins
            }

            CheckBox {
                id: showTrackRulers

                anchors.verticalCenter: parent.verticalCenter

                text: qsTrc("trackruler", "Show vertical rulers")

                checked: root.isVerticalRulersVisible

                onClicked: {
                    root.hideRulersRequested()
                }
            }
        }
    }

}
