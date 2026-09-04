/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents
import Muse.Ui 1.0

import Audacity.UiComponents 1.0

StyledPopupView {
    id: root

    required property int rulerType
    required property var availableRulerTypes

    required property bool isDefaultZoom
    required property bool isMaxZoom
    required property bool isMinZoom
    required property bool isHalfWave

    signal rulerTypeChangeRequested(int newType)
    signal zoomInRequested
    signal zoomOutRequested
    signal zoomResetRequested
    signal toggleHalfWaveRequested

    contentWidth: uiModel.popupWidth - 2 * uiModel.popupMargins
    contentHeight: -1  // dynamic implicit height, enables scrolling if needed

    margins: uiModel.popupMargins

    QtObject {
        id: uiModel

        readonly property int popupWidth: 200
        readonly property real popupHeight: 350  // increased to accommodate scrolling area

        readonly property int popupMargins: 12
        readonly property int itemsSpacing: 12
        readonly property int btnSpacing: 6

        readonly property int btnHeight: 28
        readonly property int zoomBtnWidth: 40
        readonly property int resetBtnWidth: 85
        readonly property int formatGroupBoxHeight: 120
    }

    ScrollView {
        anchors.fill: parent
        ScrollBar.vertical.policy: ScrollBar.AsNeeded
        ScrollBar.horizontal.policy: ScrollBar.AlwaysOff
        clip: true

        ColumnLayout {
            anchors.left: parent.left
            anchors.right: parent.right
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

                    normalColor: ui.theme.buttonColor
                    icon: IconCode.ZOOM_IN

                    enabled: !isMaxZoom

                    onClicked: {
                        root.zoomInRequested()
                    }
                }

                FlatButton {
                    id: zoomOutBtn

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    width: uiModel.zoomBtnWidth

                    normalColor: ui.theme.buttonColor
                    icon: IconCode.ZOOM_OUT

                    enabled: !isMinZoom

                    onClicked: {
                        root.zoomOutRequested()
                    }
                }

                FlatButton {
                    id: resetBtn

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    width: uiModel.resetBtnWidth

                    normalColor: ui.theme.buttonColor
                    icon: IconCode.UNDO

                    orientation: Qt.Horizontal

                    text: qsTrc("trackruler", "Reset")

                    enabled: !isDefaultZoom

                    onClicked: {
                        root.zoomResetRequested()
                    }
                }
            }

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.preferredHeight: uiModel.formatGroupBoxHeight

                title: qsTrc("trackruler", "Ruler format")

                titleSpacing: 4

                value: root.rulerType

                model: root.availableRulerTypes

                onValueChangeRequested: function (value) {
                    root.rulerTypeChangeRequested(value)
                }
            }

            CheckBox {
                id: halfwave

                text: qsTrc("trackruler", "Half wave")

                checked: root.isHalfWave

                onClicked: {
                    root.toggleHalfWaveRequested()
                }
            }
        }
    }
}

