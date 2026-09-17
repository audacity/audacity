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
    contentHeight: uiModel.popupHeight - 2 * uiModel.popupMargins

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
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: uiModel.itemsSpacing

        Row {
            Layout.preferredHeight: uiModel.btnHeight
            Layout.fillWidth: true

            spacing: uiModel.btnSpacing

            NavigationPanel {
                id: zoomNavPanel

                name: "TrackRulerZoom"
                section: root.navigationSection
                enabled: root.isOpened
                direction: NavigationPanel.Horizontal
                order: 1

                accessible.name: qsTrc("trackruler", "Zoom")
            }

            FlatButton {
                id: zoomInBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: uiModel.zoomBtnWidth

                normalColor: ui.theme.buttonColor
                icon: IconCode.ZOOM_IN

                enabled: !isMaxZoom

                navigation.name: "ZoomIn"
                navigation.panel: zoomNavPanel
                navigation.order: 1
                navigation.accessible.name: qsTrc("trackruler", "Zoom in")

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

                navigation.name: "ZoomOut"
                navigation.panel: zoomNavPanel
                navigation.order: 2
                navigation.accessible.name: qsTrc("trackruler", "Zoom out")

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

                navigation.name: "ZoomReset"
                navigation.panel: zoomNavPanel
                navigation.order: 3

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

            navPanel.name: "TrackRulerFormat"
            navPanel.section: root.navigationSection
            navPanel.order: 2
            navPanel.enabled: root.isOpened
            navPanel.accessible.name: title

            model: root.availableRulerTypes

            onValueChangeRequested: function (value) {
                root.rulerTypeChangeRequested(value)
            }
        }

        NavigationPanel {
            id: halfWaveNavPanel

            name: "TrackRulerHalfWave"
            section: root.navigationSection
            enabled: root.isOpened
            order: 3

            accessible.name: halfwave.text
        }

        CheckBox {
            id: halfwave

            text: qsTrc("trackruler", "Half wave")

            checked: root.isHalfWave

            navigation.name: "HalfWave"
            navigation.panel: halfWaveNavPanel
            navigation.order: 1

            onClicked: {
                root.toggleHalfWaveRequested()
            }
        }
    }
}
