/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.UiComponents
import Muse.Ui 1.0

import Audacity.UiComponents 1.0
import Audacity.Spectrogram 1.0

StyledPopupView {
    id: root

    required property SpectrogramChannelRulerModel rulerModel

    contentWidth: mainColumn.width
    contentHeight: mainColumn.height

    placementPolicies: PopupView.PreferLeft

    property alias settingsModel: settingsModel
    property alias minMaxNavigationPanel: minMaxNavigationPanel

    TrackSpectrogramSettingsModel {
        id: settingsModel
        trackId: root.rulerModel.trackId
    }

    QtObject {
        id: prv

        readonly property int zoomBtnWidth: 34
    }

    Column {
        id: mainColumn

        width: 175
        spacing: 12

        Row {
            width: parent.width
            height: 28

            spacing: 6

            NavigationPanel {
                id: zoomNavigationPanel
                section: root.navigationSection
                name: "ZoomNavigationPanel"
                order: 0
            }

            FlatButton {
                id: zoomInBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: prv.zoomBtnWidth

                navigation.panel: zoomNavigationPanel
                navigation.order: 0

                normalColor: ui.theme.buttonColor
                icon: IconCode.ZOOM_IN

                onClicked: {
                    rulerModel.zoomInFromPopup()
                }
            }

            FlatButton {
                id: zoomOutBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: prv.zoomBtnWidth

                navigation.panel: zoomNavigationPanel
                navigation.order: zoomInBtn.navigation.order + 1

                normalColor: ui.theme.buttonColor
                icon: IconCode.ZOOM_OUT

                enabled: !rulerModel.isMinZoom

                onClicked: {
                    rulerModel.zoomOutFromPopup()
                }
            }

            FlatButton {
                id: resetBtn

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: 96

                navigation.panel: zoomNavigationPanel
                navigation.order: zoomOutBtn.navigation.order + 1

                normalColor: ui.theme.buttonColor
                icon: IconCode.UNDO

                orientation: Qt.Horizontal

                text: qsTrc("spectrogram", "Reset")

                enabled: !rulerModel.isMinZoom

                onClicked: {
                    rulerModel.resetZoom()
                }
            }
        }

        StyledGroupBox {
            id: scaleGroupBox

            width: parent.width
            height: scaleGroupBox.implicitHeight

            title: qsTrc("spectrogram", "Scale")

            navPanel.section: root.navigationSection
            navPanel.order: 1
            navPanel.name: "ScaleGroupBox"

            titleSpacing: 8
            margin: 16
            itemSpacing: 16

            model: {
                const result = []
                for (var i = 0; i < settingsModel.scaleNames.length; i++) {
                    result.push({
                        label: settingsModel.scaleNames[i],
                        value: i
                    })
                }
                return result
            }
            value: settingsModel.scale

            onValueChangeRequested: function (newValue) {
                settingsModel.scale = newValue
            }
        }

        Column {
            width: parent.width
            spacing: 10

            StyledTextLabel {
                width: parent.width
                text: qsTrc("spectrogram", "Frequency range")
                horizontalAlignment: Text.AlignLeft
            }

            NavigationPanel {
                id: minMaxNavigationPanel
                section: root.navigationSection
                name: "MinMaxNavigationPanel"
                order: 2
            }

            Repeater {
                id: repeater

                model: ScaleSectionParameterListModel {
                    settingsModel: root.settingsModel
                    trackId: root.rulerModel.trackId
                }

                Row {
                    width: parent.width
                    height: control.implicitHeight

                    StyledTextLabel {
                        width: 32
                        anchors.verticalCenter: parent.verticalCenter

                        text: shortControlLabel
                        horizontalAlignment: Text.AlignLeft
                    }

                    IncrementalPropertyControl {
                        id: control

                        width: 90
                        anchors.verticalCenter: parent.verticalCenter

                        navigation.panel: root.minMaxNavigationPanel
                        navigation.order: index

                        minValue: controlMinValue
                        maxValue: controlMaxValue
                        measureUnitsSymbol: controlUnits
                        decimals: 0
                        step: 1

                        currentValue: controlCurrentValue
                        onValueEditingFinished: function (value) {
                            controlCurrentValue = value
                        }
                    }
                }
            }
        }
    }
}
