/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {
    id: root

    required property int instanceId

    implicitWidth: 480
    implicitHeight: 640

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4
    }

    property var viewModel: GeneratedEffectViewerModelFactory.createModel(root, root.instanceId)

    Component.onCompleted: {
        viewModel.init()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: prv.spaceXL
        spacing: prv.spaceXL

        StyledTextLabel {
            Layout.fillWidth: true
            text: viewModel.title
            font: ui.theme.headerBoldFont
            horizontalAlignment: Text.AlignHCenter
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: ui.theme.backgroundSecondaryColor
            border.color: ui.theme.strokeColor
            border.width: prv.borderWidth
            radius: prv.borderRadius

            StyledFlickable {
                anchors.fill: parent
                anchors.margins: prv.spaceXL
                contentHeight: parametersColumn.height

                ColumnLayout {
                    id: parametersColumn
                    width: parent.width
                    spacing: prv.spaceL

                    // Show message if no parameters
                    StyledTextLabel {
                        Layout.fillWidth: true
                        visible: !viewModel.hasParameters
                        text: viewModel.noParametersMessage
                        horizontalAlignment: Text.AlignHCenter
                        opacity: 0.6
                    }

                    // Parameter controls
                    Repeater {
                        model: viewModel.parametersModel

                        delegate: ParameterControl {
                            Layout.fillWidth: true
                            parameterData: model

                            onValueChanged: function (value) {
                                viewModel.parametersModel.setParameterValue(index, value)
                            }
                        }
                    }
                }
            }
        }
    }

    // Preview methods - delegate to viewModel
    function preview() {
        viewModel.startPreview()
    }

    function startPreview() {
        viewModel.startPreview()
    }

    function stopPreview() {
        viewModel.stopPreview()
    }

    property bool isApplyAllowed: true
    property bool usesPresets: true
    property bool isPreviewing: viewModel.isPreviewing
}
