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

    implicitWidth: prv.dialogWidth
    implicitHeight: {
        // why 5 times spaceXL?
        // -> 5 * spaceXL accounts for the combined vertical margins and spacing:
        // Calculate total height needed:
        // - Top margin: prv.spaceXL (1)
        // - Title height: titleLabel.height
        // - Spacing after title: prv.spaceXL (2)
        // - Flickable top margin: prv.spaceXL (3)
        // - Content: parametersColumn.height
        // - Flickable bottom margin: prv.spaceXL (4)
        // - Bottom margin: prv.spaceXL (5)
        // - Border: 2 * prv.borderWidth
        var totalHeight = prv.spaceXL * 5 + titleLabel.height + parametersColumn.height + 2 * prv.borderWidth
        // we automatically size the height to fit the content for plugins with few parameters
        // we limit the height to avoid making the dialog too tall
        return Math.min(totalHeight, prv.maxDialogHeight)
    }

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16
        readonly property int spaceXXL: 24

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4

        readonly property int dialogWidth: 640
        readonly property int maxDialogHeight: 640
        readonly property int maxContentWidth: 512
    }

    property var viewModel: GeneratedEffectViewerModelFactory.createModel(root, root.instanceId)

    Component.onCompleted: {
        viewModel.init()
    }

    ColumnLayout {
        id: mainLayout
        anchors.fill: parent
        anchors.margins: prv.spaceXL
        spacing: prv.spaceXL

        StyledTextLabel {
            id: titleLabel
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
                    width: Math.min(parent.width, prv.maxContentWidth)
                    anchors.horizontalCenter: parent.horizontalCenter
                    spacing: prv.spaceXXL

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

                            // Pass time-related properties for time controls
                            sampleRate: viewModel.sampleRate
                            tempo: viewModel.tempo
                            upperTimeSignature: viewModel.upperTimeSignature
                            lowerTimeSignature: viewModel.lowerTimeSignature

                            onGestureStarted: function (parameterId) {
                                viewModel.parametersModel.beginGesture(parameterId)
                            }

                            onGestureEnded: function (parameterId) {
                                viewModel.parametersModel.endGesture(parameterId)
                            }

                            onValueChanged: function (parameterId, value) {
                                viewModel.parametersModel.setParameterValue(parameterId, value)
                            }
                        }
                    }
                }
            }
        }
    }

    // Preview methods - delegate to viewModel
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
