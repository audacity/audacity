/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {
    id: root

    required property int instanceId

    implicitWidth: prv.dialogWidth
    implicitHeight: {
        // why 2 times spaceXL? and 2 times spaceM?
        // -> 2 * spaceXL and 2 * spaceM account for the combined vertical margins and spacing:
        // Calculate total height needed:
        // - Flickable top margin: prv.spaceXL (1)
        // - Flickable inner margin: prv.spaceM (1')
        // - Content: parametersColumn.height
        // - Flickable inner margin: prv.spaceM (2')
        // - Flickable bottom margin: prv.spaceXL (2)
        // - Border: 2 * prv.borderWidth
        var totalHeight = prv.spaceXL * 2 + prv.spaceM * 2 + parametersColumn.height + 2 * prv.borderWidth
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
        readonly property int maxDialogHeight: 340 // real ~444px with system bar on Mac
        readonly property int maxContentWidth: 580
    }

    property var viewModel: GeneratedEffectViewerModelFactory.createModel(root, root.instanceId)

    Component.onCompleted: {
        viewModel.init()
    }

    ColumnLayout {
        id: mainLayout
        anchors.fill: parent
        spacing: prv.spaceXL

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: ui.theme.backgroundSecondaryColor
            border.color: ui.theme.strokeColor
            border.width: prv.borderWidth
            radius: prv.borderRadius

            StyledFlickable {
                id: flickable

                anchors.fill: parent
                anchors.margins: prv.spaceM
                anchors.topMargin: prv.spaceXXL
                anchors.bottomMargin: prv.spaceXXL
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

                            onStringValueChanged: function (parameterId, stringValue) {
                                viewModel.parametersModel.setParameterStringValue(parameterId, stringValue)
                            }
                        }
                    }
                }

                ScrollBar.vertical: scrollBar
                ScrollBar.horizontal: null
            }

            StyledScrollBar {
                id: scrollBar
                anchors.top: parent.top
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.margins: 0
                policy: ScrollBar.AlwaysOn
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
    property bool isPreviewAllowed: viewModel.isPreviewAllowed
    property bool usesPresets: true
    property bool isPreviewing: viewModel.isPreviewing
}
