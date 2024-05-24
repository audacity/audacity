import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene


StyledPopupView {
    id: root
    objectName: "AddNewTrackPanel"

    contentWidth: content.width
    contentHeight: content.height

    modal: true

    property alias popupAnchorItem: root.anchorItem

    signal createTracks(type : int, quantity: int)

    QtObject {
        id: prv
        property int type: TrackType.MONO
        property string numberOfTracksPrompt: numberOfTracksText
        readonly property string numberOfTracksText: qsTrc("projectscene", "Number of tracks")
        property int numberOfTracks: numberOfTracksValidator.bottom
        property bool validNumberOfTracks: true
    }

    ColumnLayout {
        id: content

        spacing: 12

        RadioButtonGroup {
            id: trackType

            width: parent.width
            height: 64

            readonly property int navigationRowStart: 0
            readonly property int navigationRowEnd: navigationRowStart + count

            model: [
                { iconCode: IconCode.MICROPHONE, text: qsTrc("projectscene", "Mono"), value: TrackType.MONO },
                { iconCode: IconCode.MICROPHONE, text: qsTrc("projectscene", "Stereo"), value: TrackType.STEREO },
                { iconCode: IconCode.LOOP_IN, text: qsTrc("projectscene", "Label"), value: TrackType.LABEL },
            ]

            delegate: FlatRadioButton {
                width: 97
                height: 64

                Column {
                    anchors.centerIn: parent
                    height: childrenRect.height
                    spacing: 8

                    StyledIconLabel {
                        anchors.horizontalCenter: parent.horizontalCenter
                        iconCode: modelData.iconCode
                    }

                    StyledTextLabel {
                        anchors.horizontalCenter: parent.horizontalCenter
                        text: modelData.text
                    }
                }

                checked: prv.type === modelData.value
                onToggled: {
                    prv.type = modelData.value
                }
            }
        }

        StyledTextLabel {
            id: numberOfTracksLabel

            Layout.alignment: Qt.AlignHCenter
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter

            text: prv.numberOfTracksText
            font.family: ui.theme.bodyFont.family
        }

        RowLayout {

            Layout.alignment: Qt.AlignHCenter

            FlatButton {
                id: subtract

                Layout.preferredWidth: 28
                Layout.preferredHeight: 28

                text: "-"

                onClicked: {
                    if (prv.numberOfTracks === "") {
                        input.focus = false
                        return
                    }

                    var value = Number(prv.numberOfTracks)
                    if (value > numberOfTracksValidator.top) {
                        prv.numberOfTracks = numberOfTracksValidator.top
                    } else if (value > numberOfTracksValidator.bottom) {
                        prv.numberOfTracks = value - 1
                    }

                    input.focus = false
                }
            }

            TextInputField {
                id: input

                Layout.preferredWidth: 79
                Layout.preferredHeight: 28

                clip: true

                currentText: prv.numberOfTracks
                textHorizontalAlignment: Qt.AlignHCenter
                textVerticalAlignment: Qt.AlignVCenter

                validator: IntValidator {
                    id: numberOfTracksValidator
                    bottom: 1
                    top: 25
                }

                onTextEditingFinished: function(newTextValue) {
                    var value = Number(newTextValue)
                    prv.numberOfTracks = value
                }
            }

            FlatButton {
                id: add

                Layout.preferredWidth: 28
                Layout.preferredHeight: 28

                text: "+"

                onClicked: {
                    if (prv.numberOfTracks === "") {
                        input.focus = false
                        return
                    }

                    var value = Number(prv.numberOfTracks)
                    if (value < numberOfTracksValidator.bottom) {
                        prv.numberOfTracks = numberOfTracksValidator.bottom
                    } else if (value < numberOfTracksValidator.top) {
                        prv.numberOfTracks = value + 1
                    }

                    input.focus = false
                }
            }
        }

        SeparatorLine {}

        FlatButton {
            id: create

            height: 28
            Layout.fillWidth: true

            text: qsTrc("projectscene", "Create")
            accentButton: true
            enabled: prv.validNumberOfTracks

            onClicked: {
                input.focus = false
                root.createTracks(prv.type, Number(prv.numberOfTracks))
            }
        }
    }
}
