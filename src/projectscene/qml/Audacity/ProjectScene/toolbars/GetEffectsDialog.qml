import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

StyledDialogView {
    id: root

    title: qsTrc("projectscene", "Get effects")

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16
        readonly property int spaceXXL: 24

        readonly property int menuWidth: 224
        readonly property int errorTextWidth: 400

        readonly property int contentWidth: 880
        readonly property int contentHeight: 692 // 720 (figma) - 28 (figma window header)

        readonly property int sideMargin: 16
    }

    contentWidth: prv.contentWidth
    contentHeight: prv.contentHeight

    modal: true
    resizable: false

    GetEffectsModel {
        id: effectsModel

        onSelectedCategoryIndexChanged: {
            flickable.contentY = -prv.spaceXL
        }
    }

    Component.onCompleted: {
        effectsModel.load()
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true

            spacing: 0

            GetEffectsMenu {
                id: menu

                Layout.fillHeight: true
                Layout.preferredWidth: prv.menuWidth - 1 // -1 for vertical separator line

                navigation.section: root.navigationSection
                navigation.order: 1

                model: effectsModel
            }

            SeparatorLine {
                orientation: Qt.Vertical
            }

            // Main content
            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: true
                color: ui.theme.backgroundSecondaryColor

                // Loading
                ColumnLayout {
                    anchors.centerIn: parent
                    visible: effectsModel.isLoading
                    spacing: prv.spaceL

                    StyledBusyIndicator {
                        Layout.alignment: Qt.AlignHCenter
                        running: effectsModel.isLoading
                    }

                    StyledTextLabel {
                        Layout.alignment: Qt.AlignHCenter
                        text: qsTrc("projectscene", "Please wait…")
                        font: ui.theme.largeBodyFont
                    }
                }

                // Error
                ColumnLayout {
                    anchors.centerIn: parent
                    visible: effectsModel.hasError && !effectsModel.isLoading
                    spacing: prv.spaceXL

                    StyledTextLabel {
                        Layout.alignment: Qt.AlignHCenter
                        text: qsTrc("projectscene", "Connection error")
                        font: ui.theme.largeBodyBoldFont
                    }

                    StyledTextLabel {
                        Layout.alignment: Qt.AlignHCenter
                        Layout.preferredWidth: prv.errorTextWidth
                        text: qsTrc("projectscene", "Audacity is unable to connect to MuseHub.com. Please check your connection and try again.")
                        wrapMode: Text.Wrap
                        horizontalAlignment: Text.AlignHCenter
                    }

                    FlatButton {
                        Layout.alignment: Qt.AlignHCenter
                        text: qsTrc("projectscene", "Try again")
                        onClicked: effectsModel.load()
                    }
                }

                // Effects grid
                StyledFlickable {
                    id: flickable

                    anchors.fill: parent
                    leftMargin: prv.spaceXL
                    rightMargin: prv.spaceXL
                    topMargin: prv.spaceXL
                    bottomMargin: prv.spaceXL
                    visible: !effectsModel.isLoading && !effectsModel.hasError
                    contentHeight: effectsColumn.height
                    clip: true

                    Column {
                        id: effectsColumn
                        width: parent.width
                        spacing: prv.spaceXL

                        Repeater {
                            model: effectsModel.effectsGroups

                            delegate: effectsGroupDelegate
                        }
                    }

                    ScrollBar.vertical: scrollBar
                    ScrollBar.horizontal: null
                }

                StyledScrollBar {
                    id: scrollBar
                    anchors.top: flickable.top
                    anchors.right: flickable.right
                    anchors.bottom: flickable.bottom

                    policy: ScrollBar.AlwaysOn
                }
            }
        }

        SeparatorLine {}

        // Bottom bar
        RowLayout {
            Layout.fillWidth: true
            Layout.margins: prv.spaceL

            FlatButton {
                text: qsTrc("projectscene", "Become a partner")
                onClicked: effectsModel.openBecomeAPartnerUrl()
            }

            Item {
                Layout.fillWidth: true
            }

            ButtonBox {
                buttons: [ButtonBoxModel.Done]
                navigationPanel.section: root.navigationSection
                navigationPanel.order: 1
                onStandardButtonClicked: function (buttonId) {
                    if (buttonId === ButtonBoxModel.Done)
                        root.accept()
                }
            }
        }
    }

    Component {
        id: effectsGroupDelegate

        Column {
            width: effectsColumn.width

            required property int index
            required property var modelData

            visible: index === effectsModel.selectedCategoryIndex
            spacing: visible ? prv.spaceXL : 0
            height: visible ? implicitHeight : 0

            Flow {
                width: parent.width
                spacing: prv.spaceXL

                Repeater {
                    model: modelData.effects

                    EffectCard {
                        required property var modelData

                        iconUrl: modelData.iconUrl
                        title: modelData.title
                        subtitle: modelData.subtitle
                        effectCode: modelData.code

                        onGetEffectClicked: function (code) {
                            effectsModel.openEffectUrl(code)
                        }
                    }
                }
            }
        }
    }
}
