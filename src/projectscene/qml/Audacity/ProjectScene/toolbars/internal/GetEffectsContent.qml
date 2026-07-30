import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property var model: null

    property NavigationPanel navigation: NavigationPanel {
        name: "GetEffectsContentPanel"
        enabled: root.enabled && root.visible

        accessible.name: qsTrc("projectscene", "Effects")
    }

    QtObject {
        id: prv

        readonly property int spaceL: 12
        readonly property int spaceXL: 16

        readonly property int errorTextWidth: 400
    }

    color: ui.theme.backgroundSecondaryColor

    Connections {
        target: root.model

        function onSelectedCategoryIndexChanged() {
            flickable.contentY = -prv.spaceXL
        }
    }

    // Loading
    ColumnLayout {
        anchors.centerIn: parent
        visible: root.model.isLoading
        spacing: prv.spaceL

        StyledBusyIndicator {
            Layout.alignment: Qt.AlignHCenter
            running: root.model.isLoading
        }

        StyledTextLabel {
            Layout.alignment: Qt.AlignHCenter
            text: qsTrc("projectscene", "Please wait…")
            font: ui.theme.largeBodyFont
        }
    }

    // Error
    ColumnLayout {
        id: errorLayout

        anchors.centerIn: parent
        visible: root.model.hasError && !root.model.isLoading
        spacing: prv.spaceXL

        StyledTextLabel {
            id: errorLabel
            Layout.alignment: Qt.AlignHCenter
            text: qsTrc("projectscene", "Connection error")
            font: ui.theme.largeBodyBoldFont
        }

        StyledTextLabel {
            id: errorDescriptionLabel
            Layout.alignment: Qt.AlignHCenter
            Layout.preferredWidth: prv.errorTextWidth
            text: qsTrc("projectscene", "Audacity is unable to connect to MuseHub.com. Please check your connection and try again.")
            wrapMode: Text.Wrap
            horizontalAlignment: Text.AlignHCenter
        }

        FlatButton {
            Layout.alignment: Qt.AlignHCenter
            text: qsTrc("projectscene", "Try again")

            navigation.name: "TryAgain"
            navigation.panel: root.navigation
            navigation.column: 0
            navigation.enabled: errorLayout.visible
            navigation.accessible.name: errorLabel.text + ". " + errorDescriptionLabel.text + ". " + text

            onClicked: root.model.load()
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
        visible: !root.model.isLoading && !root.model.hasError
        contentHeight: effectsColumn.height
        clip: true

        Column {
            id: effectsColumn
            width: parent.width
            spacing: prv.spaceXL

            Repeater {
                model: root.model.effectsGroups

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
        visible: flickable.visible
    }

    Component {
        id: effectsGroupDelegate

        Column {
            width: effectsColumn.width

            required property int index
            required property var modelData

            visible: index === root.model.selectedCategoryIndex
            spacing: visible ? prv.spaceXL : 0
            height: visible ? implicitHeight : 0

            Flow {
                width: parent.width
                spacing: prv.spaceXL

                Repeater {
                    model: modelData.effects

                    EffectCard {
                        id: card

                        required property var modelData
                        required property int index

                        iconUrl: modelData.iconUrl
                        title: modelData.title
                        subtitle: modelData.subtitle
                        effectCode: modelData.code

                        navigation.panel: root.navigation
                        navigation.column: index
                        navigation.enabled: card.visible && card.enabled

                        navigation.onActiveChanged: {
                            if (card.navigation.active) {
                                const cardPos = card.mapToItem(effectsColumn, 0, 0)
                                const cardRect = Qt.rect(cardPos.x, cardPos.y, card.width, card.height)

                                Utils.ensureContentVisible(flickable, cardRect, prv.spaceXL)
                            }
                        }

                        onGetEffectClicked: function (code) {
                            root.model.openEffectUrl(code)
                        }
                    }
                }
            }
        }
    }
}
