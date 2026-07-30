import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

import "internal"

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

        readonly property int contentWidth: 880
        readonly property int contentHeight: 692 // 720 (figma) - 28 (figma window header)

        readonly property int sideMargin: 16
    }

    contentWidth: prv.contentWidth
    contentHeight: prv.contentHeight

    modal: true
    resizable: false

    onNavigationActivateRequested: {
        root.focusOnFirstControl()
    }

    GetEffectsModel {
        id: effectsModel

        onCategoriesChanged: {
            root.focusOnFirstControl()
        }
    }

    Component.onCompleted: {
        effectsModel.load()
    }

    function focusOnFirstControl() {
        if (effectsModel.hasError) {
            content.navigation.requestActive()
            return
        }

        menu.navigation.requestActive()
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

            GetEffectsContent {
                id: content

                Layout.fillWidth: true
                Layout.fillHeight: true

                navigation.section: root.navigationSection
                navigation.order: menu.navigation.order + 1

                model: effectsModel
            }
        }

        SeparatorLine {}

        ButtonBox {
            Layout.fillWidth: true
            Layout.margins: prv.spaceL

            buttons: [ButtonBoxModel.Done]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: content.navigation.order + 1

            onStandardButtonClicked: function (buttonId) {
                if (buttonId === ButtonBoxModel.Done) {
                    root.accept()
                }
            }

            FlatButton {
                text: qsTrc("projectscene", "Become a partner")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton
                isLeftSide: true

                onClicked: {
                    effectsModel.openBecomeAPartnerUrl()
                }
            }
        }
    }
}
