import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property var model: null

    property alias navigation: menuGroup.navigation

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16
        readonly property int spaceXXL: 24

        readonly property int tabHeight: 32
    }

    Rectangle {
        id: background
        anchors.fill: parent
        color: ui.theme.backgroundPrimaryColor
    }

    RadioButtonGroup {
        id: menuGroup

        anchors.fill: parent
        anchors.topMargin: prv.spaceM
        anchors.bottomMargin: prv.spaceM

        model: root.model ? root.model.categories : []

        clip: true
        orientation: ListView.Vertical
        spacing: 0

        delegate: PageTabButton {
            width: menuGroup.width
            height: prv.tabHeight

            orientation: Qt.Horizontal
            spacing: prv.spaceM
            leftPadding: prv.spaceL

            normalStateFont: ui.theme.bodyFont
            selectedStateFont: ui.theme.bodyBoldFont

            title: modelData
            checked: root.model ? (model.index === root.model.selectedCategoryIndex) : false

            navigation.panel: menuGroup.navigation
            navigation.row: model.index

            onToggled: {
                if (root.model) {
                    root.model.selectedCategoryIndex = model.index
                }
            }
        }
    }
}
