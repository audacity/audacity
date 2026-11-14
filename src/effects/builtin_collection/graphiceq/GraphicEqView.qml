import QtQuick
import QtQuick.Controls
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/graphiceq", "Graphic EQ")
    property bool isApplyAllowed: true

    width: boardRectangle.width
    implicitHeight: boardRectangle.height

    builtinEffectModel: GraphicEqViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 2
    property alias graphicEq: root.builtinEffectModel
    property NavigationPanel slidersNavigationPanel: NavigationPanel {
        name: "GraphicEqSliders"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }
    property NavigationPanel flattenInvertNavigationPanel: NavigationPanel {
        name: "GraphicEqFlattenInvert"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    Rectangle {
        id: boardRectangle

        width: boardAndButtons.width
        height: boardAndButtons.height
        anchors.centerIn: parent

        radius: 8
        color: ui.theme.backgroundSecondaryColor
        border.color: ui.theme.strokeColor

        Column {
            id: boardAndButtons

            padding: 16
            spacing: 16
            width: board.width + 2 * padding

            GraphicEqBoard {
                id: board

                anchors.horizontalCenter: parent.horizontalCenter

                navigationPanel: root.slidersNavigationPanel
                navigationOrderStart: 0

                bandsModel: graphicEq.bandsModel
                minDbGain: graphicEq.minDbGain
                maxDbGain: graphicEq.maxDbGain
            }

            Row {
                id: buttons

                spacing: 8
                anchors.horizontalCenter: parent.horizontalCenter

                FlatButton {
                    id: resetButton

                    width: 64
                    height: 28

                    navigation.panel: root.flattenInvertNavigationPanel
                    navigation.order: 0

                    text: qsTrc("effects/graphiceq", "Reset")

                    onClicked: graphicEq.bandsModel.flatten()
                }

                FlatButton {
                    id: invertButton

                    width: 64
                    height: 28

                    navigation.panel: root.flattenInvertNavigationPanel
                    navigation.order: resetButton.navigation.order + 1

                    text: qsTrc("effects/graphiceq", "Invert")

                    onClicked: graphicEq.bandsModel.invert()
                }
            }
        }
    }
}
