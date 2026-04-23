import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/slidingstretch", "Sliding stretch")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: 300

    builtinEffectModel: SlidingStretchViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 1
    property alias slidingStretch: root.builtinEffectModel
    property NavigationPanel normalizeNavigationPanel: NavigationPanel {
        name: "SlidingStretchControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Rectangle {
        anchors.fill: parent
        color: "transparent"
        border.color: "transparent"
        border.width: 0
    }
}
