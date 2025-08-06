import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/clickremoval", "Click Removal")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: 400

    model: clickRemoval

    ClickRemovalViewModel {
        id: clickRemoval

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        clickRemoval.init()
    }
}
