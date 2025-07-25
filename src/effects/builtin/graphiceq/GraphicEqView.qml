import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/graphiceq", "Graphic EQ")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: 400

    model: graphicEq

    GraphicEqViewModel {
        id: graphicEq

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        graphicEq.init()
    }
}
