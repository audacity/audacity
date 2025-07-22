import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/noisereduction", "Amplify")
    property bool isApplyAllowed: noisereduction.isApplyAllowed

    width: 320
    implicitHeight: 500

    model: noisereduction

    NoiseReductionViewModel {
        id: noisereduction

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        noisereduction.init()
    }
}
