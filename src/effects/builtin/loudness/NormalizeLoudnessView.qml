import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/loudness", "Normalize loudness")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: 400

    model: normalize

    NormalizeLoudnessViewModel {
        id: normalize

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        normalizeLoudness.init()
    }
}
