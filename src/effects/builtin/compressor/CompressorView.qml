import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/compressor", "Compressor")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: 400

    model: compressor

    CompressorViewModel {
        id: compressor

        instanceId: root.instanceId
    }

    Component.onCompleted: {
        compressor.init()
    }
}
