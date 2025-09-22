import QtQuick
import Muse.Ui
import Audacity.BuiltinEffects

import "../../../dynamics/timeline"

BuiltinEffectBase {
    property alias playState: playStateModel.playState

    readonly property color gridColor: "#33D4D5D9"

    DynamicsPlayStateModel {
        id: playStateModel
    }

    Component.onCompleted: {
        playStateModel.init()
    }
}
