import QtQuick
import Muse.Ui
import Audacity.BuiltinEffects

import "../../../dynamics/timeline"

BuiltinEffectBase {
    property alias playState: playStateModel.playState

    readonly property color gridColor: Qt.rgba(ui.theme.strokeColor.r, ui.theme.strokeColor.g, ui.theme.strokeColor.b, 0.2)

    DynamicsPlayStateModel {
        id: playStateModel
    }

    Component.onCompleted: {
        playStateModel.init()
    }
}
