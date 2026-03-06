import QtQuick

import Audacity.BuiltinEffects

Rectangle {
    id: root

    property var dialogView: null

    required property int instanceId
    required property BuiltinEffectModel builtinEffectModel
    property bool usesPresets: builtinEffectModel.usesPresets
    property bool isPreviewing: builtinEffectModel.isPreviewing
    property bool usedDestructively: true
    property int bottomButtonsNavigationPanelOrder: 3

    color: ui.theme.backgroundPrimaryColor

    function init() {
        root.builtinEffectModel.init()
    }

    function startPreview() {
        root.builtinEffectModel.startPreview()
    }

    function stopPreview() {
        root.builtinEffectModel.stopPreview()
    }
}
